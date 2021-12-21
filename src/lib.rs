#![feature(associated_type_defaults)]
#![feature(never_type)]

use std::{
    collections::{HashMap, VecDeque, TryReserveError},
    convert::TryInto,
    error::Error,
    fmt::{self, Debug, Display, Formatter},
    fs::File,
    io::Write,
    marker::PhantomData,
};

use num_traits::ToPrimitive;
pub use structr_derive::Parse;
use thiserror::Error;

pub mod prelude {
    pub use std::{fmt, iter};

    pub use crate::{Parse, ParseError, ParseErrorKind, Parser};
}

pub trait Parse<'p>: Sized + Debug + ToOwned {
    fn parse<'a>(parser: &'a mut Parser<'p>) -> Result<Self, ParseError<'p>>
    where 'p: 'a;
}

pub enum Endianness {
    Little,
    Big,
}

impl Default for Endianness {
    fn default() -> Self {
        if i16::from_le(1) == 1 {
            Endianness::Little
        } else {
            Endianness::Big
        }
    }
}

#[derive(Debug, Clone)]
pub enum ContextEntry {
    Index(usize),
    StructStart(&'static str),
    StructEnd,
    FieldStart(&'static str, &'static str),
    Value(String),
    FieldEnd,
}

pub struct Parser<'p> {
    bytes: &'p [u8],
    pub i: usize,
    pub endianness: Endianness,
    pub context: Vec<ContextEntry>,
}

#[derive(Debug)]
pub struct ParseError<'p> {
    pub kind: ParseErrorKind,
    phantom: PhantomData<&'p ()>,
}

impl<'p> std::error::Error for ParseError<'p> {}

impl<'p> std::fmt::Display for ParseError<'p> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self, f)
    }
}

#[derive(Debug, Error)]
pub enum ParseErrorKind {
    #[error("{0} more bytes needed")]
    MoreBytesNeeded(usize),
    #[error("Out of bytes")]
    OutOfBytes,
    #[error("String had invalid UTF-8")]
    NotUTF8,
    #[error("Vector maxed out ({actual} > {max})")]
    VecMaxed { max: usize, actual: usize },
    #[error("usize too small")]
    UnsignedPointerSizedIntegerTooSmall,
    #[error("No integer-represented variant for integer {0}")]
    NoReprIntMatch(u64),
    #[error("{:?} != {:?}", 0, 1)]
    NotEqual(String, String),
    #[error("bool byte was {0}")]
    InvalidBool(u8),
    #[error("failed to reserve space in memory: {:?}", 0)]
    TryReserve(#[from] TryReserveError),
}
use ParseErrorKind::MoreBytesNeeded;

impl<'p> Parser<'p> {
    #[must_use]
    pub fn new(bytes: &'p [u8]) -> Self {
        Parser {
            bytes,
            i: 0,
            endianness: Default::default(),
            context: Vec::new(),
        }
    }

    #[allow(clippy::missing_panics_doc)]
    pub fn take<'a, const N: usize>(&'a mut self) -> Result<&'p [u8; N], ParseError<'p>>
    where 'p: 'a {
        let bytes = self
            .bytes
            .get(self.i..self.i + N)
            .ok_or_else(|| Parser::error(MoreBytesNeeded(self.i + N - self.bytes.len())))?;
        self.i += N;
        Ok(bytes.try_into().unwrap())
    }

    pub fn take_dynamically<'q>(&'q mut self, n: usize) -> Result<&'p [u8], ParseError<'p>>
    where 'p: 'q {
        let bytes = self
            .bytes
            .get(self.i..self.i + n)
            .ok_or_else(|| Parser::error(MoreBytesNeeded(self.i + n - self.bytes.len())))?;
        self.i += n;
        Ok(bytes)
    }

    pub fn parse<'a, T: Parse<'p>>(&'a mut self) -> Result<T, ParseError<'p>>
    where 'p: 'a {
        T::parse(self)
    }

    pub fn parse_n<'a, N: ToPrimitive + Copy, T: Parse<'p>>(
        &'a mut self,
        number_of_times: N,
    ) -> Result<Vec<T>, ParseError<'p>>
    where
        <T as ToOwned>::Owned: Clone,
        'p: 'a,
    {
        let n = match number_of_times.to_usize() {
            Some(ok) => ok,
            None => {
                return Err(Parser::error(
                    ParseErrorKind::UnsignedPointerSizedIntegerTooSmall,
                ))
            }
        };
        let mut vec = try_vec_with_capacity(n)?;
        for _ in 0..n {
            vec.push(T::parse(self)?);
        }
        Ok(vec)
    }

    pub fn parse_n_then_n_of<'a, N: ToPrimitive + Parse<'p> + Copy, T: Parse<'p>>(
        &'a mut self,
    ) -> Result<Vec<T>, ParseError<'p>>
    where
        <N as ToOwned>::Owned: Clone,
        <T as ToOwned>::Owned: Clone,
        'p: 'a,
    {
        let n = self.parse::<N>()?;
        self.parse_n(n)
    }

    pub fn ctx_index(&mut self) {
        self.context.push(ContextEntry::Index(self.i));
    }

    pub fn ctx_value(&mut self, string: String) {
        self.context.push(ContextEntry::Value(string));
    }

    pub fn ctx_field_start(&mut self, name: &'static str, type_: &'static str) {
        self.context.push(ContextEntry::FieldStart(name, type_));
    }

    pub fn ctx_struct_start(&mut self, name: &'static str) {
        self.context.push(ContextEntry::StructStart(name));
    }

    pub fn ctx_field_end(&mut self) {
        self.context.push(ContextEntry::FieldEnd);
    }

    pub fn ctx_struct_end(&mut self) {
        self.context.push(ContextEntry::StructEnd);
    }

    #[must_use]
    pub fn error(kind: ParseErrorKind) -> ParseError<'p> {
        ParseError {
            kind,
            phantom: PhantomData,
        }
    }
}

macro_rules! primitive_parse {
    ($($ty:ty) *) => {
        $(
            impl<'p> Parse<'p> for $ty {
                fn parse<'a>(parser: &'a mut Parser<'p>) -> Result<Self, ParseError<'p>> where 'p: 'a {
                    let value = match parser.endianness {
                        Endianness::Little => <$ty>::from_le_bytes(*parser.take()?),
                        Endianness::Big => <$ty>::from_be_bytes(*parser.take()?)
                    };
                    parser.ctx_value(format!("{}", value));
                    Ok(value)
                }
            }
        )*
    };
}
primitive_parse! {i8 i16 i32 i64 u8 u16 u32 u64 f32 f64}

impl<'p> Parse<'p> for bool {
    fn parse<'a>(parser: &'a mut Parser<'p>) -> Result<Self, ParseError<'p>>
    where 'p: 'a {
        Ok(match parser.take::<1>()?[0] {
            0 => false,
            1 => true,
            other => return Err(Parser::error(ParseErrorKind::InvalidBool(other))),
        })
    }
}

fn try_vec_with_capacity<'p, T>(size: usize) -> Result<Vec<T>, ParseError<'p>> {
    let mut vec = Vec::new();
    vec.try_reserve_exact(size).map_err(|err| Parser::error(err.into()))?;
    Ok(vec)
}

impl<'p, T: Parse<'p> + Clone, const N: usize> Parse<'p> for [T; N]
where T: ToOwned<Owned = T>
{
    fn parse<'a>(parser: &'a mut Parser<'p>) -> Result<Self, ParseError<'p>>
    where 'p: 'a {
        let mut out = try_vec_with_capacity(N)?;
        for _i in 0..N {
            let what = {
                let value = T::parse(parser)?.to_owned();
                value
            };
            out.push(what)
        }
        Ok(out.try_into().unwrap())
    }
}

impl<'p, const N: usize> Parse<'p> for &'p [u8; N] {
    fn parse<'a>(parser: &mut Parser<'p>) -> Result<Self, ParseError<'p>>
    where 'p: 'a {
        parser.take()
    }
}

impl<'p> Parse<'p> for &'p str {
    fn parse<'a>(parser: &mut Parser<'p>) -> Result<Self, ParseError<'p>>
    where 'p: 'a {
        let bytes = &parser.bytes[parser.i..];
        let mut offset = 0;
        let offset = loop {
            if bytes[offset] == 0 {
                break offset;
            }
            offset += 1;
        };
        let string: &str = std::str::from_utf8(&bytes[..offset])
            .map_err(|_| Parser::error(ParseErrorKind::NotUTF8))?;
        parser.i += offset + 1;
        parser.ctx_value(string.to_string());
        Ok(string)
    }
}

/// # Panics
/// If the context entries are invalid, like if a struct is ended before one
/// starts.
pub fn write_debug_json(context_entries: &[ContextEntry]) -> Result<(), Box<dyn Error>> {
    let mut index = 0;
    let mut file = File::create("debug.json")?;
    let mut s = String::new();
    let mut path = VecDeque::new();
    let mut struct_path = VecDeque::new();
    let mut struct_fields =
        HashMap::<&'static str, (bool, Vec<(&'static str, &'static str)>)>::new();
    s += "{\"data\": [\"\"";
    for entry in context_entries.iter() {
        match entry {
            ContextEntry::Index(new_index) => {
                index = *new_index;
            }
            ContextEntry::StructStart(name) => {
                s += &format!(", [\"s\", {:?}", name);
                struct_path.push_back(name);
                path.push_back(name);
            }
            ContextEntry::StructEnd => {
                struct_fields
                    .get_mut(*struct_path.back().unwrap())
                    .expect("ending field not started")
                    .0 = true;
                struct_path.pop_back();
                path.pop_back();
                s += "]";
            }
            ContextEntry::FieldStart(name, type_) => {
                s += ", [";
                s += &index.to_string();
                let (done, fields) = struct_fields
                    .entry(struct_path.back().unwrap())
                    .or_default();
                if !*done {
                    fields.push((name, type_));
                }
                path.push_back(name);
            }
            ContextEntry::FieldEnd => {
                path.pop_back();
                s += "]";
            }
            ContextEntry::Value(string) => {
                s += &format!(
                    ", {}",
                    serde_json::Value::from(<String as AsRef<str>>::as_ref(string))
                );
            }
        }
    }
    for _ in path.into_iter().rev() {
        s += "]";
    }
    s += &format!(
        "], \"structs\": {}}}",
        serde_json::to_string(&struct_fields).unwrap()
    );
    file.write_all(s.as_bytes())?;
    Ok(())
}

pub enum PathElement {
    Name(&'static str),
    Index(usize),
}

impl Display for PathElement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            PathElement::Index(index) => write!(f, "{}", index),
            PathElement::Name(name) => write!(f, "{}", name),
        }
    }
}

/// # Panics
/// If more structs and fields are ended than started
#[must_use]
pub fn get_end_path(context_entries: &[ContextEntry]) -> VecDeque<PathElement> {
    let mut path = VecDeque::new();
    path.push_back(PathElement::Index(0));
    let mut in_entity_kind_field = false;
    for entry in context_entries.iter() {
        match *entry {
            ContextEntry::StructStart(name) => {
                if in_entity_kind_field {
                    println!("{}", name);
                    in_entity_kind_field = false;
                }
                match path.back_mut().unwrap() {
                    PathElement::Index(index) => {
                        *index += 1;
                    }
                    PathElement::Name(_) => unreachable!("maybe"),
                }
                path.push_back(PathElement::Name(name))
            }
            ContextEntry::StructEnd => {
                path.pop_back();
            }
            ContextEntry::FieldStart(name, type_) => {
                in_entity_kind_field = type_.starts_with("EntityKind");
                path.push_back(PathElement::Name(name));
                path.push_back(PathElement::Index(0));
            }
            ContextEntry::FieldEnd => {
                path.pop_back();
                path.pop_back();
            }
            ContextEntry::Value(_) => match path.back_mut().unwrap() {
                PathElement::Index(index) => {
                    *index += 1;
                }
                PathElement::Name(_) => unreachable!("maybe"),
            },
            ContextEntry::Index(_) => {}
        }
    }
    path
}
