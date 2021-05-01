use structr::{get_end_path, prelude::*, write_debug_json, Endianness};

#[derive(Debug, Clone, Parse)]
struct What {
    float: f32,
    int: i32,
    // #[structr(with = "u8")]
    // with: u16,
    #[structr(run = "println!(\"hi\");")]
    hmm: u16,
    #[structr(len = "u32")]
    len_then_bytes: Vec<u8>,
    #[structr(eq = "[1]")]
    one: [u8; 1],
}

#[test]
fn test() {
    let mut parser = Parser::new(include_bytes!("bytes"));
    parser.endianness = Endianness::Little;
    match parser.parse::<What>() {
        Ok(what) => {
            #[allow(clippy::float_cmp)]
            assert_eq!(what.len_then_bytes.len(), 3)
        }
        Err(err) => {
            write_debug_json(&parser.context).unwrap();
            let end_path = get_end_path(&parser.context);
            for element in end_path {
                print!(".{}", element);
            }
            println!();
            panic!("{:#?}", err)
        }
    }
    write_debug_json(&parser.context).unwrap();
}
