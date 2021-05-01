var structs
var struct_data_map = new WeakMap()

    ; (async () => {
        json = await (await fetch("/debug.json")).json();
        const data_root = json.data[1]
        structs = json.structs
        add_struct(data_root, document.body, true)
    })()

function add_struct(data, parent, with_fields) {
    const eStruct = document.createElement("div")
    eStruct.classList.add("struct")
    const eStructName = document.createElement("div")
    eStructName.classList.add("struct__name")
    eStructName.textContent = data[1]
    eStruct.appendChild(eStructName)
    let def = structs[data[1]][1]
    if (with_fields) {
        add_fields(eStruct, data, def)
    }
    parent.appendChild(eStruct)
    return eStruct
}

function add_fields(eStruct, data, def) {
    const eStructFields = document.createElement("ol")
    eStruct.appendChild(eStructFields)
    eStructFields.classList.add("struct__fields")
    eStruct.classList.add("struct--expanded")
    const fields = data.slice(2)
    for (let i = 0; i < fields.length; i++) {
        const eField = document.createElement("div")
        eField.classList.add("field")
        let field_def = def[i]
        const [field_name, field_type] = field_def
        eField.setAttribute("data-index", fields[i][0])
        const field_values = fields[i].slice(1)
        const eFieldDef = document.createElement("div")
        eFieldDef.classList.add("field__definition")
        const eFieldName = document.createElement("div")
        eFieldName.classList.add("field__name")
        eFieldName.textContent = field_name
        const eFieldType = document.createElement("div")
        eFieldType.classList.add("field__type")
        eFieldType.textContent = field_type
        const eFieldValues = document.createElement("ol")
        eFieldValues.classList.add("field__values")
        eFieldDef.appendChild(eFieldName)
        eFieldDef.appendChild(eFieldType)
        eField.appendChild(eFieldDef)
        eField.appendChild(eFieldValues)
        for (const field_value of field_values) {
            const eFieldValue = document.createElement("div")
            eFieldValue.classList.add("field__value")
            if (Array.isArray(field_value)) {
                if (field_value[0] !== "s") {
                    console.warn("hmm")
                } else {
                    const field_struct = add_struct(field_value, eFieldValue, false)
                    struct_data_map.set(field_struct, field_value)
                }
            } else {
                eFieldValue.textContent = field_value;
            }
            eFieldValues.appendChild(eFieldValue)
        }
        eStructFields.appendChild(eField)
    }
}

function ensure_expanded(struct) {
    if (struct.classList.contains("struct--expanded") && !struct.querySelector(":scope > .struct__fields")) {
        const struct_name = struct.querySelector(".struct__name").textContent
        const def = structs[struct_name][1]
        const data = struct_data_map.get(struct)
        add_fields(struct, data, def)
    }
}

window.addEventListener("click", event => {
    let parent = event.target
    do {
        if (parent.classList.contains("field")) {
            console.log(parent)
            parent.classList.toggle("field--expanded")
            if (parent.classList.contains("field--expanded")) {
                const child_structs = parent.querySelectorAll(":scope > .field__values > .field__value > .struct")
                console.log("child_structs: %o", child_structs)
                if (child_structs.length === 1) {
                    console.log(child_structs[0])
                    child_structs[0].classList.add("struct--expanded")
                    ensure_expanded(child_structs[0])
                }
            }
            break
        } else if (parent.classList.contains("struct")) {
            console.log(parent)
            parent.classList.toggle("struct--expanded")
            ensure_expanded(parent)
            break
        }
    } while (parent = parent.parentElement)
})
