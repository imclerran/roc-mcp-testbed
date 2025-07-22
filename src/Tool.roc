module [Tool, PropertySchema, InputSchema, create_tool, create_simple_tool]

PropertySchema : [
    String { description : Str },
    Number { description : Str, minimum : F64, maximum : F64 },
    Boolean { description : Str },
    Array { description : Str, items : PropertySchema },
    Object { description : Str, properties : Dict Str PropertySchema },
    Enum { description : Str, values : List Str },
]

InputSchema : {
    type : Str, # Always "object" for tool inputs
    properties : Dict Str PropertySchema,
    required : List Str,
}

Tool : {
    name : Str,
    description : Str,
    inputSchema : InputSchema,
}

create_tool : Str, Str, InputSchema -> Tool
create_tool = |name, description, inputSchema|
    {
        name: name,
        description: description,
        inputSchema: inputSchema,
    }

create_simple_tool : Str, Str -> Tool
create_simple_tool = |name, description|
    create_tool(name, description, {
        type: "object",
        properties: Dict.empty({}),
        required: [],
    })