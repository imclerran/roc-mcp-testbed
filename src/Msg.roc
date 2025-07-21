module [Msg, InitMsg, NotificationMsg, RequestMsg, IdType, decode_str, decode_bytes, get_method, get_id, get_id_str, compare_ids]

import jv.Value as JV
import rt.Compare

InitMsg : { id : IdType, jsonrpc : Str, method : Str, params : { capabilities : Dict Str Str, client_info : { name : Str, version : Str }, protocol_version : Str } }
NotificationMsg : { jsonrpc : Str, method : Str, params : Dict Str Str }
RequestMsg : { id : IdType, jsonrpc : Str, method : Str, params : { arguments : Dict Str Str, name : Str } }

Msg : [
    Init InitMsg,
    Notification NotificationMsg,
    Request RequestMsg,
]

IdType : [Str Str, Num U64]

id_recipe : JV.Recipe IdType
id_recipe = JV.one_of(
    [
        JV.map(JV.u64, Num),
        JV.map(JV.string, Str),
    ],
)

client_info_recipe : JV.Recipe _
client_info_recipe =
    { JV.build_record <-
        name: JV.field(JV.string, "name"),
        version: JV.field(JV.string, "version"),
    }

init_params_recipe : JV.Recipe _
init_params_recipe =
    { JV.build_record <-
        protocol_version: JV.field(JV.string, "protocolVersion"),
        capabilities: JV.field(JV.dict(JV.string), "capabilities"),
        client_info: JV.field(client_info_recipe, "clientInfo"),
    }

request_params_recipe : JV.Recipe _
request_params_recipe =
    { JV.build_record <-
        name: JV.field(JV.string, "name"),
        arguments: JV.field(JV.dict(JV.string), "arguments"),
    }

msg_init_recipe : JV.Recipe InitMsg
msg_init_recipe =
    { JV.build_record <-
        jsonrpc: JV.field(JV.string, "jsonrpc"),
        id: JV.field(id_recipe, "id"),
        method: JV.field(JV.string, "method"),
        params: JV.field(init_params_recipe, "params"),
    }

msg_notification_recipe : JV.Recipe NotificationMsg
msg_notification_recipe =
    { JV.build_record <-
        jsonrpc: JV.field(JV.string, "jsonrpc"),
        method: JV.field(JV.string, "method"),
        params: JV.optional_field(JV.dict(JV.string), "params", Dict.empty({})),
    }

msg_request_recipe : JV.Recipe RequestMsg
msg_request_recipe =
    { JV.build_record <-
        jsonrpc: JV.field(JV.string, "jsonrpc"),
        id: JV.field(id_recipe, "id"),
        method: JV.field(JV.string, "method"),
        params: JV.optional_field(request_params_recipe, "params", { name: "", arguments: Dict.empty({}) }),
    }

msg_recipe : JV.Recipe Msg
msg_recipe = JV.one_of(
    [
        msg_init_recipe |> JV.map(Init),
        msg_request_recipe |> JV.map(Request),
        msg_notification_recipe |> JV.map(Notification),
    ],
)

get_method : Msg -> Str
get_method = |msg|
    when msg is
        Init({ method }) -> method
        Request({ method }) -> method
        Notification({ method }) -> method

get_id : Msg -> Result IdType [NotificationHasNoId]
get_id = |msg|
    when msg is
        Init({ id }) -> Ok(id)
        Request({ id }) -> Ok(id)
        Notification(_) -> Err(NotificationHasNoId)

get_id_str : Msg -> Result Str [NotificationHasNoId]
get_id_str = |msg|
    when msg is
        Init({ id }) -> Ok(id_to_str(id))
        Request({ id }) -> Ok(id_to_str(id))
        Notification(_) -> Err(NotificationHasNoId)

id_to_str : IdType -> Str
id_to_str = |id|
    when id is
        Str(id_str) -> id_str
        Num(num) -> Num.to_str(num)

compare_ids : IdType, IdType -> [LT, EQ, GT]
compare_ids = |id1, id2|
    when (id1, id2) is
        (Str(str1), Str(str2)) -> Compare.str(str1, str2)
        (Num(num1), Num(num2)) -> Num.compare(num1, num2)
        (Str(str1), Num(num2)) ->
            when Str.to_u64(str1) is
                Ok(num1) -> Num.compare(num1, num2)
                Err(InvalidNumStr) -> Compare.str(str1, Num.to_str(num2))

        (Num(num1), Str(str2)) ->
            when Str.to_u64(str2) is
                Ok(num2) -> Num.compare(num1, num2)
                Err(InvalidNumStr) -> Compare.str(Num.to_str(num1), str2)

decode_str : Str -> Result Msg _
decode_str = |str| JV.decode_str(str, msg_recipe)

decode_bytes : List U8 -> Result Msg _
decode_bytes = |bytes| JV.decode_bytes(bytes, msg_recipe)

expect
    init_msg_json =
        """
        {"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05", "capabilities": {}, "clientInfo": {"name": "manual-test", "version": "1.0.0"}}}
        """
    result = JV.decode_str(init_msg_json, msg_recipe)
    result == Ok(Init({ jsonrpc: "2.0", id: Num(1), method: "initialize", params: { protocol_version: "2024-11-05", capabilities: Dict.empty({}), client_info: { name: "manual-test", version: "1.0.0" } } }))

expect
    request_msg_json =
        """
        {"jsonrpc": "2.0", "id": "3", "method": "tools/call", "params": {"name": "current_datetime", "arguments": {"arg1": "value1", "arg2": "value2"}}}
        """
    result = JV.decode_str(request_msg_json, msg_recipe)
    result == Ok(Request({ jsonrpc: "2.0", id: Str("3"), method: "tools/call", params: { name: "current_datetime", arguments: Dict.from_list([("arg1", "value1"), ("arg2", "value2")]) } }))

expect
    notification_msg_json =
        """
        {"jsonrpc": "2.0", "method": "notifications/initialized"}
        """
    result = JV.decode_str(notification_msg_json, msg_recipe)
    result == Ok(Notification({ jsonrpc: "2.0", method: "notifications/initialized", params: Dict.empty({}) }))

expect
    (compare_ids(Str("a"), Str("b")) == LT)
    and
    (compare_ids(Str("b"), Str("a")) == GT)
    and
    (compare_ids(Str("a"), Str("a")) == EQ)

expect
    (compare_ids(Num(1), Num(2)) == LT)
    and
    (compare_ids(Num(2), Num(1)) == GT)
    and
    (compare_ids(Num(1), Num(1)) == EQ)

expect
    (compare_ids(Str("1"), Num(2)) == LT)
    and
    (compare_ids(Num(2), Str("1")) == GT)
    and
    (compare_ids(Str("1"), Num(1)) == EQ)

expect
    (compare_ids(Str("1"), Num(12)) == LT)
    and
    (compare_ids(Num(12), Str("1")) == GT)
    and
    (compare_ids(Str("12"), Num(12)) == EQ)
    and
    (compare_ids(Str("21"), Num(3)) == GT)

expect
    (compare_ids(Num(97), Str("a")) == LT)
    and
    (compare_ids(Str("a"), Num(97)) == GT)
    