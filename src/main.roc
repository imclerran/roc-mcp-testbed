app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br",
    json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.13.0/RqendgZw5e1RsQa3kFhgtnMP8efWoqGRsAvubx4-zus.tar.br",
    ansi: "https://github.com/lukewilliamboswell/roc-ansi/releases/download/0.8.0/RQlGWlkQEfxtkSYKl0nHNQaOFT0-Jh7NNFEX2IPXlec.tar.br",
    rt: "https://github.com/imclerran/rtils/releases/download/v0.1.7/xGdIJGyOChqLXjqx99Iqunxz3XyEpBp5zGOdb3OVUhs.tar.br",
    dt: "https://github.com/imclerran/roc-isodate/releases/download/v0.7.4/bEDedspHQApTvZI2GJfgXpzUmYO_ATw5d6xE_w1qcME.tar.br",
    jv: "https://github.com/Enkidatron/roc-json-value/releases/download/0.0.2/VXTTp0a_zFOaCYmWN480h81mUTvQ40zRDuSSqn_WS9A.tar.br",
}

import ansi.ANSI
import cli.Cmd
import cli.Stderr
import cli.Stdin
import cli.Stdout
import cli.Utc
import dt.DateTime
import dt.Duration
import dt.Time
import dt.Now {
    now!: Utc.now!,
    now_to_nanos: Utc.to_nanos_since_epoch,
}
import Msg
import Router exposing [ServerState, Response]
import Tool

main! = |_args|
    log!("Starting server...", Info)
    tools = define_tools({})
    loop!(Uninitialized({ tools: tools })) |> Result.map_ok(|_| {})

loop! : ServerState => Result ServerState [Exit I32 Str]
loop! = |state|
    request =
        Stdin.line!({})
        |> Result.map_err(|_| Exit(1, "Failed to read line"))?
    new_state =
        handle_request!(request, state)
        |> Result.on_err(|_| Ok(state))?
    loop!(new_state)

handle_request! : Str, ServerState => Result ServerState _
handle_request! = |request_str, state|
    message_result = Msg.decode_str(request_str)
    log!("Received request: ${request_str}", Info)
    when message_result is
        Ok(message) ->
            route_result = Router.route(message, state)
            when route_result is
                Ok((response, new_state)) ->
                    send_response!(response)
                    |> Result.on_err(|_| Ok({}))?
                    Ok(new_state)

                Err(HandlerError(error_msg)) ->
                    log!("Handler error: ${error_msg}", Error)
                    Ok(state)

        Err(_) ->
            log!("Failed to parse JSON-RPC message: ${request_str}", Error)
            Ok(state)

send_response! : Response => Result {} _
send_response! = |response|
    when response is
        Initialization(init_resp) ->
            log!("Server initialized", Info)
            response_json = format_init_response(init_resp)
            Stdout.line!(response_json)

        InitializationComplete ->
            log!("Handshake complete", Info)
            Ok({})

        ToolsList(tools_resp) ->
            response_json = format_tools_response(tools_resp)
            Stdout.line!(response_json)

        ToolCall(tool_call) ->
            execute_tool!(tool_call)

        Error(error_resp) ->
            log!("JSON-RPC error: ${error_resp.error.message}", Error)
            response_json = format_error_response(error_resp)
            Stdout.line!(response_json)

        Notification(_) ->
            Ok({})

execute_tool! : { id : Msg.IdType, tool_name : Str, arguments : Dict Str Str } => Result {} _
execute_tool! = |tool_call|
    tool_result =
        when tool_call.tool_name is
            "zulu_datetime" ->
                zulu_datetime_tool!(tool_call.arguments)

            "local_datetime" ->
                local_datetime_tool!(tool_call.arguments)

            _ ->
                Err("Unknown tool: ${tool_call.tool_name}")

    when tool_result is
        Ok(content) ->
            response_json = format_tool_success_response(tool_call.id, content)
            Stdout.line!(response_json)

        Err(error_msg) ->
            error_response_json = format_error_response(
                {
                    id: tool_call.id,
                    error: { code: -32602, message: error_msg },
                },
            )
            Stdout.line!(error_response_json)

zulu_datetime_tool! : Dict Str Str => Result Str _
zulu_datetime_tool! = |_|
    Now.date_time!({})
    |> DateTime.to_iso_str
    |> Str.concat("Z")
    |> Ok

local_datetime_tool! : Dict Str Str => Result Str _
local_datetime_tool! = |_|
    now = Now.date_time!({})
    offset =
        get_offset!({})
        |> Result.map_err(|_| "Failed to get timezone offset")?
    DateTime.add(now, offset)
    |> DateTime.to_iso_str
    |> Ok

get_offset! : {} => Result Duration.Duration [InvalidOffset]
get_offset! = |{}|
    Cmd.new("date") |> Cmd.arg("+%z") |> Cmd.output! |> .stdout |> offset_to_duration

offset_to_duration : List U8 -> Result Duration.Duration [InvalidOffset]
offset_to_duration = |bytes|
    when bytes is
        ['+', b1, b2, b3, b4, ..] ->
            hours = [b1, b2] |> Str.from_utf8_lossy |> Str.to_i64 |> Result.map_err(|_| InvalidOffset)?
            minutes = [b3, b4] |> Str.from_utf8_lossy |> Str.to_i64 |> Result.map_err(|_| InvalidOffset)?
            Ok(Duration.from_hms(hours, minutes, 0))

        ['-', b1, b2, b3, b4, ..] ->
            hours = [b1, b2] |> Str.from_utf8_lossy |> Str.to_i64 |> Result.map_err(|_| InvalidOffset)? |> Num.mul(-1)
            minutes = [b3, b4] |> Str.from_utf8_lossy |> Str.to_i64 |> Result.map_err(|_| InvalidOffset)? |> Num.mul(-1)
            Ok(Duration.from_hms(hours, minutes, 0))

        _ -> Err(InvalidOffset)

format_init_response = |init_resp|
    id_str = format_id(init_resp.id)
    """
    {"jsonrpc":"2.0","id":${id_str},"result":{"protocolVersion":"${init_resp.result.protocolVersion}","capabilities":{"experimental":{},"prompts":{"listChanged":${bool_to_str(init_resp.result.capabilities.prompts.listChanged)}},"resources":{"subscribe":${bool_to_str(init_resp.result.capabilities.resources.subscribe)},"listChanged":${bool_to_str(init_resp.result.capabilities.resources.listChanged)}},"tools":{"listChanged":${bool_to_str(init_resp.result.capabilities.tools.listChanged)}}},"serverInfo":{"name":"${init_resp.result.serverInfo.name}","version":"${init_resp.result.serverInfo.version}"}}}
    """

format_tools_response = |tools_resp|
    id_str = format_id(tools_resp.id)
    tools_json = format_tools_list(tools_resp.result.tools)
    """
    {"jsonrpc":"2.0","id":${id_str},"result":{"tools":[${tools_json}]}}
    """

format_tools_list = |tools|
    tools
    |> List.map(
        |tool|
            """
            {"name":"${tool.name}","description":"${tool.description}","inputSchema":{"type":"${tool.inputSchema.type}","properties":{}}}
            """,
    )
    |> Str.join_with(",")

format_error_response = |error_resp|
    id_str = format_id(error_resp.id)
    """
    {"jsonrpc":"2.0","id":${id_str},"error":{"code":${Num.to_str(error_resp.error.code)},"message":"${error_resp.error.message}"}}
    """

format_tool_success_response = |id, content|
    id_str = format_id(id)
    """
    {"jsonrpc":"2.0","id":${id_str},"result":{"content":[{"type":"text","text":"${content}"}]}}
    """

format_id = |id|
    when id is
        Str(s) -> "\"${s}\""
        Num(n) -> Num.to_str(n)

log! : Str, [Info, Warn, Error] => _
log! = |message, type|
    Now.time!({})
    |> Time.format(ANSI.color("[{hh}:{mm}:{ss}]", { fg: Standard Blue }))
    |> Str.concat(colorize(" ${message}", type))
    |> Stderr.line!
    |> Result.with_default({})

colorize = |msg, type|
    when type is
        Info -> ANSI.color(msg, { fg: Standard Green })
        Warn -> ANSI.color(msg, { fg: Standard Yellow })
        Error -> ANSI.color(msg, { fg: Standard Red })

bool_to_str = |bool| if bool then "true" else "false"

define_tools : {} -> List Tool.Tool
define_tools = |{}|
    [
        Tool.create_simple_tool("zulu_datetime", "Get the current zulu datetime in ISO 8601 format"),
        Tool.create_simple_tool("local_datetime", "Get the current local datetime in ISO 8601 format"),
    ]
