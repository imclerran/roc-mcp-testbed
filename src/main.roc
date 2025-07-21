app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br",
    json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.13.0/RqendgZw5e1RsQa3kFhgtnMP8efWoqGRsAvubx4-zus.tar.br",
    ansi: "https://github.com/lukewilliamboswell/roc-ansi/releases/download/0.8.0/RQlGWlkQEfxtkSYKl0nHNQaOFT0-Jh7NNFEX2IPXlec.tar.br",
    dt: "https://github.com/imclerran/roc-isodate/releases/download/v0.7.4/bEDedspHQApTvZI2GJfgXpzUmYO_ATw5d6xE_w1qcME.tar.br",
}

import cli.Stdout
import cli.Stdin
import cli.Stderr
import cli.Utc
import ansi.ANSI
import dt.Time
import dt.DateTime
import dt.Now {
    now!: Utc.now!,
    now_to_nanos: Utc.to_nanos_since_epoch,
}
import Message
import Router exposing [ServerState, Response]

main! = |_args|
    loop!(Uninitialized) |> Result.map_ok(|_| {})

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
    message_result = Message.decode(Str.to_utf8(request_str))
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

        Err(TooShort) ->
            log!("Message too short", Error)
            Ok(state)

        Err(InvalidMessage) ->
            log!("Invalid message format", Error)
            Ok(state)

send_response! : Response => Result {} _
send_response! = |response|
    when response is
        Initialization(init_resp) ->
            log!("Server initialized", Info)
            response_json = format_init_response(init_resp)
            Stdout.line!(response_json)

        ToolsList(tools_resp) ->
            response_json = format_tools_response(tools_resp)
            Stdout.line!(response_json)

        Error(error_resp) ->
            response_json = format_error_response(error_resp)
            Stdout.line!(response_json)

        ToolCall(tool_call) ->
            execute_tool!(tool_call)

        Notification(msg) ->
            log!(msg, Info)
            Ok({})

        InitializationComplete ->
            log!("Handshake complete", Info)
            Ok({})

execute_tool! : { id : Message.IdType, tool_name : Str, arguments : Dict Str Str } => Result {} _
execute_tool! = |tool_call|
    tool_result =
        when tool_call.tool_name is
            "current_datetime" ->
                current_datetime_tool!(tool_call.arguments)

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

current_datetime_tool! : Dict Str Str => Result Str _
current_datetime_tool! = |_arguments|
    Now.date_time!({})
    |> DateTime.to_iso_str
    |> Str.concat("Z")
    |> Ok

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
