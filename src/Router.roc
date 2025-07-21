module [Response, ServerState, route]

import Message exposing [Message, get_method, get_id]

Response : [
    Initialization {
            id : Message.IdType,
            result : {
                protocolVersion : Str,
                capabilities : {
                    experimental : {},
                    prompts : { listChanged : Bool },
                    resources : { subscribe : Bool, listChanged : Bool },
                    tools : { listChanged : Bool },
                },
                serverInfo : { name : Str, version : Str },
            },
        },
    ToolsList {
            id : Message.IdType,
            result : {
                tools : List {
                    name : Str,
                    description : Str,
                    inputSchema : {
                        type : Str,
                        properties : {},
                    },
                },
            },
        },
    ToolCall {
            id : Message.IdType,
            tool_name : Str,
            arguments : Dict Str Str,
        },
    Error { id : Message.IdType, error : { code : I32, message : Str } },
    Notification Str,
    InitializationComplete,
]

ServerState : [
    Uninitialized,
    Initializing {
            protocol_version : Str,
            client_info : { name : Str, version : Str },
            client_capabilities : [None],
        },
    Initialized {
            protocol_version : Str,
            client_info : { name : Str, version : Str },
            client_capabilities : [None],
            tools : List Str,
        },
]

route : Message, ServerState -> Result (Response, ServerState) [HandlerError Str]
route = |message, state|
    method = get_method(message)

    when (method, state) is
        ("initialize", Uninitialized) ->
            handle_initialize(message, state)

        ("notifications/initialized", Initializing(_)) ->
            handle_initialized_notification(message, state)

        ("tools/list", Initialized(_)) ->
            handle_tools_list(message, state)

        ("tools/call", Initialized(_)) ->
            handle_tools_call(message, state)

        (_, _) ->
            id_result = get_id(message)
            when id_result is
                Ok(id) ->
                    Ok(
                        (
                            Error {
                                id: id,
                                error: { code: -32601, message: "Method not found" },
                            },
                            state,
                        ),
                    )

                Err(_) ->
                    Err(HandlerError("Unknown notification: ${method}"))

handle_initialize : Message, ServerState -> Result (Response, ServerState) [HandlerError Str]
handle_initialize = |message, _state|
    when message is
        InitializationRequest(req) ->
            new_state = Initializing(
                {
                    protocol_version: req.params.protocol_version,
                    client_info: req.params.client_info,
                    client_capabilities: None,
                },
            )

            response = Initialization {
                id: req.id,
                result: {
                    protocolVersion: req.params.protocol_version,
                    capabilities: {
                        experimental: {},
                        prompts: { listChanged: Bool.false },
                        resources: { subscribe: Bool.false, listChanged: Bool.false },
                        tools: { listChanged: Bool.false },
                    },
                    serverInfo: { name: "current_time", version: "1.10.1" },
                },
            }

            Ok((response, new_state))

        _ ->
            Err(HandlerError("Expected initialization request"))

handle_initialized_notification : Message, ServerState -> Result (Response, ServerState) [HandlerError Str]
handle_initialized_notification = |message, state|
    when (message, state) is
        (Notification(notif), Initializing(st)) if notif.method == "notifications/initialized" ->
            new_state = Initialized(
                {
                    protocol_version: st.protocol_version,
                    client_info: st.client_info,
                    client_capabilities: st.client_capabilities,
                    tools: [],
                },
            )

            Ok((InitializationComplete, new_state))

        _ ->
            Err(HandlerError("Expected initialized notification"))

handle_tools_list : Message, ServerState -> Result (Response, ServerState) [HandlerError Str]
handle_tools_list = |message, state|
    when (message, state) is
        (Request(req), Initialized(_st)) ->
            response = ToolsList {
                id: req.id,
                result: {
                    tools: [
                        {
                            name: "current_datetime",
                            description: "Get the current datetime in ISO 8601 format",
                            inputSchema: {
                                type: "object",
                                properties: {},
                            },
                        },
                    ],
                },
            }

            Ok((response, state))

        _ ->
            Err(HandlerError("Expected tools/list request in initialized state"))

handle_tools_call : Message, ServerState -> Result (Response, ServerState) [HandlerError Str]
handle_tools_call = |message, state|
    when (message, state) is
        (Request(req), Initialized(_st)) ->
            tool_name = Dict.get(req.params, "name") |> Result.with_default("")
            if Str.is_empty(tool_name) then
                Ok(
                    (
                        Error {
                            id: req.id,
                            error: { code: -32602, message: "Missing required parameter: name" },
                        },
                        state,
                    ),
                )
            else
                Ok(
                    (
                        ToolCall {
                            id: req.id,
                            tool_name: tool_name,
                            arguments: req.params,
                        },
                        state,
                    ),
                )

        _ ->
            Err(HandlerError("Expected tools/call request in initialized state"))
