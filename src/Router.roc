module [Response, ServerState, route]

import Msg exposing [Msg, get_method, get_id]

Response : [
    Initialization {
            id : Msg.IdType,
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
            id : Msg.IdType,
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
            id : Msg.IdType,
            tool_name : Str,
            arguments : Dict Str Str,
        },
    Error { id : Msg.IdType, error : { code : I32, message : Str } },
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

route : Msg, ServerState -> Result (Response, ServerState) [HandlerError Str]
route = |message, state|
    method = get_method(message)
    method_parts = method |> Str.split_on("/")

    when (method_parts, state) is
        (["initialize"], Uninitialized) ->
            handle_initialize(message, state)

        (["initialize"], Initializing(_)) ->
            id_result = get_id(message)
            when id_result is
                Ok(id) ->
                    Ok(
                        (
                            Error {
                                id: id,
                                error: { code: -32600, message: "Server initialization already in progress" },
                            },
                            state,
                        ),
                    )

                Err(_) ->
                    Err(HandlerError("Server initialization already in progress"))

        (["initialize"], Initialized(_)) ->
            id_result = get_id(message)
            when id_result is
                Ok(id) ->
                    Ok(
                        (
                            Error {
                                id: id,
                                error: { code: -32600, message: "Server is already initialized" },
                            },
                            state,
                        ),
                    )

                Err(_) ->
                    Err(HandlerError("Server is already initialized"))

        (["notifications", "initialized"], Initializing(_)) ->
            handle_initialized_notification(message, state)

        (["notifications", str], Initialized(_)) ->
            Ok(
                (
                    Notification(str),
                    state,
                )
            )

        (["tools", "list"], Initialized(_)) ->
            handle_tools_list(message, state)

        (["tools", "call"], Initialized(_)) ->
            handle_tools_call(message, state)

        (_, _) ->
            id_result = get_id(message)
            when id_result is
                Ok(id) ->
                    Ok(
                        (
                            Error {
                                id: id,
                                error: { code: -32601, message: "Method not found: ${method}" },
                            },
                            state,
                        ),
                    )

                Err(_) ->
                    Ok(
                        (
                            Notification(method),
                            state,
                        )
                    )

handle_initialize : Msg, ServerState -> Result (Response, ServerState) [HandlerError Str]
handle_initialize = |message, _state|
    when message is
        Init(req) ->
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

        Request(req) ->
            Ok(
                (
                    Error {
                        id: req.id,
                        error: { code: -32600, message: "Expected initialize method for handshake" },
                    },
                    Uninitialized,
                ),
            )

        _ ->
            Err(HandlerError("Expected initialize method for handshake"))

handle_initialized_notification : Msg, ServerState -> Result (Response, ServerState) [HandlerError Str]
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
            Err(HandlerError("Expected notifications/initialized to complete handshake"))

handle_tools_list : Msg, ServerState -> Result (Response, ServerState) [HandlerError Str]
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

        (Request(req), _) ->
            Ok(
                (
                    Error {
                        id: req.id,
                        error: { code: -32600, message: "tools/list can only be called after server initialization" },
                    },
                    state,
                ),
            )

        _ ->
            Err(HandlerError("tools/list can only be called after server initialization"))

handle_tools_call : Msg, ServerState -> Result (Response, ServerState) [HandlerError Str]
handle_tools_call = |message, state|
    when (message, state) is
        (Request(req), Initialized(_st)) ->
            tool_name = req.params.name
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
                            arguments: req.params.arguments,
                        },
                        state,
                    ),
                )

        (Request(req), _) ->
            Ok(
                (
                    Error {
                        id: req.id,
                        error: { code: -32600, message: "tools/call can only be called after server initialization" },
                    },
                    state,
                ),
            )

        _ ->
            Err(HandlerError("tools/call can only be called after server initialization"))
