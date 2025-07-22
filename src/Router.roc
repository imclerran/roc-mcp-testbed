module [Response, ServerState, route]

import Msg exposing [Msg, InitMsg, NotificationMsg, RequestMsg]
import Tool exposing [Tool]

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
                tools : List Tool,
            },
        },
    ToolCall {
            id : Msg.IdType,
            tool_name : Str,
            arguments : Dict Str Str,
        },
    Error { id : Msg.IdType, error : { code : I32, message : Str } },
    Notification NotificationMsg,
    InitializationComplete,
]

ServerState : [
    Uninitialized { tools : List Tool },
    Initializing {
            protocol_version : Str,
            client_info : { name : Str, version : Str },
            client_capabilities : [None],
            tools : List Tool,
        },
    Initialized {
            protocol_version : Str,
            client_info : { name : Str, version : Str },
            client_capabilities : [None],
            tools : List Tool,
        },
]

route : Msg, ServerState -> Result (Response, ServerState) [HandlerError Str]
route = |message, state|
    when (message, state) is
        (Init(init_msg), Uninitialized(_uninitialized_state)) ->
            handle_initialize(init_msg, state)

        (Init(init_msg), Initializing(_)) ->
            Ok(
                (
                    Error {
                        id: init_msg.id,
                        error: { code: -32600, message: "Server initialization already in progress" },
                    },
                    state,
                ),
            )

        (Init(init_msg), Initialized(_)) ->
            Ok(
                (
                    Error {
                        id: init_msg.id,
                        error: { code: -32600, message: "Server is already initialized" },
                    },
                    state,
                ),
            )

        (Notification(notif_msg), Initializing(_)) if notif_msg.method == "notifications/initialized" ->
            handle_initialized_notification(notif_msg, state)

        (Notification(notif_msg), Initialized(_)) ->
            Ok((Notification(notif_msg), state))

        (Request(req_msg), Initialized(_)) if req_msg.method == "tools/list" ->
            handle_tools_list(req_msg, state)

        (Request(req_msg), Initialized(_)) if req_msg.method == "tools/call" ->
            handle_tools_call(req_msg, state)

        (Request(req_msg), _) ->
            Ok(
                (
                    Error {
                        id: req_msg.id,
                        error: { code: -32601, message: "Method not found: ${req_msg.method}" },
                    },
                    state,
                ),
            )

        (Notification(notif_msg), _) ->
            Ok((Notification(notif_msg), state))

handle_initialize : InitMsg, ServerState -> Result (Response, ServerState) [HandlerError Str]
handle_initialize = |init_msg, state|
    new_state = Initializing(
        {
            protocol_version: init_msg.params.protocol_version,
            client_info: init_msg.params.client_info,
            client_capabilities: None,
            tools: get_tools(state),
        },
    )
    response = Initialization {
        id: init_msg.id,
        result: {
            protocolVersion: init_msg.params.protocol_version,
            capabilities: {
                experimental: {},
                prompts: { listChanged: Bool.false },
                resources: { subscribe: Bool.false, listChanged: Bool.false },
                tools: { listChanged: Bool.false },
            },
            serverInfo: { name: "date_and_time", version: "0.1.0" },
        },
    }
    Ok((response, new_state))

handle_initialized_notification : NotificationMsg, ServerState -> Result (Response, ServerState) [HandlerError Str]
handle_initialized_notification = |notif_msg, state|
    when state is
        Initializing(st) ->
            new_state = Initialized(
                {
                    protocol_version: st.protocol_version,
                    client_info: st.client_info,
                    client_capabilities: st.client_capabilities,
                    tools: st.tools,
                },
            )
            Ok((InitializationComplete, new_state))

        _ -> Ok((Notification(notif_msg), state))

get_tools : ServerState -> List Tool
get_tools = |state|
    when state is
        Uninitialized(st) -> st.tools
        Initializing(st) -> st.tools
        Initialized(st) -> st.tools

handle_tools_list : RequestMsg, ServerState -> Result (Response, ServerState) [HandlerError Str]
handle_tools_list = |req_msg, state|
    response = ToolsList {
        id: req_msg.id,
        result: {
            tools: get_tools(state),
        },
    }
    Ok((response, state))

handle_tools_call : RequestMsg, ServerState -> Result (Response, ServerState) [HandlerError Str]
handle_tools_call = |req_msg, state|
    tool_name = req_msg.params.name
    if Str.is_empty(tool_name) then
        Ok(
            (
                Error {
                    id: req_msg.id,
                    error: { code: -32602, message: "Missing required parameter: name" },
                },
                state,
            ),
        )
    else
        Ok(
            (
                ToolCall {
                    id: req_msg.id,
                    tool_name: tool_name,
                    arguments: req_msg.params.arguments,
                },
                state,
            ),
        )
