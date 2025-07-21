module [IdType, Message, decode, get_method, get_id]

import json.Json
import json.Option exposing [Option]

IdType : [Str Str, Num U64]

Message : [
    InitializationRequest {
            jsonrpc : Str,
            id : IdType,
            method : Str,
            params : {
                protocol_version : Str,
                capabilities : {},
                client_info : {
                    name : Str,
                    version : Str,
                },
            },
        },
    Notification {
            jsonrpc : Str,
            method : Str,
        },
    Request {
            jsonrpc : Str,
            id : IdType,
            method : Str,
            params : Dict Str Str,
        },
]

RawMessage : {
    jsonrpc : Str,
    id : Option IdType,
    method : Str,
    params : Option Str,
}

decode : List U8 -> Result Message [TooShort, InvalidMessage]
decode = |bytes|
    raw_result = decode_raw(bytes)
    when raw_result is
        Ok(raw) -> classify_message(raw)
        Err(TooShort) -> Err(TooShort)

decode_raw : List U8 -> Result RawMessage [TooShort]
decode_raw = |bytes|
    decode_with_num_id(bytes)
    |> Result.on_err(|_| decode_with_str_id(bytes))

decode_with_num_id : List U8 -> Result RawMessage [TooShort]
decode_with_num_id = |bytes|
    decoder = Json.utf8_with({ field_name_mapping: CamelCase })

    decoded :
        Decode.DecodeResult {
            jsonrpc : Str,
            id : Option U64,
            method : Str,
            params : Option Str,
        }
    decoded = Decode.from_bytes_partial(bytes, decoder)

    when decoded.result is
        Ok(raw) ->
            num_id =
                when Option.get(raw.id) is
                    None -> Option.none({})
                    Some(id) -> Option.some(Num(id))
            Ok(
                {
                    jsonrpc: raw.jsonrpc,
                    id: num_id,
                    method: raw.method,
                    params: raw.params,
                },
            )

        Err(e) -> Err(e)

decode_with_str_id : List U8 -> Result RawMessage [TooShort]
decode_with_str_id = |bytes|
    decoder = Json.utf8_with({ field_name_mapping: CamelCase })

    decoded :
        Decode.DecodeResult {
            jsonrpc : Str,
            id : Option Str,
            method : Str,
            params : Option Str,
        }
    decoded = Decode.from_bytes_partial(bytes, decoder)

    when decoded.result is
        Ok(raw) ->
            str_id =
                when Option.get(raw.id) is
                    None -> Option.none({})
                    Some(id) -> Option.some(Str(id))
            Ok(
                {
                    jsonrpc: raw.jsonrpc,
                    id: str_id,
                    method: raw.method,
                    params: raw.params,
                },
            )

        Err(e) -> Err(e)

classify_message : RawMessage -> Result Message [InvalidMessage]
classify_message = |raw|
    when (Option.get(raw.id), raw.method) is
        (Some(id), "initialize") ->
            when Option.get(raw.params) is
                Some(raw_params) ->
                    params_bytes = raw_params |> Str.to_utf8
                    decoder = Json.utf8_with({ field_name_mapping: CamelCase })
                    decoded :
                        Decode.DecodeResult {
                            protocol_version : Str,
                            capabilities : Str,
                            client_info : {
                                name : Str,
                                version : Str,
                            },
                        }
                    decoded = Decode.from_bytes_partial(params_bytes, decoder)
                    when decoded.result is
                        Ok(params) ->
                            Ok(
                                InitializationRequest {
                                    jsonrpc: raw.jsonrpc,
                                    id: id,
                                    method: raw.method,
                                    params: {
                                        protocol_version: params.protocol_version,
                                        capabilities: {},
                                        client_info: params.client_info,
                                    },
                                },
                            )

                        Err(_) -> Err(InvalidMessage)

                None -> Err(InvalidMessage)

        (None, _) ->
            # Notification (no id)
            Ok(
                Notification {
                    jsonrpc: raw.jsonrpc,
                    method: raw.method,
                },
            )

        (Some(id), _) ->
            # Regular request
            when Option.get(raw.params) is
                Some(raw_params) ->
                    params_bytes = raw_params |> Str.to_utf8
                    decoder = Json.utf8_with({ field_name_mapping: CamelCase })
                    decoded : Decode.DecodeResult (List (Str, Str))
                    decoded = Decode.from_bytes_partial(params_bytes, decoder)
                    when decoded.result is
                        Ok(params_list) ->
                            params = Dict.from_list(params_list)
                            Ok(
                                Request {
                                    jsonrpc: raw.jsonrpc,
                                    id: id,
                                    method: raw.method,
                                    params: params,
                                },
                            )

                        Err(_) -> Err(InvalidMessage)

                None ->
                    Ok(
                        Request {
                            jsonrpc: raw.jsonrpc,
                            id: id,
                            method: raw.method,
                            params: Dict.empty({}),
                        },
                    )

get_method : Message -> Str
get_method = |message|
    when message is
        InitializationRequest(req) -> req.method
        Notification(notif) -> notif.method
        Request(req) -> req.method

get_id : Message -> Result IdType [NoId]
get_id = |message|
    when message is
        InitializationRequest(req) -> Ok(req.id)
        Request(req) -> Ok(req.id)
        Notification(_) -> Err(NoId)
