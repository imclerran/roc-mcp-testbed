# Roc Model-Context-Protocol server
This is a simple Roc MCP server proof-of-concept, hacked together in a few hours. Take it for what its worth.

## Configuring in Claude desktop:
Go to `Claude->Settings->Developer->Edit Config`, and add the MCP server like so:
```json
{
  "mcpServers": {
    "current_datetime": {
      "command": "/absolute/path/to/your/roc/",
      "args": [
        "/absolute/path/to/roc-mcp-testbed/src/main.roc"
      ]
    }
  }
}
```

## Manual Test Messages
The MCP server communicates over Stdin/Stdout, with additional info logged to Stderr.

You can mannually test the behavior of the server by running the binary from your cli and sending messages over Stdin. Model Context Protocol uses JSON-RPC, and requires an initialization handshake for before the server will handle tool calls.

Below are some messages to get you started:

__1) Initialization__
```json
{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05", "capabilities": {}, "clientInfo": {"name": "manual-test", "version": "1.0.0"}}}
```

__2) Confirm the handshake__
```json
{"jsonrpc": "2.0", "method": "notifications/initialized"}
```

__3) Get the tool list__
```json
{"jsonrpc": "2.0", "id": 2, "method": "tools/list"}
```

__4) Call a tool__
```json
{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "local_datetime", "arguments": {}}}
```
