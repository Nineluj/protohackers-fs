open System
open System.Net
open System.Net.Sockets

let echo (logger: Logging.Logger) (client: TcpClient) : Async<Unit> =
    async {
        client.NoDelay <- true
        logger.info $"Echo connection opened with {client.Client.RemoteEndPoint}"
        use stream = client.GetStream()
        let buffer = Array.zeroCreate 4096

        let rec loop () =
            async {
                let! read = stream.AsyncRead(buffer, 0, buffer.Length)
                logger.info "Received data"
                logger.trace $"Data: %A{buffer[0 .. read - 1] |> System.Text.Encoding.ASCII.GetString}"

                if read = 0 then
                    return ()
                else
                    logger.info "Sending same thing back"
                    do! stream.AsyncWrite(buffer, 0, read)
                    return! loop ()
            }

        do! loop ()
        logger.info "Closing connection"
        client.Client.Shutdown(SocketShutdown.Both)
        client.Close()
    }

let server (listener: TcpListener) clientFn : Async<unit> =
    async {
        while true do
            let! client = listener.AcceptTcpClientAsync() |> Async.AwaitTask
            Async.Start(clientFn client)
    }

let port =
    try
        Environment.GetEnvironmentVariable("SVC_PORT") |> int
    with :? ArgumentNullException ->
        failwith "Port not set"

let logger = Logging.Logger({ level = Logging.Trace })
let listener = TcpListener(IPAddress.Any, port)
logger.info $"Starting TCP listener on port {port}"
listener.Start()

server listener (echo logger)
|> Async.RunSynchronously
