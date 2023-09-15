module Server

open System.IO
open System.Net
open System.Net.Sockets

type ResponseHandlerResult = { exit: bool; result: string }
type ResponseHandlerFn = string -> ResponseHandlerResult

type Application(port: int, logger: Logging.Logger, respondFn: ResponseHandlerFn) =
    static member private bufferSize = 4096

    member private this.loop (reader: StreamReader) (writer: StreamWriter) (buffer: byte array) =
        async {
            let! line = reader.ReadLineAsync() |> Async.AwaitTask
            if isNull line then
                return ()
            else
                match respondFn line with
                | { exit = exit; result = response } ->
                    do! writer.WriteLineAsync(response) |> Async.AwaitTask
                    do! writer.FlushAsync() |> Async.AwaitTask
                    if exit then () else return! this.loop reader writer buffer
        }

    member private this.handleClient(client: TcpClient) : Async<Unit> =
        async {
            client.NoDelay <- true
            logger.info $"Echo connection opened with {client.Client.RemoteEndPoint}"
            use stream = client.GetStream()
            let buffer = Array.zeroCreate Application.bufferSize
            do! this.loop <| new StreamReader(stream) <| new StreamWriter(stream) <| buffer
            logger.info "Closing connection"
            client.Client.Shutdown(SocketShutdown.Both)
            client.Close()
        }

    member this.run: Async<unit> =
        let listener = TcpListener(IPAddress.Any, port)
        logger.info $"Starting TCP listener on port {port}"
        listener.Start()

        async {
            while true do
                let! client = listener.AcceptTcpClientAsync() |> Async.AwaitTask
                Async.Start(this.handleClient client)
        }
