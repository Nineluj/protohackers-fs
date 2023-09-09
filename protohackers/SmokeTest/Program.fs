open System.IO
open System.Net
open System.Net.Sockets

type LoggingLevel =
    | Error
    | Info
    | Trace

type LoggingConfig = {
    level: LoggingLevel
}

type Logger (config: LoggingConfig) =
    static member withoutNewline (msg: string) =
        msg.Replace("\n", "\\n")
        
    member x.trace msg =
        match config.level with
        | Trace -> printfn $"trace> {Logger.withoutNewline msg}"
        | _ -> ()
    member x.info msg =
        match config.level with
        | Info | Trace -> printfn $"info> {Logger.withoutNewline msg}"
        | _ -> ()
    member x.error msg =
        printfn $"error> {Logger.withoutNewline msg}"

/// Handle an individual client
let handleClient (logger: Logger) (client: TcpClient) =
    async {
        logger.info "Starting handleClient"
        use stream = client.GetStream()
        // use reader = new StreamReader(stream)
        // use writer = new StreamWriter(stream)
        
        let buffer: byte array = Array.zeroCreate 4096
        let rec loop () = async {
            let! n = stream.ReadAsync(buffer).AsTask() |> Async.AwaitTask
            logger.info "Received from client"
            if n = 0 then
                client.Client.Shutdown(SocketShutdown.Both)
                client.Close()
                logger.info "Ending handleClient"
            else
                logger.trace $"Data: %A{buffer}"
                do! stream.WriteAsync(buffer, 0, n) |> Async.AwaitTask
                do! stream.FlushAsync() |> Async.AwaitTask
                logger.info "Sent to client"
                do! loop ()
        }
        do! loop ()
    }

/// Start a TcpListener and begin accepting connections
let startServer handleClientFn (ip: IPAddress, port: int) =
    let listener = TcpListener(ip, port)
    printfn $"Starting listener on port {port}"
    listener.Start()

    let rec loop () =
        async {
            while true do
                let! client = listener.AcceptTcpClientAsync() |> Async.AwaitTask
                let! _ = (handleClientFn client) |> Async.StartChild
                return! loop ()
        }
    loop ()

[<EntryPoint>]
let main _args =
    let logger = Logger({ level = Trace })
    startServer (handleClient logger) (IPAddress.Any, 10_000) |> Async.RunSynchronously
    0
