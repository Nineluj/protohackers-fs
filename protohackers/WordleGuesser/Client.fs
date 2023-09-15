module Client

open System.IO
open System.Net.Security
open System.Net.Sockets
open NLog

type ResponseHandlerResult<'state> =
    { exit: bool
      result: string option
      state: 'state }
    with static member defaultValue defaultStateVal =
            { exit = true; result = None; state = defaultStateVal }

type ResponseHandlerFn<'state> = 'state -> string -> ResponseHandlerResult<'state>
type InitialMessageFn = unit -> string

type Application<'state>(respondFn: ResponseHandlerFn<'state>) =
    member this.logger = LogManager.GetCurrentClassLogger()

    member private this.loop (reader: StreamReader) (writer: StreamWriter) (state: 'state) =
        async {
            let! line = reader.ReadLineAsync() |> Async.AwaitTask

            if isNull line then
                return ()
            else
                match respondFn state line with
                | { exit = exit; result = response; state = newState } ->
                    match response with
                    | Some txt -> do! writer.WriteLineAsync(txt) |> Async.AwaitTask
                    | None -> ()

                    if exit then () else return! this.loop reader writer newState
        }

    member private this.handleClient (stream: Stream) (initialState: 'state) (initialMessage: InitialMessageFn option) : Async<Unit> =
        async {
            let reader = new StreamReader(stream)
            let writer = new StreamWriter(stream)
            writer.AutoFlush <- true
            
            this.logger.Info "Sending initial message"
            match initialMessage with
            | Some fn ->
                let payload = fn ()
                this.logger.Trace $"Sending [{payload}]"
                do! writer.WriteLineAsync(fn ()) |> Async.AwaitTask
            | None -> ()

            this.logger.Info "Starting loop"
            do! this.loop reader writer initialState
            this.logger.Info "Closing connection"
            stream.Close ()
        }

    member this.run (initialState: 'state) (initialMessage: InitialMessageFn option) host port useTLS : Async<unit> =
        async {
            let connection = new TcpClient(host, port)
            if useTLS then
                let secureStream = new SslStream(connection.GetStream())
                secureStream.AuthenticateAsClient host
                do! this.handleClient secureStream initialState initialMessage
            else
                do! this.handleClient (connection.GetStream ()) initialState initialMessage
        }
