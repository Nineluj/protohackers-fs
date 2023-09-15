open System.IO
open Argu
open NLog

open Client
open Guesser


type Arguments =
    | [<AltCommandLine("-d")>] DictionaryFile of path: string
    | [<AltCommandLine("-h")>] ServerHost of host: string
    | [<AltCommandLine("-p")>] ServerPort of port: int
    | [<AltCommandLine("-u")>] Username of string
    | [<AltCommandLine("-s")>] UseTLS

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | DictionaryFile _ -> "path to the dictionary file"
            | ServerHost _ -> "the host for the server to connect to"
            | ServerPort _ -> "the port on the host to connect to"
            | Username _ -> "username to use when connecting"
            | UseTLS -> "whether to use TLS when connecting"

[<EntryPoint>]
let main args =
    let args = ArgumentParser.Create<Arguments>().Parse(args)
    let wordlist = args.GetResult(DictionaryFile) |> File.ReadAllLines |> List.ofArray
    let logger = LogManager.GetCurrentClassLogger()
    logger.Info "Starting program"

    Guesser()
    |> respondFn
    |> Application
    |> (fun app ->
        app.run
            (getInitialState wordlist)
            (initialMessageFn (args.GetResult Username))
            (args.GetResult ServerHost)
            (args.GetResult ServerPort)
            (args.Contains UseTLS))
    |> Async.RunSynchronously

    logger.Info "Program ending..."
    0
