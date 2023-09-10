module Logging 

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
