open System
open FSharp.Data
open Server

module Messaging =
    type Request = JsonProvider<"./samples/request.json">
    type SuccessResponse = JsonProvider<"./samples/success-response.json">
    type FailResponse = Request

    type Response =
        | Failed of FailResponse.Root
        | Success of SuccessResponse.Root

        static member Serialize(x: Response) =
            let obj =
                match x with
                | Failed r -> r.JsonValue
                | Success r -> r.JsonValue

            obj.ToString(JsonSaveOptions.DisableFormatting)

type Input = Messaging.Request.Root
type Output = Messaging.Response

let isPrime (number: int64) =
    if number < 2L then
        false
    else
        let sqrtNumber = int (sqrt (float number))
        seq { 2L..sqrtNumber } |> Seq.exists (fun x -> number % x = 0L) |> not

let handleRequest (r: Input) : Output =
    match r.Method with
    | "isPrime" ->
        let result = r.Number % 1m = 0m && (r.Number |> Decimal.ToInt64 |> isPrime) 
        Messaging.Success(Messaging.SuccessResponse.Root("isPrime", result))
    | _ -> Messaging.Failed r


let respondFn input =
    try
        input
        |> Messaging.Request.Parse
        |> handleRequest
        |> Output.Serialize
        |> (fun x -> { exit = false; result = x })
    with _ ->
        { exit = true; result = input }

let port =
    try
        Environment.GetEnvironmentVariable("SVC_PORT") |> int
    with :? ArgumentNullException ->
        failwith "Port not set"

let logger = Logging.Logger({ level = Logging.Trace })
Application(port, logger, respondFn).run |> Async.RunSynchronously
