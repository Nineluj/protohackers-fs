module Messaging

open System
open System.Collections.Generic
open System.Text.Json

type HelloMessage = { northeastern_username: string }
type StartMessage = { id: string }
type GuessMessage = { id: string; word: string }
type RetryMessageGuess = { word: string; marks: int list }

type RetryMessage =
    { id: string
      guesses: RetryMessageGuess list }

type ErrorMessage = { message: string }
type ByeMessage = { id: string; flag: string }

type ClientToServer =
    | Hello of HelloMessage
    | Guess of GuessMessage

    member this.serializeToJson() =
        let serializeWithType (typeString: string) (data: obj) : string =
            let json = JsonSerializer.SerializeToNode(data)
            json["type"] <- typeString
            json.ToJsonString()

        match this with
        | Hello helloMessage -> serializeWithType "hello" helloMessage
        | Guess guessMessage -> serializeWithType "guess" guessMessage

type ServerToClient =
    | Start of StartMessage
    | Retry of RetryMessage
    | Error of ErrorMessage
    | Bye of ByeMessage

let serializeOptions = JsonSerializerOptions()

let tryGetProperty (property: string) (element: JsonElement) =
    try
        Some(element.GetProperty(property))
    with :? KeyNotFoundException ->
        None

let safeDeserialize<'T> (element: JsonElement) =
    try
        Some(element.Deserialize<'T>(serializeOptions))
    with
    | :? NotSupportedException -> None
    | :? InvalidOperationException -> None

let convertToServerToClientMessage (jd: JsonDocument) : ServerToClient option =
    let root = jd.RootElement

    match tryGetProperty "type" root with
    | None -> None
    | Some typ ->
        match typ.GetString() with
        | "start" -> safeDeserialize<StartMessage> root |> Option.map Start
        | "retry" -> safeDeserialize<RetryMessage> root |> Option.map Retry
        | "error" -> safeDeserialize<ErrorMessage> root |> Option.map Error
        | "bye" -> safeDeserialize<ByeMessage> root |> Option.map Bye
        | _ -> None
