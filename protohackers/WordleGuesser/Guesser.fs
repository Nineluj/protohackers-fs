module Guesser

open Microsoft.FSharp.Collections
open System.Text.Json
open Messaging
open Client
open NLog

type Input = ServerToClient
type Output = ClientToServer

type DomainState =
    { words: string list
      invalidLetters: Set<char>
      invalidLetterPlacements: Set<char> list
      definitePlacement: char option list }

let adjust f idx list =
    list |> List.mapi (fun i x -> if i = idx then f x else x)

type Guesser() =
    member this.logger = LogManager.GetCurrentClassLogger()
    
    member this.recordBadGuess (badGuess: RetryMessageGuess) (state: DomainState) : DomainState =
        // marks are: 0 -> bad letter, 1 -> wrong position, 2 -> right position
        let lettersAndMarks =
            badGuess.marks
            |> List.zip (badGuess.word |> List.ofSeq)
            // add the index
            |> List.mapi (fun i (letter, mark) -> i, letter, mark)

        let byMark = lettersAndMarks |> List.groupBy (fun (_, _, mark) -> mark) |> Map

        let count x = Seq.filter ((=) x) >> Seq.length

        let badLetters =
            Map.tryFind 0 byMark
            |> Option.map (fun entries ->
                entries
                |> List.map (fun (_, letter, _) -> letter)
                // only add entries where the number of occurrences is 1
                |> List.filter (fun letter -> (count letter badGuess.word) = 1)
                |> List.fold (fun (acc: Set<char>) letter -> Set.add letter acc) state.invalidLetters)
            |> Option.defaultValue state.invalidLetters

        let badlyPlacedLetters =
            Map.tryFind 1 byMark
            |> Option.map (fun entries ->
                entries
                |> List.fold
                    (fun acc (i, letter, _) -> adjust (fun (charSet: Set<char>) -> Set.add letter charSet) i acc)
                    state.invalidLetterPlacements)
            |> Option.defaultValue state.invalidLetterPlacements

        let correctlyPlacedLetters =
            Map.tryFind 2 byMark
            |> Option.map (fun entries ->
                entries
                |> List.fold
                    (fun acc (i, letter, _) -> adjust (fun (_: char option) -> Some(letter)) i acc)
                    state.definitePlacement)
            |> Option.defaultValue state.definitePlacement

        { state with
            invalidLetters = badLetters
            invalidLetterPlacements = badlyPlacedLetters
            definitePlacement = correctlyPlacedLetters }


    static member private matchesCriteria (w: string) (state: DomainState) =
        let chars = w |> List.ofSeq

        let hasBadLetter () =
            (chars |> List.exists state.invalidLetters.Contains)

        let hasBadlyPlacedLetter () =
            (chars
             |> List.zip state.invalidLetterPlacements
             |> List.exists (fun (invalidLetterSet, letter) -> invalidLetterSet.Contains letter))

        let hasWrongLetterPlacedInDefiniteSpot () =
            (chars
             |> List.zip state.definitePlacement
             |> List.exists (fun (definitive, letter) ->
                 match definitive with
                 | Some v -> letter <> v
                 | None -> false))

        not (
            hasBadLetter ()
            || hasBadlyPlacedLetter ()
            || hasWrongLetterPlacedInDefiniteSpot ()
        )

    static member private findNextSuitable wordlist state =
        match wordlist with
        | [] -> None, []
        | w :: rest ->
            match Guesser.matchesCriteria w state with
            | true -> Some(w), rest
            | false -> Guesser.findNextSuitable rest state

    member this.makeNewGuess(currState: DomainState) : string option * DomainState =
        let selected, remaining = Guesser.findNextSuitable currState.words currState
        selected, { currState with words = remaining }
    
    member this.handleRequest (state: DomainState) (input: Input) =
        match input with
        | Bye { id = _; flag = flag } ->
            this.logger.Info $"Got answer: {flag}"
             
            { exit = true
              result = None
              state = state }
        | Start { id = id } ->
            let guessMsg =
                { id = id
                  word = (List.item 0 state.words) }
                |> Guess
        
            { exit = false
              result = Some(guessMsg.serializeToJson ())
              state = { state with words = state.words.Tail } }
        | Retry { id = id; guesses = guesses } ->
            this.logger.Trace $"Guess: %s{(List.last guesses).word} was wrong..."
        
            state
            |> this.recordBadGuess (guesses |> List.last)
            |> this.makeNewGuess
            |> (fun (newGuess, newState) ->
                match newGuess with
                | None ->
                    this.logger.Error "Ran out of words..."
        
                    { exit = true
                      result = None
                      state = newState }
                | Some value ->
                    { exit = false
                      result = Some(({ id = id; word = value } |> Guess).serializeToJson ())
                      state = newState })
        | Error errorMessage ->
            printfn $"Got error {errorMessage}"
        
            { exit = true
              result = None
              state = state }

let getInitialState wordlist =
    { words = wordlist
      invalidLetters = Set.empty
      invalidLetterPlacements = List.init 5 (fun _ -> Set.empty)
      definitePlacement = List.init 5 (fun _ -> None) }

let defaultResult =
    ResponseHandlerResult<DomainState>.defaultValue (getInitialState [])

let respondFn (guesser: Guesser) (state: DomainState) (input: string) =
    try
        input
        |> JsonDocument.Parse
        |> convertToServerToClientMessage
        |> Option.map (guesser.handleRequest state)
        |> Option.defaultValue defaultResult
    with _ ->
        defaultResult

let initialMessageFn username =
    Some(fun () -> (Hello { northeastern_username = username }).serializeToJson ())
