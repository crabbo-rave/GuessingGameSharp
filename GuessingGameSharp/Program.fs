open System
open System.IO

type GameTermination =
    | Won
    | GaveUp

type GameState = {
    Guesses:Set<int>
    Target:int
    Count: int
    Termination: GameTermination option
}

let (|Parse|_|) (str: string) : int option =
    match Int32.TryParse(str) with
    | true, value -> Some value
    | _ -> None

let parseRange args =
    let parse = 
      function
      | Parse v -> v
      | v -> failwithf "Argument provided was not an int: '%s'" v
    
    let clamp (lower,upper) x = 
      if x < lower then failwithf "Argument must be greater than or equal to %i but was %i" lower x
      elif x > upper then failwithf "Argument must be less than or equal to %i but was %i" upper x
      else x

    Array.tryHead args
    |> Option.map (parse >> clamp (1,350))
    |> function
        | None -> failwith "Range was not provided"
        | Some x -> x

let runGame range : GameState =
    let random = Random()
    let number = random.Next(1, range+1)
    Console.WriteLine $"OK, let's start. I am thinking of a number between 1 to {range}"
    Console.WriteLine "Enter your guess: "
    {
        Guesses = Set.empty
        Target = number
        Count = 0
        Termination = None
    }
    |> Seq.unfold (fun gameState -> 
                match gameState.Termination with
                | Some _ -> None
                | None ->
                    Console.WriteLine "Enter your guess: "
                    let userInput = Console.ReadLine()
                    match userInput with
                    | Parse guess ->
                        let addGuess() = gameState.Guesses |> Set.add guess
                        let next =
                            if gameState.Guesses.Contains guess then
                                printfn "You already guessed that!"
                                gameState
                            elif 0 >= guess || guess > range then
                                printfn $"Sorry, {guess} is less than or equal to zero or greater than {range}."
                                gameState
                            elif guess < gameState.Target then
                                printfn $"{guess} is too low!"
                                { gameState with Guesses = addGuess(); Count = gameState.Count + 1 }
                            elif guess > gameState.Target then
                                printfn $"{guess} is too high!"
                                { gameState with Guesses = addGuess(); Count = gameState.Count + 1 }
                            else
                                { gameState with Termination = Some Won; Guesses = addGuess() }
                        Some(next,next) 
                    | "give up" ->
                        let next = { gameState with Termination = Some GaveUp }
                        Some(next,next)
                    | userInput ->
                            printfn $"\"{userInput}\" should be an integer: "
                            Some (gameState, gameState)
                )
    |> Seq.last

let displayEndState (state: GameState) =
    let cclear() =
        try
            Console.Clear()
        with | :? IOException as io -> eprintfn "Couldn't clear console: %s" io.Message
    cclear()
    match state.Termination with
    | Some termination ->
        match termination with
        | Won ->
            Console.WriteLine $"Yay! you guessed my number!! It took you {state.Count} tries! (Not counting bad inputs.)"
        | _ ->
            Console.WriteLine $":( You gave up. The number was {state.Target}. It took you {state.Count} tries. (Not counting bad inputs.)"
    | _ -> 
        failwith "Error processing termination"
        
[<EntryPoint>]
let main args =
    args
    |> parseRange
    |> runGame
    |> displayEndState
    0