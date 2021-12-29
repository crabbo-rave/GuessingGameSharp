open System
open System.Collections.Generic

let (|Parse|_|) (str: string) : int option =
    match Int32.TryParse(str) with
    | true, value -> Some value
    | _ -> None

let parseRange args =
    match args with
    | [| Parse range |] when range > 100 || range < 1 -> failwith "Error: Range must be between 1 and 100"
    | [| Parse range |] -> range
    | [||] -> failwith "Error: Provide range as a command line argument"
    | _ -> failwith "Error: Provide integer range"

let runGame range =
    let random = Random()
    let mutable count = 0
    let guesses = List<int>()
    let mutable hasWon = false
    let number = random.Next(range+1)
    Console.WriteLine $"OK, let's start. I am thinking of a number between 1 to {range}"
    Console.WriteLine "Enter your guess: "
    let mutable guess = 0
    let mutable Break = false
    while not Break do
        let userInput = Console.ReadLine()
        match userInput with
        | Parse input ->
            guess <- input
            if guesses.Contains guess then
                Console.WriteLine "You already guessed that!!"
            elif 0 >= guess || guess > range then
                Console.WriteLine $"Sorry, {guess} is less than or equal to zero or greater than {range}."
                count <- count - 1
            elif guess < number then
                Console.WriteLine $"{guess} is too low!"
            elif guess > number then
                Console.WriteLine $"{guess} is too high!"
            else
                hasWon <- true
                Break <- true
            guesses.Add(guess)
            count <- count + 1
            Console.WriteLine "Enter your guess: "
        | _ as input ->
            if input.Equals "give up" then
                hasWon <- false
                Break <- true
            else
                Console.WriteLine $"\"{userInput}\" should be an integer: "
    hasWon, number, count

let displayEndState (hasWon, number, count) =
    match hasWon with
    | true ->
        Console.Clear()
        Console.WriteLine $"Yay! you guessed my number!! It took you {count} tries! (Not counting bad inputs.)"
    | _ ->
        Console.Clear()
        Console.WriteLine $":( You gave up. The number was {number}. It took you {count} tries. (Not counting bad inputs.)"

[<EntryPoint>]
let main args =
    args
    |> parseRange
    |> runGame
    |> displayEndState
    0