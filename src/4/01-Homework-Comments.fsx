// -   [ ] Bank Account
// -   [ ] Accumulate
// -   [ ] Space Age

// Allgemein

// - Publish on exercism.io
// - Formatierung (dotnet fantomas)
// - Vermeide "mutable"!!
// - nur wichtiges verdient einen Namen
// - Vertraue der "Pipe"!!

// - If-Then-Else mit Boolean ist unnötig
// - Parametrisiere!

// Bank Account

module BankAccount =

    type OpenAccount =
        { mutable Balance: decimal }

    type Account =
        | Closed
        | Open of OpenAccount

    let mkBankAccount() = Closed

    let openAccount account =
        match account with
        | Closed -> Open { Balance = 0.0m }
        | Open _ -> failwith "Account is already open"

    let closeAccount account =
        match account with
        | Open _ -> Closed
        | Closed -> failwith "Account is already closed"

    let getBalance account =
        match account with
        | Open openAccount -> Some openAccount.Balance
        | Closed -> None

    let updateBalance change account =
        match account with
        | Open openAccount ->
            lock (openAccount) (fun _ ->
                openAccount.Balance <- openAccount.Balance + change
                Open openAccount)
        | Closed -> failwith "Account is closed"

    // for lock see: https://technet.microsoft.com/en-ca/ee370413(v=vs.90) 
    let updateBalance0 change account =
        match account with
        | Open openAccount ->
            openAccount.Balance <- openAccount.Balance + change
            Open openAccount
        | Closed -> failwith "Account is closed"

    // Test!!
    let ``Account can be updated from multiple threads`` updateFn =
        let account = 
            mkBankAccount()
            |> openAccount

        let updateAccountAsync =        
            async {                             
                account 
                |> updateFn 1.0m
                |> ignore
            }

        updateAccountAsync
        |> List.replicate 1000
        |> Async.Parallel 
        |> Async.RunSynchronously
        |> ignore

        getBalance account //= (Some 1000.0m)

    ``Account can be updated from multiple threads`` updateBalance
    ``Account can be updated from multiple threads`` updateBalance0

// Accumulate

module Accumulate =
    let rec accumulateR func input acc = 
        match input with
        | [] -> acc |> List.rev
        | head::tail -> accumulateR func tail (func head :: acc)
    let accumulate func input = accumulateR func input []

module Accumulate2 = 
    let accumulate (inputFunc: 'a -> 'b) (input: 'a list): 'b list =
        [ for x in input do
            yield inputFunc x ]

// Space Age

module SpaceAge = 
    open System
    type Planet = 
        | Mercury
        | Venus
        | Earth
        | Mars
        | Jupiter
        | Saturn
        | Uranus
        | Neptune
    let orbitalPeriodRelativeToEarthOn planet = 
        match planet with
        | Mercury -> 0.2408467
        | Venus -> 0.61519726
        | Earth -> 1.0
        | Mars -> 1.8808158
        | Jupiter -> 11.862615
        | Saturn -> 29.447498
        | Uranus -> 84.016846
        | Neptune -> 164.79132
    [<Literal>]
    let SecondsInOneEarthYear = 31557600.0
    let secondsInAYearOn planet =
        SecondsInOneEarthYear * orbitalPeriodRelativeToEarthOn planet
    let round (number : float) = Math.Round(number, 2)
    let age (planet: Planet) (seconds: int64): float = 
        float seconds / (secondsInAYearOn planet)
        |> round

module SpaceAge2 = 
    type Planet(name: string, day: float) =
        member this.name = name
        member this.day = day * 31557600.0
    let Mercury = Planet("Mercury", 0.2408467)
    let Venus = Planet("Venus", 0.61519726)
    let Earth = Planet("Earth", 1.0)
    let Mars = Planet("Mars", 1.8808158)
    let Jupiter = Planet("Jupiter", 11.862615)
    let Saturn = Planet("Saturn", 29.447498)
    let Uranus = Planet("Uranus", 84.016846)
    let Neptune = Planet("Neptune", 164.79132)
    let age (planet: Planet) (seconds: int64): float = float (seconds) / planet.day
