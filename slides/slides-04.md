

# Ziel 


## Programm

-   Hausaufgaben (8..10/10)
    -   [X] Bank Account
    -   [X] Accumulate
    -   [X] Space Age
-   Domain Driven Design (DDD)
-   Property Based Testing


# Hausaufgaben 


## Bank Account (1)

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

    type OpenAccount =
      { mutable Balance: decimal }
    type Account =
      | Closed
      | Open of OpenAccount
    val mkBankAccount: unit -> Account
    val openAccount: account: Account -> Account


## Bank Account (2)

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


## Bank Account (3)

    let account = mkBankAccount() |> openAccount
    let updateAccountAsync =        
        async { account |> updateBalance 1.0m |> ignore }
    let ``updated from multiple threads`` =
        updateAccountAsync
            |> List.replicate 1000
            |> Async.Parallel 
            |> Async.RunSynchronously
            |> ignore
    let test1 = getBalance account = (Some 1000.0m)


## Accumulate

    let rec accumulateR func input acc = 
        match input with
        | [] -> acc |> List.rev
        | head::tail -> accumulateR func tail (func head :: acc)
    let accumulate func input = accumulateR func input []
    let test1 = accumulate (fun x -> x * x) [1; 2; 3]
    let test2 = accumulate (fun (x:string) -> x.ToUpper()) ["hello"; "world"]

    val accumulateR: func: ('a -> 'b) -> input: 'a list -> acc: 'b list -> 'b list
    val accumulate: func: ('a -> 'b) -> input: 'a list -> 'b list
    val test1: int list = [1; 4; 9]
    val test2: string list = ["HELLO"; "WORLD"]


## Space Age

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


## Space Age (II)

    open System
    [<Literal>]
    let SecondsInOneEarthYear = 31557600.0
    let secondsInAYearOn planet =
        SecondsInOneEarthYear * orbitalPeriodRelativeToEarthOn planet
    let round (number : float) = Math.Round(number, 2)
    let age (planet: Planet) (seconds: int64): float =
        float seconds / (secondsInAYearOn planet)
        |> round
    let test1 = age Earth 1000000000L

    [<Literal>]
    val SecondsInOneEarthYear: float = 31557600
    val secondsInAYearOn: planet: Planet -> float
    val round: number: float -> float
    val age: planet: Planet -> seconds: int64 -> float
    val test1: float = 31.69


## Zusammenfassung

-   nutze [exercism.io](https://exercism.io)!
-   Vermeide `mutable`!!
-   nur wichtiges verdient einen Namen
-   Vertraue der **Pipe** (`>>`, `|>`, &#x2026;)!!
-   If-Then-Else mit Boolean ist unnötig
-   Parametrisiere!
-   If-Then-Else vermeiden &#x2026; besser `match`!
-   Be lazy! (vermeide `for`-loops)
-   [Troubleshooting F#](https://fsharpforfunandprofit.com/troubleshooting-fsharp/)
-   [F#-Styleguide](https://docs.microsoft.com/de-de/dotnet/fsharp/style-guide/)


## Pause

1.  

    You’re bound to be unhappy if you optimize everything.
    
    \null\hfill &#x2013; Donald Knuth


# DDD (Domain Driven Design) 


## DDD

$\leadsto$ [Domain Driven Design](./4.1 Dmmf.pdf)

\null\hfill&#x2013;Scott Wlashin: [F# for Fun and Profit](https://fsharpforfunandprofit.com/series/designing-with-types/)


## Prinzipien

-   Verwende die Sprache der Domäne (ubiquitous Language)
-   Values und Entities
-   der Code ist das Design (kein UML nötig)
-   Design mit (algebraischen) Typen
    -   Option statt Null
    -   DU statt Vererbung
-   illegale Konstellationen sollten nicht repräsentierbar sein!


## Pause

1.  

    Are you quite sure that all those bells and whistles, all those wonderful facilities of your so called powerful programming languages, belong to the solution set rather than the problem set?
    
    \null\hfill &#x2013; Edsger Dijkstra


## DDD Übung 1 (Contacts &#x2013; ex 2)

A Contact has

-   a personal name
-   an optional email address
-   an optional postal address
-   Rule: a contact must have an email or a postal address

A Personal Name consists of a first name, middle initial, last name

-   Rule: the first name and last name are required
-   Rule: the middle initial is optional
-   Rule: the first name and last name must not be more than 50 chars
-   Rule: the middle initial is exactly 1 char, if present

A postal address consists of a four address fields plus a country

-   Rule: An Email Address can be verified or unverified


## DDD Übung 2 (Payments &#x2013; ex 3)

The payment taking system should accept:

-   Cash
-   Credit cards
-   Cheques
-   Paypal
-   Bitcoin

A payment consists of a:

-   payment
-   non-negative amount

After designing the types, create functions that will:

-   print a payment method
-   print a payment, including the amount
-   create a new payment from an amount and method


## DDD Übung 3 (Refactoring &#x2013; ex 4)

Much C# code has implicit states that you can recognize by fields called "IsSomething", or nullable date.

This is a sign that states transitions are present but not being modelled properly.


## DDD Übung 4 (Shopping Cart &#x2013; fsm ex 3)

Create types that model an e-commerce shopping cart.

-   Rule: "You can't remove an item from an empty cart"!
-   Rule: "You can't change a paid cart"!
-   Rule: "You can't pay for a cart twice"!

States are:

-   Empty
-   ActiveCartData
-   PaidCartData


## Pause

1.  

    About the use of language: it is impossible to sharpen a pencil with a blunt axe. 
    It is equally vain to try to do it with ten blunt axes instead.
    
    \null\hfill &#x2013; Edsger Dijkstra


# Property Based Testing 


## Example Based Tests :)

    module Test1 =
        open Implementation1
        let tests = testList "implementation 1" [
            test "add 1 3 = 4" {
                let actual = add 1 3
                let expected = 4
                Expect.equal expected actual "" }
            test "add 2 2 = 4" {
                let actual = add 2 2
                let expected = 4
                Expect.equal expected actual "" } ];;
    runTests expectoConfig Test1.tests

    [23:23:18 INF] EXPECTO? Running tests... <Expecto>
    [23:23:18 INF] EXPECTO! 2 tests run in 00:00:00.0117930 for implementation 1 – 2 passed, 0 ignored, 0 failed, 0 errored. Success! <Expecto>
    val it: int = 0


## Evil Developer From Hell :(

    module Implementation1 =
        let add x y =
            4

    module Implementation1 =
      val add: x: 'a -> y: 'b -> int


## PBT

$\leadsto$ [Property Based Testing](./4.2 An introduction to property based testing.pdf)

\null\hfill&#x2013;Scott Wlashin: [F# for Fun and Profit](https://fsharpforfunandprofit.com/series/property-based-testing/)


## FsCheck

    let add1 x y = x + y
    let add2 x y = x - y
    let commutativeProperty f x y =
       let result1 = f x y
       let result2 = f y x
       result1 = result2;;
    FsCheck.Check.Quick (commutativeProperty add1)
    FsCheck.Check.Quick (commutativeProperty add2)

    FsCheck.Check.Quick (commutativeProperty add1)
    FsCheck.Check.Quick (commutativeProperty add2);;
    Ok, passed 100 tests.
    Falsifiable, after 2 tests (2 shrinks) (StdGen (905515291, 297320661)):
    Original:
    1
    -1
    Shrunk:
    0
    1
    val it: unit = ()


## FsCheck (Generate)

    type Temp = F of int | C of float;;
    let fGen =
        FsCheck.Gen.choose(32,212)
        |> FsCheck.Gen.map (fun i -> F i);;
    let cGen =
        FsCheck.Gen.choose(0,100)
        |> FsCheck.Gen.map (fun i -> C (float i));;
    let tempGen =
        FsCheck.Gen.oneof [fGen; cGen]
    
    let test = tempGen |> FsCheck.Gen.sample 0 20
    test

    let tempGen =
        FsCheck.Gen.oneof [fGen; cGen]
    
    let test = tempGen |> FsCheck.Gen.sample 0 20
    test;;
    val tempGen: Gen<Temp> = Gen <fun:Bind@88>
    val test: Temp list =
      [F 185; F 153; F 130; F 98; C 16.0; C 6.0; C 26.0; C 16.0; F 146; F 114;
       F 91; F 59; C 56.0; C 46.0; C 66.0; C 56.0; C 76.0; F 75; F 52; F 201]
    val it: Temp list =
      [F 185; F 153; F 130; F 98; C 16.0; C 6.0; C 26.0; C 16.0; F 146; F 114;
       F 91; F 59; C 56.0; C 46.0; C 66.0; C 56.0; C 76.0; F 75; F 52; F 201]


## FsCheck (Shrink)

    open FsCheck
    let smallerThan81Property x = x < 81
    FsCheck.Check.Quick smallerThan81Property
    
    let test1 = FsCheck.Arb.shrink 100 |> Seq.toList
    let test2 = FsCheck.Arb.shrink 88 |> Seq.toList
    test2

    Ok, passed 100 tests.
    val smallerThan81Property: x: int -> bool
    val test1: int list = [0; 50; 75; 88; 94; 97; 99]
    val test2: int list = [0; 44; 66; 77; 83; 86; 87]
    val it: int list = [0; 44; 66; 77; 83; 86; 87]


## Auswahl der Eigenschaften

-   Unterschiedlicher Weg, gleiches Ziel (Map(f)(Option(x))=Option(f x))
-   Hin und Her (z.B. Reverse einer Liste)
-   Invarianten (z.B. Länge einer Liste bei Sortierung)
-   Idempotenz (noch einmal bringt nichts mehr)
-   Divide et Impera! (teile und herrsche)
-   Hard to prove, easy to verify (Primzahlzerlegung)
-   Test-Orakel (z.B. einfach aber langsam)


# Ende 


## Zusammenfassung

-   funktionales Domain Modeling (DDD)
-   eigenschaftsbasiertes Testen (Property Based Testing)


## Links

-   [Domain Driven Design](https://fsharpforfunandprofit.com/ddd/)
-   [Domain Modeling Made Functional](https://fsharpforfunandprofit.com/books/)
-   [FsCheck](https://github.com/fscheck/FsCheck)
-   [An introduction to property-based testing](https://fsharpforfunandprofit.com/posts/property-based-testing/)
-   [Choosing properties for property-based testing](https://fsharpforfunandprofit.com/posts/property-based-testing-2/)

