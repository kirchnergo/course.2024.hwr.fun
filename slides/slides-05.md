

# Ziel 


## Programm

-   Programmieraufgabe
-   Test
-   Parser (Kombinatoren)


# Programmieraufgabe 


## Poker

    type Rank =
         | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten 
         | Jack | Queen | King | Ace
    type HandCategory =
         | HighCard of Rank * Rank * Rank * Rank * Rank
         | OnePair of Rank * Rank * Rank * Rank
         | TwoPair of Rank * Rank * Rank
         | ThreeKind of Rank * Rank
         | Straight of Rank
         | Flush of Rank
         | FullHouse of Rank * Rank
         | FourKind of Rank * Rank
         | StraightFlush of Rank
         | RoyalFlush


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


# Test 


## Test

-   60 Minuten

$\leadsto$ [Test](../src/test/test.md)


# Parser 


## Parser 1 (hard-coded character)

    open System
    let A_Parser str =
        if String.IsNullOrEmpty(str) then
            (false,"")
        else if str.[0] = 'A' then
            let remaining = str.[1..]
            (true,remaining)
        else
            (false,str)
    let inputABC = "ABCD"
    let inputZBC = "ZBCD"
    let test11 = A_Parser inputABC
    let test12 = A_Parser inputZBC

    val A_Parser: str: string -> bool * string
    val inputABC: string = "ABCD"
    val inputZBC: string = "ZBCD"
    val test11: bool * string = (true, "BCD")
    val test12: bool * string = (false, "ZBCD")


## Parser 2 (match a specified character)

    let pchar (charToMatch,str) =
        if String.IsNullOrEmpty(str) then
            let msg = "No more input"
            (msg,"")
        else 
            let first = str.[0] 
            if first = charToMatch then
                let remaining = str.[1..]
                let msg = sprintf "Found %c" charToMatch
                (msg,remaining)
            else
                let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
                (msg,str)

    val pchar: charToMatch: char * str: string -> string * string


## Parser 2 (2)

    let inputABC = "ABCD"
    let inputZBC = "ZBCD"
    let test21 = pchar('A',inputABC) 
    let test22 = pchar('A',inputZBC)

    val inputABC: string = "ABCD"
    val inputZBC: string = "ZBCD"
    val test21: string * string = ("Found A", "BCD")
    val test22: string * string = ("Expecting 'A'. Got 'Z'", "ZBCD")


## Parser 3 (return a Result)

    let pchar (charToMatch, s) =
        if String.IsNullOrEmpty(s) then
            Error "No more input"
        else
            let first = s.[0]
            if first = charToMatch then
                let remaining = s.[1..]
                Ok (charToMatch, remaining)
            else
                let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
                Error msg

    val pchar: charToMatch: char * s: string -> Result<(char * string),string>


## Parser 3 (2)

    let test31 = pchar('A',inputABC) 
    let test32 = pchar('A',inputZBC) 
    let test33 = pchar('Z',inputZBC)

    val test31: Result<(char * string),string> = Ok ('A', "BCD")
    val test32: Result<(char * string),string> = Error "Expecting 'A'. Got 'Z'"
    val test33: Result<(char * string),string> = Ok ('Z', "BCD")


## Parser 4 (use currying)

    let pchar charToMatch str = 
        if String.IsNullOrEmpty(str) then
            Error "No more input"
        else
            let first = str.[0] 
            if first = charToMatch then
                let remaining = str.[1..]
                Ok (charToMatch,remaining)
            else
                let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
                Error msg

    val pchar: charToMatch: char -> str: string -> Result<(char * string),string>


## Parser 4 (2)

    let parseA = pchar 'A'
    let inputABC = "ABC"
    let inputZBC = "ZBC"
    let test41 = parseA inputABC
    let test42 = parseA inputZBC
    let parseZ = pchar 'Z' 
    let test43 = parseZ inputZBC

    val parseA: (string -> Result<(char * string),string>)
    val inputABC: string = "ABC"
    val inputZBC: string = "ZBC"
    val test41: Result<(char * string),string> = Ok ('A', "BC")
    val test42: Result<(char * string),string> = Error "Expecting 'A'. Got 'Z'"
    val parseZ: (string -> Result<(char * string),string>)
    val test43: Result<(char * string),string> = Ok ('Z', "BC")


## Parser 5 (type to wrap the parser function)

    type Parser<'T> =
        | Parser of (string -> Result<'T , string>)
    let pchar charToMatch = 
        let innerFn str =
            if String.IsNullOrEmpty(str) then
                Error "No more input"
            else
                let first = str.[0] 
                if first = charToMatch then
                    let remaining = str.[1..]
                    Ok (charToMatch, remaining)
                else
                    let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
                    Error msg
        Parser innerFn

    type Parser<'T> = | Parser of (string -> Result<'T,string>)
    val pchar: charToMatch: char -> Parser<char * string>


## Parser 5 (2)

    let parseA = pchar 'A'
    let inputABC = "ABC"
    parseA inputABC

    
      parseA inputABC;;
      ^^^^^^
    
    error FS0003: This value is not a function and cannot be applied.


## Parser 5 (3)

    let run parser input = 
        let (Parser innerFn) = parser 
        innerFn input
    let parseA = pchar 'A' 
    let inputABC = "ABC"
    let test1 = run parseA inputABC
    let inputZBC = "ZBC"
    let test2 = run parseA inputZBC

    val run: parser: Parser<'a> -> input: string -> Result<'a,string>
    val parseA: Parser<char * string> = Parser <fun:pchar@238-14>
    val inputABC: string = "ABC"
    val test1: Result<(char * string),string> = Ok ('A', "BC")
    val inputZBC: string = "ZBC"
    val test2: Result<(char * string),string> = Error "Expecting 'A'. Got 'Z'"


# Parser Kombinatoren 


## Understanding Parser Combinators

$\leadsto$ [Understanding parser combinators](./5 Understanding parser combinators.pdf) (Scott Wlashin)

\null\hfill&#x2013;Scott Wlashin: [F# for Fun and Profit](https://fsharpforfunandprofit.com/parser)


## FParsec Tutorial

-   [FParsec Tutorial](http://www.quanttec.com/fparsec/tutorial.html#)
-   [User’s Guide](http://www.quanttec.com/fparsec/users-guide/)
-   [FParsec vs alternatives](http://www.quanttec.com/fparsec/about/fparsec-vs-alternatives.html)


## Using FParsec (1)

    #r "../src/5/02-fparsec/lib/FParsecCS.dll";; 
    #r "../src/5/02-fparsec/lib/FParsec.dll";;
    open FParsec
    let test p str =
        match run p str with
        | Success(result, _, _)   -> printfn "Success: %A" result
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg;;
    test pfloat "1.25"
    test pfloat "1.25E 2"

    test pfloat "1.25"
    test pfloat "1.25E 2";;
    Success: 1.25                           
    Failure: Error in Ln: 1 Col: 6
    1.25E 2
         ^
    Expecting: decimal digit
    
    val it: unit = ()


## Using FParsec (2)

    let str s = pstring s
    let floatBetweenBrackets:Parser<float, unit>  = str "[" >>. pfloat .>> str "]";;
    
    test floatBetweenBrackets "[1.0]"
    test floatBetweenBrackets "[]"
    test floatBetweenBrackets "[1.0]"

    test floatBetweenBrackets "[1.0]"
    test floatBetweenBrackets "[]"
    test floatBetweenBrackets "[1.0]";;
    Success: 1.0                            
    Failure: Error in Ln: 1 Col: 2
    []
     ^
    Expecting: floating-point number
    
    Success: 1.0                            
    val it: unit = ()


## Using FParsec (3)

    let betweenStrings s1 s2 p = str s1 >>. p .>> str s2;;
    let floatBetweenBrackets_:Parser<float, unit> = pfloat |> betweenStrings "[" "]";;
    let floatBetweenDoubleBrackets_:Parser<float, unit> = pfloat |> betweenStrings "[[" "]]";;
    test floatBetweenBrackets_ "[1.0]"
    test floatBetweenDoubleBrackets_ "[[1.0]]"
    let between_ pBegin pEnd p  = pBegin >>. p .>> pEnd;;
    let betweenStrings_ s1 s2 p = p |> between_ (str s1) (str s2);;
    test (many floatBetweenBrackets) ""
    test (many floatBetweenBrackets) "[1.0]"
    test (many floatBetweenBrackets) "[2][3][4]"
    test (many floatBetweenBrackets) "[1][2.0E]"

    Success: []
    Success: [1.0]
    Success: [2.0; 3.0; 4.0]
    Failure: Error in Ln: 1 Col: 9
    [1][2.0E]
            ^
    Expecting: decimal digit
    
    val it: unit = ()


# Ende 


## Zusammenfassung (Kurs)

-   Wichtige Werkzeuge (git, dotnet, code)
-   Elementare Syntax
-   Funktionen, Pattern Matching, Discriminated Unions (DU)
-   Tuple, Record, List, Array, Seq
-   funktionale Operationen auf Listen (Tail-Rekursion)
-   funktionaler Umgang mit fehlenden Daten (Option)
-   funktionaler Umgang mit Fehlern (Result)
-   funktionales Design (statt Patterns: Funktionen & Verkettung)
-   funktionales Refactoring
-   funktionales Domain Modeling (DDD)
-   eigenschaftsbasiertes Testen (Property Based Testing) (cool!!)
-   funktionale Parser (Kombinatoren) (noch cooler!!)

$\leadsto$ ****Was ist Funktionale Programmierung?****


## Links

-   [fsharp.org](https://fsharp.org/)
-   [docs.microsoft.com/../dotnet/fsharp](https://docs.microsoft.com/de-de/dotnet/fsharp/)
-   [F# weekly](https://sergeytihon.com/)
-   [fsharpforfunandprofit.com](https://fsharpforfunandprofit.com/)
-   [github.com/../awesome-fsharp](https://github.com/fsprojects/awesome-fsharp)


## Ende

-   Wie geht es weiter?
-   [Exercism!](https://exercism.io)
-   Buchtipps
    -   [Domain Modeling Made Functional](https://pragprog.com/book/swdddf/domain-modeling-made-functional) (F#)
    -   [Stylish F#](https://www.apress.com/gp/book/9781484239995) (F#)
    -   [Perls of Functional Algorithm Design](https://www.cambridge.org/core/books/pearls-of-functional-algorithm-design/B0CF0AC5A205AF9491298684113B088F#) (Haskell)
    -   [Thinking Functional with Haskell](https://www.cs.ox.ac.uk/publications/books/functional/) (Haskell)
    -   [On Lisp](http://www.paulgraham.com/onlisp.html) (LISP)
    -   [Funktionale Programmierung und Metaprogrammierung](http://www.iqool.de/FPMP.html) (LISP)
    -   [Paradigms of Artificial Intelligence Programming](https://github.com/norvig/paip-lisp) (LISP)
    -   [Advanced R](https://adv-r.hadley.nz/) (R)
-   Sprachen: [R](https://www.r-project.org/), [Haskell](https://www.haskell.org/), [Clojure](https://clojure.org/), [Common Lisp](https://lisp-lang.org/), [Elixir](https://elixir-lang.org/), [q](https://code.kx.com/q/)

-   ****Have FUN!****

