namespace aoc_library

module util =
    open System.IO

    let readLinesFromFile (filePath: string) : string list =
        try
            // Use 'File.ReadAllLines' to read all lines from the file
            File.ReadAllLines("/Users/frederikoliverflint/Code/aoc-2024/" + filePath)
            |> List.ofArray
        with :? System.IO.FileNotFoundException as ex ->
            // Handle the case where the file is not found
            printfn "File not found: %s" ex.Message
            []


module aoc1 =
    open System.Text.RegularExpressions

    let basic = Regex("\d", RegexOptions.Compiled)

    let ltr =
        Regex("one|two|three|four|five|six|seven|eight|nine|\d", RegexOptions.Compiled)

    let rtl =
        Regex("one|two|three|four|five|six|seven|eight|nine|\d", RegexOptions.RightToLeft)

    let rec getTail =
        function
        | [] -> failwith "empty list in getTail"
        | a :: [] -> a
        | _ :: rest -> getTail rest

    let toDigit =
        function
        | "1" -> 1
        | "2" -> 2
        | "3" -> 3
        | "4" -> 4
        | "5" -> 5
        | "6" -> 6
        | "7" -> 7
        | "8" -> 8
        | "9" -> 9
        | "one" -> 1
        | "two" -> 2
        | "three" -> 3
        | "four" -> 4
        | "five" -> 5
        | "six" -> 6
        | "seven" -> 7
        | "eight" -> 8
        | "nine" -> 9
        | _ -> failwith "failed parsing"

    let findDigitsInLine (input: list<char>) withSpelledWords =
        let charsAsString = List.fold (fun acc state -> acc + state.ToString()) "" input
        let m = fun (a: Match) -> a.Value

        match withSpelledWords with
        | true ->
            let ltr =
                ltr.Matches(charsAsString)
                |> Seq.cast
                |> Seq.map m
                |> Seq.map toDigit
                |> List.ofSeq

            let rtl =
                rtl.Matches(charsAsString)
                |> Seq.cast
                |> Seq.map m
                |> Seq.map toDigit
                |> List.ofSeq

            ltr[0 .. ltr.Length - 2] @ [ rtl.Head ]
        | false ->
            basic.Matches(charsAsString)
            |> Seq.cast
            |> Seq.map m
            |> Seq.map toDigit
            |> List.ofSeq

    let decryptLine line withSpelledWords =
        match (findDigitsInLine line withSpelledWords) with
        | x :: [] -> [ x ]
        | x :: rest -> [ x; (getTail (rest)) ]
        | [] -> failwith "empty list in decryptLine"

    let produceSolution (input: 'a list) withSpelledWords =
        List.fold
            (fun acc line ->
                let a = decryptLine (Seq.toList line) withSpelledWords

                match a with
                | x :: y :: [] -> acc + (x * 10) + y
                | x :: [] -> acc + (x * 10) + x
                | _ -> failwith "Not supposed to happen")
            0
            input

module aoc2 =
    open System.Text.RegularExpressions
    type Game = Set<int * int * int> // (blue, green, red)
    let bluematch = Regex("\d+ blue(,?)")
    let redmatch = Regex("\d+ red(,?)")
    let greenmatch = Regex("\d+ green(,?)")

    let extractCounts (regex: Regex) (line: string) : List<string> =
        regex.Matches(line)
        |> Seq.cast
        |> Seq.map (fun (a: Match) -> a.Value.Split(" ")[0])
        |> List.ofSeq

    let composeGame (line: string) : Game =
        Array.map
            (fun set ->
                let blueMatch = extractCounts bluematch set
                let redMatch = extractCounts redmatch set
                let greenMatch = extractCounts greenmatch set

                let blueCount = List.fold (fun acc count -> acc + int (count)) 0 blueMatch
                let redCount = List.fold (fun acc count -> acc + int (count)) 0 redMatch
                let greenCount = List.fold (fun acc count -> acc + int (count)) 0 greenMatch

                (blueCount, greenCount, redCount))
            (line.Split(";"))
        |> Set.ofArray

    let extractMaxFromGame (game: Game) =
        Set.fold
            (fun (blue, green, red) (bl, gr, rd) ->
                let newBlue = max blue bl
                let newGreen = max green gr
                let newRed = max red rd
                (newBlue, newGreen, newRed))
            (0, 0, 0)
            game

    let produceGenericSolution (lines: string list) accumulatorFn =
        let games = List.map composeGame lines
        let gameMaxes = List.map extractMaxFromGame games

        let (sum, index) = List.fold accumulatorFn (0, 1) gameMaxes

        sum

    let produceSolution1 (lines: string list) (blMax, grMax, rdMax) : int =
        produceGenericSolution lines (fun (acc, index) tup ->
            match tup with
            | (bl, gr, rd) when bl <= blMax && gr <= grMax && rd <= rdMax -> (acc + index, index + 1)
            | _ -> (acc, index + 1))

    let produceSolution2 (lines: string list) : int =
        produceGenericSolution lines (fun (acc, index) (bl, gr, rd) -> (acc + (bl * gr * rd), index + 1))
