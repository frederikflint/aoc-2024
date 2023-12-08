namespace aoc_library

open System.Text.RegularExpressions

module util =
    open System.IO
    open System.Text.RegularExpressions

    let readLinesFromFile (filePath: string) : string list =
        try
            // Use 'File.ReadAllLines' to read all lines from the file
            File.ReadAllLines("/Users/frederikoliverflint/Code/aoc-2024/" + filePath)
            |> List.ofArray
        with :? System.IO.FileNotFoundException as ex ->
            // Handle the case where the file is not found
            printfn "File not found: %s" ex.Message
            []

    let matchesFromRegex (rx: Regex) (input: string) =
        let m = fun (a: Match) -> a.Value
        rx.Matches(input) |> Seq.cast |> Seq.map m |> List.ofSeq
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
module aoc3 =
    open System
    open System.Text.RegularExpressions

    type P = Pos of int * int
    type NV = NumVal of int * P

    let numberRx = Regex("\d+")
    let symbolRx = Regex("[^(\d)\.\n]")

    let rec findIndices (number: string) (input: string) index =
        match input with
        | i when i.StartsWith number -> [index; index + number.Length - 1]
        | i -> findIndices number i[1 .. i.Length - 1] (index + 1)
        
    let charsToString (chars: char list) = String.Concat(Array.ofList(chars))
    let charToString (char: char) = String.Concat(Array.ofList([char]))
        
    let rec parseLineRec (line: string) accNums accSymbols index =
        match (Seq.toList line) with
        | a::b::c::tail when Char.IsDigit(a) && Char.IsDigit(b) && Char.IsDigit(c)
                             -> parseLineRec (charsToString tail) (accNums@[(charsToString [a;b;c], [index; index + 2])]) accSymbols (index + 3)
        | a::b::tail when Char.IsDigit(a) && Char.IsDigit(b)
                             -> parseLineRec (charsToString tail) (accNums@[(charsToString [a;b], [index; index + 1])]) accSymbols (index + 2)
        | a::tail when Char.IsDigit(a)
                             -> parseLineRec (charsToString tail) (accNums@[(charToString a, [index; index])]) accSymbols (index + 1)
        | a::tail when symbolRx.IsMatch (charToString a)
                             -> parseLineRec (charsToString tail) accNums (accSymbols@[(charToString a, index)]) (index + 1)
        | _::tail -> parseLineRec (charsToString tail) accNums accSymbols (index + 1)
        | [] -> (accNums, accSymbols)
    
    let parseLine (line: string) = parseLineRec line [] [] 0
    
    let parseInput (lines: string list) = List.map parseLine lines
    
    let lineContainSymbolWithinIndicesRange (index: int) (indices: int list) (maxRows: int) (symbols: (string * int list) list * (string * int) list) =
        match (index, symbols) with
        | (-1, _) -> false
        | (i, _) when i = maxRows -> false
        | (i, (_, [])) -> false
        | (i, (_, symbs)) -> List.exists (fun (_, i1) -> (List.contains i1 [(indices[0] - 1) .. (indices[1] + 1)])) symbs  
    
    let produceSolution lines =
        let parsedInput = parseInput lines
        let mutable validNums: string list = [];
               
        for i in [0..(parsedInput.Length - 1)] do
            let (numbers, symbols) = parsedInput[i]
            for j in [0..(numbers.Length - 1)] do
                let (number, indices) = numbers[j]
                                
                let (runPrev, runNext ) = match i with
                                            | 0 -> (false, true)
                                            | i when i = (parsedInput.Length - 1) -> (true, false)
                                            | _ -> (true, true)
                
                // Look up
                if runPrev && lineContainSymbolWithinIndicesRange (i - 1) indices parsedInput.Length parsedInput[i - 1] then 
                     validNums <- List.append validNums [number]
                     
                if lineContainSymbolWithinIndicesRange (i) indices parsedInput.Length parsedInput[i] then 
                     validNums <- List.append validNums [number]
                     
                if runNext && lineContainSymbolWithinIndicesRange (i + 1) indices parsedInput.Length parsedInput[i + 1] then 
                     validNums <- List.append validNums [number]
                               
        List.fold (fun acc num -> acc + int(num)) 0 validNums

    let getAdjacentNumbers (lineIndex: int) (symbolIndex: int) (maxIndex: int) (symbols: (string * int list) list * (string * int) list) =
        match (lineIndex, symbols) with
        | (-1, _) -> []
        | (i, _) when i = maxIndex -> []
        | (i, ([], _)) -> []
        | (i, (nums, _)) ->
            List.map (fun (num, _) -> num) (List.filter (fun (num, a::b::[]) ->
                let lowerB = a - 1
                let upperB = b + 1
                List.contains symbolIndex [lowerB..upperB]) nums)
    let produceSolution2 lines =
        let parsedInput = parseInput lines
        let mutable validNums: (string list) list = [];
               
        for i in [0..(parsedInput.Length - 1)] do
            let (numbers, symbols) = parsedInput[i]
            for j in [0..(symbols.Length - 1)] do
                let (symbol, index) = symbols[j]
                                
                let (runPrev, runNext) = match i with
                                            | 0 -> (false, true)
                                            | i when i = (parsedInput.Length - 1) -> (true, false)
                                            | _ -> (true, true)
                
                // Look up
                let mutable adjacentNums = []
                if runPrev then
                     adjacentNums <- adjacentNums@(getAdjacentNumbers (i - 1) index parsedInput.Length parsedInput[i - 1]) 
                
                adjacentNums <- adjacentNums@(getAdjacentNumbers (i) index parsedInput.Length parsedInput[i])     
                     
                if runNext then
                    adjacentNums <- adjacentNums@(getAdjacentNumbers (i+1) index parsedInput.Length parsedInput[i+1])
                    
                if adjacentNums.Length = 2 then
                    validNums <- validNums@[adjacentNums]
                     
                               
        List.fold (fun acc (n1::n2::_) -> acc + (int(n1) * int(n2))) 0 validNums
        
module aoc4 =
    
    let digitRx = Regex("\d+")
    
    let matchesToIntArray (matches: MatchCollection) =
        matches |> Seq.cast |> Seq.map (fun (m: Match) -> int(m.Value)) |> List.ofSeq
    
    let parseLine (line: string) =
        let numbersPart = (line.Split ":")[1]
        let winningNumbersPart = numbersPart.Split("|")[0]
        let yourNumbersPart = numbersPart.Split("|")[1]
        
        let winningNumbers = matchesToIntArray (digitRx.Matches(winningNumbersPart))
        let yourNumbers = matchesToIntArray (digitRx.Matches(yourNumbersPart))
        
        (winningNumbers, yourNumbers)
        
    let calculateLinePoints ((winning: int list), (yours: int list)): int =
        List.fold (fun acc num -> match ((List.contains num winning), acc) with
                                     | (false, _) -> acc
                                     | (true, 0) -> 1
                                     | (true, _) -> acc * 2) 0 yours
        
    let produceSolution lines =
        lines |> List.map (fun line -> calculateLinePoints (parseLine line)) |> List.sum
        
    let countWinningNumbers ((winning: int list), (yours: int list)) =
        (List.filter (fun num -> List.contains num winning) yours).Length
        
                    
    let rec calculate (input: (int list * int list) list) =
        match input with
        | [] -> 0
        | head::tail -> let winningNums = countWinningNumbers head
                        winningNums +
                        match winningNums with
                        | 0 -> calculate tail
                        | win -> + (calculateExtra tail[0..winningNums - 1] tail) + (calculate tail)    
                                      
    and calculateExtra (rem: (int list * int list) list) (all: (int list * int list) list) =
            match rem with
            | [] -> 0
            | (head::tail) -> calculate tail + calculate all
    let rec addGameNum (input: (int list * int list) list) (index: int) =
        match input with
        | head::tail -> (index, head)::(addGameNum tail (index + 1))
        | [] -> []
        
    let rec cloneGames (input: (int * (int list * int list)) list) (indexList: int list) =
        match (input, indexList) with
        | ([],_) -> []
        | (ls, []) -> ls
        | ((gameNr, game)::rest, firstIndex::restIndex) when gameNr = firstIndex ->
            let game = (gameNr, game)
            game::game::(cloneGames rest restIndex)
        | (game::rest, indices) -> game::(cloneGames rest indices)
    let produceSolution2 lines =
        let parsedLines = addGameNum (lines |> List.map parseLine) 1
        
        let mutable keepRunning = true
        let mutable games = parsedLines
        let mutable index = 0
        
        while keepRunning do
            let gameNr, game = games[index]
            let winningNumbers = countWinningNumbers game
            
            if winningNumbers > 0 then
                games <- cloneGames games [gameNr+1..gameNr+winningNumbers]
                
            index <- index + 1    
            keepRunning <- games.Length > index
        
        games.Length
        
        
        
        
        
        
        