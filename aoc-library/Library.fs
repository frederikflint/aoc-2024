namespace aoc_library

open System.Text.RegularExpressions
open Microsoft.FSharp.Collections

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
        
        
    let rec calculata (input: (int list * int list) list) index =
        match (input, index) with
        | [], _ -> 0
        | _, index when index = input.Length -> 0
        | arr, index -> let head = arr[index]
                        let winningNums = countWinningNumbers head
                        winningNums +
                        match winningNums with
                        | 0 -> calculata arr (index + 1)
                        | win -> + List.fold (fun acc num -> acc + calculata arr num) 0 [(index + 1)..(index+win)] + (calculata arr (index + 1))    
                    
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
        
    let countIndices (input: (int * (int list * int list)) list) (gameNr: int) = (List.filter (fun (a,_) -> a = gameNr) input).Length
        
        
    let rec cloneGames (input: (int * (int list * int list)) list) (indexList: int list) (cloneAmount: int) =
        match (input, indexList) with
        | ([],_) -> []
        | (ls, []) -> ls
        | ((gameNr, game)::rest, firstIndex::restIndex) when gameNr = firstIndex ->
            let game = (gameNr, game)
            let clonedGames = List.map (fun _ -> game) [1..cloneAmount]
            let indexToSkip = countIndices rest gameNr
            game::rest[0..indexToSkip - 1]@clonedGames@(cloneGames (rest[indexToSkip..rest.Length - 1]) restIndex cloneAmount)
        | (game::rest, indices) -> game::(cloneGames rest indices cloneAmount) 
    let produceSolution2 lines =
        let parsedLines =  (lines |> List.map parseLine)
        
        let gamess = addGameNum parsedLines 1
        
        // let test = calculata parsedLines 0
                
        let mutable keepRunning = true
        let mutable games = gamess
        let mutable index = 0
        let mutable amount = 1
        
        while keepRunning do
            let gameNr, game = games[index]
            let winningNumbers = countWinningNumbers game
            
            amount <- countIndices games gameNr
            if winningNumbers > 0 then
                
                let former = games[0..(index + amount) - 1]
                let future = cloneGames games[(index + amount)..games.Length - 1] [gameNr+1..gameNr+winningNumbers] amount
                games <- former@future
                
            index <- index + amount
            amount <- 1
            keepRunning <- games.Length > index
        
        games.Length
        
module aoc5 =
    open Microsoft.FSharp.Core.Operators.Checked

    let parseSeeds (input: string) =
        let numbersString = input.Split(": ")[1]
        let numbers = List.map(fun str -> int64(str)) (List.ofArray (numbersString.Split(" ")))
        numbers
    let findIndices (startPhrase: string) (endPhrase: string) (collection: string list) =
        let startI = List.findIndex (fun (str: string) -> str.StartsWith(startPhrase)) collection
        let endI = List.findIndex (fun (str: string) -> str.StartsWith(endPhrase)) collection
        [startI + 1; endI - 2]
        
    let createMapping (lines: string list) = List.map (fun (line: string) -> let dest::source::size::[] = (List.ofArray (line.Split(" ")))
                                                                             (int64(source), int64(source) + int64(size) - int64(1), int64(dest) - int64(source))) lines
        
    // let rec createMapping (lines: string list) (acc: (int64 * int64) list) =
    //     match lines with
    //     | [] -> Map.ofList acc
    //     | h::t -> let numbers = List.map (fun str -> int64(str)) (List.ofArray (h.Split(" ")))
    //               let mapping = List.fold (fun (acc: (int64 * int64) list) (inp: int64) -> acc@[(numbers[1] + inp, numbers[0] + inp)]) [] [0..numbers[2] - int64(1)]
    //               createMapping t (acc@mapping)
                  
    let tryFind (map: (int64 * int64 * int64) list) (index: int64)  =
        match (List.tryFind (fun (start, eNd, _) -> start <= index && index <= eNd) map) with
        | Some(_, _, delta) -> index + delta
        | None -> index
        
    let produceSolution (input: string list) (seedsFunc: string -> int64 list) =
        let seedSoilStr = "seed-to-soil map:"
        let soilFertStr = "soil-to-fertilizer map:"
        let fertWaterStr = "fertilizer-to-water map:"
        let waterLightStr = "water-to-light map:"
        let lightTempStr = "light-to-temperature map:"
        let tempHumStr = "temperature-to-humidity map:"
        let humLocStr = "humidity-to-location map:"
        
        let seeds = seedsFunc input[0]
        let seedSoilInds = findIndices seedSoilStr soilFertStr input
        let soilFertInds = findIndices soilFertStr fertWaterStr input
        let fertWaterInds = findIndices fertWaterStr waterLightStr input
        let waterLightInds = findIndices waterLightStr lightTempStr input
        let lightTempInds = findIndices lightTempStr tempHumStr input
        let tempHumInds = findIndices tempHumStr humLocStr input
        let humLocInds = [tempHumInds[tempHumInds.Length - 1] + 3; input.Length - 1]
        
        let seedSoilMap = createMapping input[seedSoilInds[0]..seedSoilInds[1]]
        let soilFertMap = createMapping input[soilFertInds[0]..soilFertInds[1]] 
        let fertWaterMap = createMapping input[fertWaterInds[0]..fertWaterInds[1]] 
        let waterLightMap = createMapping input[waterLightInds[0]..waterLightInds[1]] 
        let lightTempMap = createMapping input[lightTempInds[0]..lightTempInds[1]] 
        let tempHumMap = createMapping input[tempHumInds[0]..tempHumInds[1]] 
        let humLocMap = createMapping input[humLocInds[0]..humLocInds[1]] 
        
        let locations = List.map (fun seedNr -> let soilNr = tryFind seedSoilMap seedNr
                                                let fertNr = tryFind soilFertMap soilNr
                                                let waterNr = tryFind fertWaterMap fertNr
                                                let lightNr = tryFind waterLightMap waterNr
                                                let tempNr = tryFind lightTempMap lightNr
                                                let humNr = tryFind tempHumMap tempNr
                                                let locNr = tryFind humLocMap humNr
                                                locNr) seeds
        
        List.min locations
        
    let produceSolution1 (input: string list) =
        produceSolution input parseSeeds
    
    let parseSeedPairs (line: string): int64 list =
        let numbers = parseSeeds line
        
        let halfLength = numbers.Length / 2
        let iters = List.map (fun num -> num * 2) [0..halfLength - 1]
        
        let seeds = List.fold (fun acc num -> [numbers[num]..numbers[num]+numbers[num+1]]@acc) [] iters
        
        seeds
    
    let produceSolution2 (input: string list) =
        produceSolution input parseSeedPairs
        
        
        
        
        
        
        
        