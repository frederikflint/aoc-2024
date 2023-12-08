// For more information see https://aka.ms/fsharp-console-apps
open aoc_library


let res1 = 
    aoc1.produceSolution (util.readLinesFromFile "aoc-library/Inputs/aoc-1.txt") false

let res1_2 = aoc1.produceSolution (util.readLinesFromFile "aoc-library/Inputs/aoc-1-2.txt") true

printfn "%d %d" res1 res1_2

let res2 = 
    aoc2.produceSolution1(util.readLinesFromFile "aoc-library/Inputs/aoc-2.txt") (14, 13, 12)

let res2_1 = 
    aoc2.produceSolution2(util.readLinesFromFile "aoc-library/Inputs/aoc-2.txt")

printfn "%d %d" res2 res2_1

let res3 =
    aoc3.produceSolution(util.readLinesFromFile "aoc-library/Inputs/aoc-3.txt")
    
let res3_1 =
    aoc3.produceSolution2(util.readLinesFromFile "aoc-library/Inputs/aoc-3.txt")
    
printfn "%d %d" res3 res3_1

let res4 =
    aoc4.produceSolution(util.readLinesFromFile "aoc-library/Inputs/aoc-4.txt")
    
let res4_1 =
    aoc4.produceSolution2(util.readLinesFromFile "aoc-library/Inputs/aoc-4.txt")
    
printfn "%d %d" res4 res4_1