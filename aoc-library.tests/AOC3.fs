namespace aoc_library.tests

open System
open NUnit.Framework
open aoc_library


[<TestFixture>]
type AOC3() =

    [<Test>]
    member this.TestParseLine() =
        let expected = [("467", [0; 2]); ("114", [5; 7])]

        let (actualNums, _) = aoc3.parseLine "467..114.."
        
        Assert.That(actualNums, Is.EquivalentTo(expected))
        
    [<Test>]
    member this.TestParseInput() =
        let expected = [
            ([("467", [0; 2]); ("114", [5; 7])], [])
            ([], [("*", 3)]);
            ([("35", [2;3]); ("633", [6;8])], [])
        ]

        let input = util.readLinesFromFile ("aoc-library.tests/Tests/aoc-3-short.txt")
        let actual = aoc3.parseInput input
        
        Assert.That(actual, Is.EquivalentTo(expected))
    
    [<Test>]
    member this.TestSolution() =
        let expected = 4361

        let input = util.readLinesFromFile ("aoc-library.tests/Tests/aoc-3.txt")
        let actual = aoc3.produceSolution input
        
        Assert.That(actual, Is.EqualTo(expected))
    
    [<Test>]
    member this.TestSolution2() =
        let expected = 467835

        let input = util.readLinesFromFile ("aoc-library.tests/Tests/aoc-3.txt")
        let actual = aoc3.produceSolution2 input
        
        Assert.That(actual, Is.EqualTo(expected))
    