namespace aoc_library.tests

open System
open NUnit.Framework
open aoc_library


[<TestFixture>]
type AOC4() =
    [<Test>]
    member this.TestParseLine() =
        let expected = ([41; 48; 83; 86; 17], [83; 86; 6; 31; 17; 9; 48; 53])

        let actual = aoc4.parseLine "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
        
        Assert.That(actual, Is.EqualTo(expected))
        
    [<Test>]
    member this.TestPointsForLine() =
        let expected = 8
        
        let actual = aoc4.calculateLinePoints (aoc4.parseLine "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")
        
        Assert.That(actual, Is.EqualTo(expected))
        
    [<Test>]
    member this.TestSolution() =
        let expected = 13
        
        let input = util.readLinesFromFile ("aoc-library.tests/Tests/aoc-4.txt")
        
        let actual = aoc4.produceSolution input
        
        Assert.That(actual, Is.EqualTo(expected))
        
    [<Test>]
    member this.TestSolution2() =
        let expected = 30
                
        let input = util.readLinesFromFile ("aoc-library.tests/Tests/aoc-4.txt")
        
        let actual = aoc4.produceSolution2 input
        
        Assert.That(actual, Is.EqualTo(expected))
        
    


// Card1: 4 + res[Card2..5] = 4 + 6 + 3 + 1 = 14
// Card2: 2 + res[Card3..4] = 2 + 3 + 1 = 6
// Card3: 2 + res[Card4..5] = 2 + 1 = 3
// Card4: 1 + res[Card5..5] = 1 + 0 = 1
// Card5: 0 + 0
// Card6: 0 + 0
//
// + 6 