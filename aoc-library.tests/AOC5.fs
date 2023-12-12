namespace aoc_library.tests

open System
open NUnit.Framework
open aoc_library


[<TestFixture>]
type AOC5() =
    [<Test>]
    member this.TestSolution() =
        let expected = 35
        let input = util.readLinesFromFile ("aoc-library.tests/Tests/aoc-5.txt")    
        let actual = aoc5.produceSolution1 input
        
        Assert.That(actual, Is.EqualTo(expected))
            
    [<Test>]
    member this.TestSolution2() =
        let expected = 46
        let input = util.readLinesFromFile ("aoc-library.tests/Tests/aoc-5.txt")    
        let actual = aoc5.produceSolution2 input
        
        Assert.That(actual, Is.EqualTo(expected))