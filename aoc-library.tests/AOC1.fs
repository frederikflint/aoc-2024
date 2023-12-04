namespace aoc_library.tests

open NUnit.Framework
open aoc_library


[<TestFixture>]
type AOC1() =

    [<Test>]
    member this.TestDigitFinding() =
        let expectedWithSpelled = [ 8; 3; 7; 4; 8; 5; 1 ]
        let expectedWithoutSpelled = [ 8; 5 ]
        let input = "8threesevenfourgbgteight5twonenjr"
        Assert.That(aoc1.findDigitsInLine (Seq.toList input) true, Is.EquivalentTo(expectedWithSpelled))
        Assert.That(aoc1.findDigitsInLine (Seq.toList input) false, Is.EquivalentTo(expectedWithoutSpelled))

    [<Test>]
    member this.TestAoc1() =
        let expected = 142
        let input = util.readLinesFromFile ("aoc-library.tests/Tests/aoc-1.txt")
        Assert.That(aoc1.produceSolution input false, Is.EqualTo(expected))

    [<Test>]
    member this.TestAoc1_2() =
        let expected = 281
        let input = util.readLinesFromFile ("aoc-library.tests/Tests/aoc-1-2.txt")
        Assert.That(aoc1.produceSolution input true, Is.EqualTo(expected))
