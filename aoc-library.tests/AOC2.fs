namespace aoc_library.tests

open System
open NUnit.Framework
open aoc_library


[<TestFixture>]
type AOC2() =

    [<Test>]
    member this.TestGameCreation() =
        let expected = Set.ofList [ (6, 8, 20); (5, 13, 4); (0, 5, 1) ]

        let input = "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
        let actual = aoc2.composeGame (input)
        Assert.That(actual, Is.EquivalentTo(expected))

    [<Test>]
    member this.TestGameExtractionMax() =
        let input = Set.ofList [ (6, 8, 20); (5, 13, 4); (0, 5, 1) ]

        let expected = (6, 13, 20)
        let actual = aoc2.extractMaxFromGame input
        Assert.That(actual, Is.EqualTo(expected))


    [<Test>]
    member this.TestSolution() = 
        let input = util.readLinesFromFile ("aoc-library.tests/Tests/aoc-2.txt")

        let expected = 8;
        let actual = aoc2.produceSolution1 input (14, 13, 12)

        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.TestSolution2() = 
        let input = util.readLinesFromFile ("aoc-library.tests/Tests/aoc-2.txt")

        let expected = 2286;
        let actual = aoc2.produceSolution2 input

        Assert.That(actual, Is.EqualTo(expected))