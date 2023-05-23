using System;
using System.IO;
using System.Text;

namespace Day2;
class Program
{
    static void Main()
    {
        var data = File.ReadAllText("input.txt");
        var answerPartOne = data.Split($"{Environment.NewLine}").Select(CalculateScoreOne).Sum();
        Console.WriteLine($"Answer Part One: {answerPartOne}");

        var answerPartTwo = data.Split($"{Environment.NewLine}").Select(CalculateScoreTwo).Sum();
        Console.WriteLine($"Answer Part Two: {answerPartTwo}");
    }

    static Shape CreateShape(string letter)
    {
        return letter switch
        {
            "A" => Shape.Rock,
            "X" => Shape.Rock,
            "B" => Shape.Paper,
            "Y" => Shape.Paper,
            "C" => Shape.Scissor,
            "Z" => Shape.Scissor,
            _ => Shape.Unvalid
        };
    }

    static Shape WhatBeats(Shape shape) => shape switch
    {
        Shape.Rock => Shape.Paper,
        Shape.Paper => Shape.Scissor,
        Shape.Scissor => Shape.Rock,
        _ => Shape.Unvalid
    };

    static Shape WhatIsBeatenBy(Shape shape) => shape switch
    {
        Shape.Rock => Shape.Scissor,
        Shape.Paper => Shape.Rock,
        Shape.Scissor => Shape.Paper,
        _ => Shape.Unvalid
    };

    static int CalculateScoreOne(string roundInput)
    {
        var opponent = CreateShape(roundInput.Split(" ").ElementAt(0));
        var mine = CreateShape(roundInput.Split(" ").ElementAt(1));

        if (mine == opponent)
        {
            return (int)mine + 3;
        }
        if (mine == WhatBeats(opponent))
        {
            return (int)mine + 6;
        }

        return (int)mine;
    }

    static int CalculateScoreTwo(string roundInput)
    {
        var opponent = CreateShape(roundInput.Split(" ").ElementAt(0));
        var desiredOutcome = roundInput.Split(" ").ElementAt(1);

        return desiredOutcome switch
        {
            "X" => (int)WhatIsBeatenBy(opponent),
            "Y" => (int)opponent + 3,
            "Z" => (int)WhatBeats(opponent) + 6,
            _ => 0
        };
    }
}

