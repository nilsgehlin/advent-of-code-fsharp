using System;
using System.IO;
using System.Text;
using System.Linq;

var data = File.ReadAllText("input.txt");
var query =
    from calorieList in data.Split($"{Environment.NewLine}{Environment.NewLine}")
    let caloriePerElf = calorieList.Split(Environment.NewLine).Select(int.Parse).Sum()
    orderby caloriePerElf descending
    select caloriePerElf;

var answerPartOne = query.First();
Console.WriteLine($"Answer Part One: {answerPartOne}");

var answerPartTwo = query.Take(3).Sum();
Console.WriteLine($"Answer Part Two: {answerPartTwo}");
