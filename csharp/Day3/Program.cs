
using System;
using System.IO;
using System.Text;

namespace Day3;
class Program
{
    static void Main()
    {
        var data = File.ReadAllText("input.txt");
        var answerPartOne = data.Split($"{Environment.NewLine}").Select(CalculatePrio).Sum();
        Console.WriteLine($"Answer Part One: {answerPartOne}");
    }

    static int CalculatePrio(string line)
    {
        var items = line.ToCharArray();
        var compartmentSize = items.Length;



        // foreach (var item in items)
        // {
        //     System.Console.Write($"{item}, ");
        // }
        // System.Console.WriteLine();
        return 0;
    }
}



