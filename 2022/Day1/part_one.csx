using System.Linq;

var result = System.IO.File.ReadAllLines(
    System.Environment.GetCommandLineArgs()[2])
    .Aggregate(
        new { max = 0, current = 0 },
        (acc, line) =>
        {
            if (int.TryParse(line, out var number))
                return new { max = acc.max, current = acc.current + number };
            return new { max = Math.Max(acc.max, acc.current), current = 0 };
        });

System.Console.WriteLine(result.max);
