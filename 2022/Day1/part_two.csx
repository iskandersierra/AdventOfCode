using System.Linq;

int[] AddSorted(int[] list, int value, int maxSize) =>
    list.Append(value).OrderDescending().Take(maxSize).ToArray();

var result = System.IO.File.ReadAllLines(
    System.Environment.GetCommandLineArgs()[2])
    .Aggregate(
        new { max = Array.Empty<int>(), current = 0 },
        (acc, line) =>
        {
            if (int.TryParse(line, out var number))
                return new { max = acc.max, current = acc.current + number };
            return new { max = AddSorted(acc.max, acc.current, 3), current = 0 };
        });

System.Console.WriteLine(result.max.Sum());
