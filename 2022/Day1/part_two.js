const fs = require("fs");

const fileName = process.argv[2] || "data/input.txt";
const content = fs.readFileSync(fileName, "utf8");
const lines = content.split(/\r?\n/);

function include_calories(array, value, maxSize) {
  array = [...array, value].sort((a, b) => a - b);
  return array.length > maxSize ? array.slice(1) : array;
}

const result = lines.reduce(
  (acc, line) => {
    if (line === "") {
      const max = include_calories(acc.max, acc.current, 3);
      return { max, current: 0 };
    }
    const value = parseInt(line, 10);
    return { max: acc.max, current: acc.current + value };
  },
  { max: [], current: 0 }
);

const sum = result.max.reduce((a, b) => a + b, 0);

console.log(sum);
