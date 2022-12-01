const fs = require("fs");

const fileName = process.argv[2] || "data/input.txt";
const content = fs.readFileSync(fileName, "utf8");
const lines = content.split(/\r?\n/);

const result = lines.reduce(
  (acc, line) => {
    if (line === "") {
      const max = Math.max(acc.max, acc.current);
      return { max, current: 0 };
    }
    const value = parseInt(line, 10);
    return { max: acc.max, current: acc.current + value };
  },
  { max: 0, current: 0 }
);

console.log(result.max);
