package main

import (
	"bufio"
	"fmt"
	"os"
)

// A=Rock, B=Paper, C=Scissors
// Part1: X=Rock, Y=Paper, Z=Scissors
// Part2: X=Loose, Y=Draw, Z=Win

func computeChoicePart1(choice string) string {
	switch choice {
	case "X":
		return "A"
	case "Y":
		return "B"
	case "Z":
		return "C"
	}
	panic("Invalid choice")
}

func computeChoicePart2(opponent string, choice string) string {
	switch choice {
	case "X": // I loose
		switch opponent {
		case "A": // Rock
			return "C" // Scissors to loose
		case "B": // Paper
			return "A" // Rock to loose
		case "C": // Scissors
			return "B" // Paper to loose
		}
	case "Y": // I draw
		return opponent // I play the same to draw
	case "Z": // I win
		switch opponent {
		case "A": // Rock
			return "B" // Paper to win
		case "B": // Paper
			return "C" // Scissors to win
		case "C": // Scissors
			return "A" // Rock to win
		}
	}
	panic("Invalid choice")
}

func valueChoice(choice string) int {
	switch choice {
	case "A": // Rock has value 1
		return 1
	case "B": // Paper has value 2
		return 2
	case "C": // Scissors has value 3
		return 3
	}
	return 0
}

func playRound(opponent string, self string) int {
	switch self {
	case "A":
		switch opponent {
		case "A":
			return 3 // Draw
		case "B":
			return 0 // Rock looses to Paper
		case "C":
			return 6 // Rock wins to Scissors
		}
	case "B":
		switch opponent {
		case "A": // Paper wins to Rock
			return 6
		case "B": // Draw
			return 3
		case "C": // Paper looses to Scissors
			return 0
		}
	case "C":
		switch opponent {
		case "A": // Scissors looses to Rock
			return 0
		case "B": // Scissors wins to Paper
			return 6
		case "C": // Draw
			return 3
		}
	}
	return 0
}

func part1(scanner *bufio.Scanner) {
	score := 0
	for scanner.Scan() {
		line := scanner.Text()
		opponent := string(line[0])
		choice := computeChoicePart1(string(line[2]))
		score += valueChoice(choice)
		score += playRound(opponent, choice)
	}
	fmt.Println(score)
}

func part2(scanner *bufio.Scanner) {
	score := 0
	for scanner.Scan() {
		line := scanner.Text()
		opponent := string(line[0])
		choice := computeChoicePart2(opponent, string(line[2]))
		score += valueChoice(choice)
		score += playRound(opponent, choice)
	}
	fmt.Println(score)
}

func main() {
	fileName := os.Args[1]
	part := os.Args[2]
	readFile, err := os.Open(fileName)
	if err != nil {
		panic(err)
	}
	defer readFile.Close()
	scanner := bufio.NewScanner(readFile)
	scanner.Split(bufio.ScanLines)
	switch part {
	case "1":
		part1(scanner)
	case "2":
		part2(scanner)
	}
}
