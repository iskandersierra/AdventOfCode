import sys

def main():
    with open(sys.argv[1], "r") as f:
        lines = f.readlines()
        maximum = 0
        current = 0
        for line in lines:
            line = line.strip()
            if line == "":
                maximum = max(maximum, current)
                current = 0
            else:
                current = current + int(line)
        print(maximum)

if __name__ == "__main__":
    main()
