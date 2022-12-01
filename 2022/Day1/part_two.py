import sys

def add_sorted(list: list[int], value: int, maxSize: int):
    list.append(value)
    list.sort(reverse=True)
    return list[:maxSize]

def main():
    with open(sys.argv[1], "r") as f:
        lines = f.readlines()
        maximum = []
        current = 0
        for line in lines:
            line = line.strip()
            if line == "":
                maximum = add_sorted(maximum, current, 3)
                current = 0
            else:
                current = current + int(line)
        print(sum(maximum))

if __name__ == "__main__":
    main()
