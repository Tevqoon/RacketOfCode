with open("day_20.txt", "r") as f:
    contents = f.readlines()

ex1 = ["..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#",
"\n",
"#..#.",
"#....",
"##..#",
"..#..",
"..###"]

# contents = ex1

key = list(map(int, contents[0].strip().replace("#", "1").replace(".", "0")))
image = [[1 if c == "#" else 0 for c in line.strip()] for line in contents[2:]]

def to_look(x, y):
    return [(i, j) for i in range(x - 1, x + 2) for j in range(y - 1, y + 2)]

def pad_and_prepare(matrix, amount, value):
    return [[value for _ in range(len(matrix[0]) + 4)] for _ in range(amount)] + [[value for _ in range(amount)] + line + [value for _ in range(amount)] for line in matrix] + [[value for _ in range(len(matrix[0]) + 4)] for _ in range(amount)], [[value for _ in range(len(matrix[0]) + 4)] for _ in range(2 * amount + len(matrix))]

def trim(matrix):
    return [line[1:-1] for line in matrix[1:-1]]

def step(image, val):
    old, new = pad_and_prepare(image, 2, val)
    for x in range(1, len(new) - 1):
        for y in range(1, len(new[0]) - 1):
            # print(x, y)
            # print(to_look(x, y))
            new[x][y] = key[int("".join(map(str, [old[i][j] for i, j in to_look(x, y)])), 2)]
    return trim(new)

def print_image(image):
    s = ""
    for line in image:
        for c in line:
            s += "#" if c else "."
        s += '\n'
    print(s)

def step_n(n, image):
    val = 0
    for _ in range(n):
        image = step(image, val)
        val = 1 - val
    return sum(map(sum, image))

print(step_n(2, image))
print(step_n(50, image))
