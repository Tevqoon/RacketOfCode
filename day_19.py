with open("day_19.txt", "r") as f:
    contents = f.readlines()

# print(contents)

ex1 = ["--- scanner 0 ---",
       "0,2",
       "4,1",
       "3,3",
       "\n",
       "--- scanner 1 ---",
       "-1,-1",
       "-5,0",
       "-2,1"]

ex2 = ["--- scanner 0 ---",
"404,-588,-901",
"528,-643,409",
"-838,591,734",
"390,-675,-793",
"-537,-823,-458",
"-485,-357,347",
"-345,-311,381",
"-661,-816,-575",
"-876,649,763",
"-618,-824,-621",
"553,345,-567",
"474,580,667",
"-447,-329,318",
"-584,868,-557",
"544,-627,-890",
"564,392,-477",
"455,729,728",
"-892,524,684",
"-689,845,-530",
"423,-701,434",
"7,-33,-71",
"630,319,-379",
"443,580,662",
"-789,900,-551",
"459,-707,401",
"\n",
"--- scanner 1 ---",
"686,422,578",
"605,423,415",
"515,917,-361",
"-336,658,858",
"95,138,22",
"-476,619,847",
"-340,-569,-846",
"567,-361,727",
"-460,603,-452",
"669,-402,600",
"729,430,532",
"-500,-761,534",
"-322,571,750",
"-466,-666,-811",
"-429,-592,574",
"-355,545,-477",
"703,-491,-529",
"-328,-685,520",
"413,935,-424",
"-391,539,-444",
"586,-435,557",
"-364,-763,-893",
"807,-499,-711",
"755,-354,-619",
"553,889,-390",
"\n",
"--- scanner 2 ---",
"649,640,665",
"682,-795,504",
"-784,533,-524",
"-644,584,-595",
"-588,-843,648",
"-30,6,44",
"-674,560,763",
"500,723,-460",
"609,671,-379",
"-555,-800,653",
"-675,-892,-343",
"697,-426,-610",
"578,704,681",
"493,664,-388",
"-671,-858,530",
"-667,343,800",
"571,-461,-707",
"-138,-166,112",
"-889,563,-600",
"646,-828,498",
"640,759,510",
"-630,509,768",
"-681,-892,-333",
"673,-379,-804",
"-742,-814,-386",
"577,-820,562",
"\n",
"--- scanner 3 ---",
"-589,542,597",
"605,-692,669",
"-500,565,-823",
"-660,373,557",
"-458,-679,-417",
"-488,449,543",
"-626,468,-788",
"338,-750,-386",
"528,-832,-391",
"562,-778,733",
"-938,-730,414",
"543,643,-506",
"-524,371,-870",
"407,773,750",
"-104,29,83",
"378,-903,-323",
"-778,-728,485",
"426,699,580",
"-438,-605,-362",
"-469,-447,-387",
"509,732,623",
"647,635,-688",
"-868,-804,481",
"614,-800,639",
"595,780,-596",
"\n",
"--- scanner 4 ---",
"727,592,562",
"-293,-554,779",
"441,611,-461",
"-714,465,-776",
"-743,427,-804",
"-660,-479,-426",
"832,-632,460",
"927,-485,-438",
"408,393,-506",
"466,436,-512",
"110,16,151",
"-258,-428,682",
"-393,719,612",
"-211,-452,876",
"808,-476,-593",
"-575,615,604",
"-485,667,467",
"-680,325,-822",
"-627,-443,-432",
"872,-547,-609",
"833,512,582",
"807,604,487",
"839,-516,451",
"891,-625,532",
"-652,-548,-490",
"30,-46,-14",]

def dot(v1, v2):
    return sum(x * y for x, y in zip(v1, v2))

def mult_mat_vect(m, v):
    return [dot(row, v) for row in m]

def transpose(m):
    return tuple(zip(*m))

def mult_mat(m1, m2):
    return transpose([mult_mat_vect(m1, col) for col in transpose(m2)])

identity = ((1,0,0), (0,1,0), (0,0,1))

def mat_pow(m, p):
    if p == 0:
        return identity
    else:
        return mult_mat(m, mat_pow(m, p - 1))

def get_orientations():
    Rx = ((0, 0, -1), (0, 1, 0), (1, 0, 0))
    Ry = ((1, 0, 0), (0, 0, -1), (0, 1, 0))
    Rz = ((0, -1, 0), (1, 0, 0), (0, 0, 1))
    orientations = set()
    inverses = {}
    for x in range(4):
        for y in range(4):
            for z in range(4):
                orientation = mult_mat(mat_pow(Rx, x), mult_mat(mat_pow(Ry, y), mat_pow(Rz, z)))
                orientations.add(orientation)
    return orientations

orientations = get_orientations()

def test_orientations():
    for o in orientations:
        print(mult_mat(o, inverses[o]))
    
def get_scanners(lines):
    scanners = []
    scanner = []
    for line in lines:
        if line == "\n":
            scanners.append(scanner)
            scanner = []
        elif not "scanner" in line:
            scanner.append(tuple(map(int, line.strip().split(","))))
    else:
        scanners.append(scanner)
    return scanners

def tuple_sum(tup1, tup2):
    return tuple(x + y for x, y in zip(tup1, tup2))

def difference(tup1, tup2):
    return tuple(x - y for x, y in zip(tup1, tup2))

def orient(tup, orientation):
    return mult_mat_vect(orientation, tup)

def try_difference(scanner1, scanner2, n):
    """Tries to return the relative position of scanner2 to scanner1.
       If it finds at least n overlapping points, it returns a pair of the difference
       and the relevant rotation. This can then be passed to the translator function.
       If not enough common points are detected, it returns None."""
    for orientation in orientations:
        # print("Current orientation: " + str(orientation))
        differences = {}
        for beacon1 in scanner1:
            for beacon2 in scanner2:
                beacon2 = orient(beacon2, orientation)
                relative = difference(beacon1, beacon2)
                differences[relative] = differences.get(relative, []) + [(beacon1, beacon2)]
        # print(differences)
        possibilities = {key : value for key, value in differences.items() if len(value) >= n}
        if len(possibilities) > 1:
            print("Found multiple possibilities!")
        elif len(possibilities) == 1:
            # print(possibilities)
            return list(possibilities.keys())[0], orientation
    else:
        return None

def get_connections(scanners):
    connected = [{} for scanner in scanners]
    for i, scanner1 in enumerate(scanners):
        for j, scanner2 in [(i, scanner) for i, scanner in enumerate(scanners) if scanner != scanner1]:
            ret = try_difference(scanner1, scanner2, 12)
            if ret:
                connected[i][j] = ret
    return connected

def to_graph(connections):
    parents = [None for _ in connections]
    parents[0] = 0
    to_process = {0}
    while to_process:
        x = to_process.pop()
        for y in connections[x].keys():
            if parents[y] == None:
                parents[y] = x
                to_process.add(y)
    return parents

def transform_to_coordinates(scanner, relative, orientation):
    return {tuple_sum(orient(beacon, orientation), relative) for beacon in scanner}

def transform_to_zero(beacons, index, connections, graph):
    if index == 0:
        return beacons
    else:
        parent = graph[index]
        rel, ori = connections[parent][index]
        transformed = transform_to_coordinates(beacons, rel, ori)
        return transform_to_zero(transformed, parent, connections, graph)

def scanner_position(index, connections, graph):
    return transform_to_zero([(0,0,0)], index, connections, graph).pop()
    
def manhattan(tup1, tup2):
    s = 0
    for x, y in zip(tup1, tup2):
        s += abs(x - y)
    return s

def solver(scanners):
    connections = get_connections(scanners)
    print("Calculated connections.")
    graph = to_graph(connections)
    print("Calculated connection graph")
    beacons = set()
    for index, scanner in enumerate(scanners):
        new = transform_to_zero(scanner, index, connections, graph)
        beacons = beacons.union(new)
    print(len(beacons))

    positions = [(0,0,0)]
    for scanner in range(1, len(scanners)):
        positions.append(scanner_position(scanner, connections, graph))
    print(max(manhattan(x, y) for x in positions for y in positions))
        

scanners = get_scanners(contents)

solver(scanners)

# connections = get_connections(scanners)
# graph = to_graph(connections)
# 
# rel01, ori01 = connections[0][1]
# transform = transform_to_coordinates(scanners[1], rel01, ori01)
# # print(transform)
# 
# # To transform 4 -> 1 -> 0
# rel14, ori14 = connections[1][4]
# transform1 = transform_to_coordinates(scanners[4], rel14, ori14)
# transform2 = transform_to_coordinates(transform1, rel01, ori01)
# print(transform2)


