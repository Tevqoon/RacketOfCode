p1 = 5
p2 = 6

rolls = [(3,1),(4,3),(5,6),(6,7),(7,6),(8,3),(9,1)]

def wins(space1, points1, space2, points2):
    if points2 <= 0:
        return (0, 1)

    w1, w2 = 0, 0
    for (roll, freq) in rolls:
        new_space = (space1 + roll) % 10
        new_points = points1 - new_space - 1
        u2, u1 = wins(space2, points2, new_space, new_points)

        w1 += freq * u1
        w2 += freq * u2

    return w1, w2

print(max(wins(p1 - 1, 21, p2 - 1, 21)))
    
    
