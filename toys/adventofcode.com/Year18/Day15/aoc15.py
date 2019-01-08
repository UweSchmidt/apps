def setup_inp(inp):
    """Convert list of strings into list of lists, with glves/goblins replaced by tuples"""
    grid = []
    for rowI,row in enumerate(inp.split("\n")):
        grid.append([x for x in row])
        for colI,col in enumerate(row):
            if col in ["G","E"]:
                #Replace enemies with tuples so we can track them - (character_type, hit_points, moved_already_bool)
                char_tup = (col, 200, False)
                grid[rowI][colI] = char_tup
    return grid

def print_board(inp):
    for row in inp:
        extra = []
        print_row = []  #In case we append hitpoints
        for char in row:
            if isinstance(char,tuple):
                print_row.append(char[0])
                extra.append(str(char[1]))
            else:
                print_row.append(char)
        print("".join(print_row),"   ", " ".join(extra))

def move_character(inp, from_row, from_col, to_row, to_col, char):
    """Move character on grid, and increment the i value so we can tell we already moved it"""
    inp[from_row][from_col] = "."
    inp[to_row][to_col] = (char[0],char[1],True)
    return inp

def attack(inp, row, col, enemy, damage=3):
    """
    Attack weakest adjacent enemy, if one is there
    If multiple weakest enemies, attack in reading order
    Return the modified board, and a boolean indicating whether anyone died
    """

    if not adjacent_enemy(inp, row, col, enemy):
        return inp, False

    #Create a dict of {coordinates: hp} for each adjacent enemy
    enemies = {}
    for coords in [(row-1,col), (row+1,col), (row,col-1), (row,col+1)]:
        if inp[coords[0]][coords[1]][0] == enemy:
            #enemy is a tuple, (char_type, hp, already_moved_bool)
            enemies[coords] = inp[coords[0]][coords[1]][1]

    #Filter to only the enemies with minimum hp
    min_hp = min(enemies.values())
    enemies = [x for x in enemies if enemies[x]==min_hp]

    #Now we have a list of coordinates, we can sort to get reading order, then take the first to get our enemy
    enemies.sort()
    coords = enemies[0]

    enemy = inp[coords[0]][coords[1]]
    enemy_pts = enemy[1] - damage
    enemy_tup = (enemy[0], enemy_pts, enemy[2])

    #Check for killed
    if enemy_pts <= 0:
        inp[coords[0]][coords[1]] = "."
        return inp, True
    else:
        inp[coords[0]][coords[1]] = enemy_tup
        return inp, False


def adjacent_enemy(inp, rowI, colI, enemy):
    """Check for enemy in adjacent square"""
    if any(x[0]==enemy for x in [inp[rowI+1][colI], inp[rowI-1][colI], inp[rowI][colI+1], inp[rowI][colI-1]]):
        return True
    return False

def get_best_move(best_moves):
    """
    Takes a list of tuples of
    (first_move, number_of_moves, tile_coordinates), which might look like -
    ((12, 22), 8, (17, 25))
    ((12, 22), 8, (18, 24))
    ((12, 22), 8, (19, 21))
    ((13, 21), 6, (19, 21))
    ((13, 23), 6, (17, 25))
    ((13, 23), 6, (18, 24))
    ((14, 22), 6, (17, 25))
    ((14, 22), 6, (18, 24))
    ((14, 22), 6, (19, 21))
    And filters/sorts them to satisfy all the conditions
    """

    if not best_moves:
        return None

    #First condition - fewest number of moves away
    min_steps = min([x[1] for x in best_moves])
    best_moves = [x for x in best_moves if x[1]==min_steps]

    #Second condition - if tie, choose the first tile in reading order
    best_moves.sort(key = lambda x:x[2])
    best_moves = [x for x in best_moves if x[2]==best_moves[0][2]]

    #Third condition - if tie, take the first step in reading order
    best_moves.sort(key = lambda x:x[0])
    best_moves = [x for x in best_moves if x[0]==best_moves[0][0]]

    return best_moves[0][0]

def count_characters(inp):
    seen = {"G":0,"E":0}
    for row in inp:
        for col in row:
            if col[0] in ["G","E"]:
                seen[col[0]]+=1
    return seen

def bfs_move(inp, rowI, colI, hero, enemy):
    """
    Perform a breadth first search for each adjacent tile
    Although not the most efficient, the approach is still fast and makes it
    easy to sort in such a way that satisfies all the conditions
    """

    #If an enemy is located adjacent to our current location - no move!
    if adjacent_enemy(inp, rowI, colI, enemy):
        return None

    first_moves = [(rowI+1,colI),(rowI-1,colI),(rowI,colI-1),(rowI,colI+1)]
    #Filter down to valid first moves - must be a '.' there
    first_moves = [x for x in first_moves if inp[x[0]][x[1]]=="."]

    #Keep the list of tuples nearest tiles we've found, in format -
    #(first_move, number_of_moves, tile_coordinates)
    #At the end we'll need to use all these values to find the proper move
    best_moves = []

    for move in first_moves:
        r,c = move

        #We might immediately have an adjacent enemy and not need to search further
        if adjacent_enemy(inp, r, c, enemy):
            best_moves.append((move, 1, move))
            continue

        #We'll need to keep track of two things -
        #seen_coordinates - the tiles we've already visited
        #stack - the "new" tiles accessible from the current furthest points
        seen_coordinates = {(rowI,colI),(r,c)}
        stack = [(r+1,c),(r-1,c),(r,c-1),(r,c+1)]
        #Filter stack to only include "." tiles, which we haven't already seen
        stack = [x for x in stack if inp[x[0]][x[1]]=="." and (x[0],x[1]) not in seen_coordinates]

        #Now do the search -

        i=1  #Already have moved one tile at this point
        run = True
        while run:
            i+=1

            #Keep track of the new tiles here
            new_stack = []

            #Loop through and look for new tiles to add
            for tile in stack:
                if tile in seen_coordinates:
                    continue

                seen_coordinates.add(tile)
                r,c = tile

                if adjacent_enemy(inp, r, c, enemy):
                    best_moves.append((move,i,(r,c)))
                    #We want to complete this iteration to find all other reachable tiles at the same distance
                    run = False
                    continue

                #Add all newly accessible tiles to stack
                new_tiles = [(r+1,c),(r-1,c),(r,c-1),(r,c+1)]
                new_stack += [x for x in new_tiles if inp[x[0]][x[1]]=="." and (x[0],x[1]) not in seen_coordinates]

            stack = list(set(new_stack))
            #We might also need to end at this point if we have no more newly accessible tiles
            if not stack:
                run = False

    #Take our list of the best_moves from each starting point that we generated, and find the one move we'll take
    return get_best_move(best_moves)


def score_game(inp, rounds):
    pts = 0
    for rowI,row in enumerate(inp):
        for colI,col in enumerate(row):
            if col[0] in ["G","E"]:
                pts+=col[1]
    return rounds*pts

def reset_moved_bools(inp):
    """Reset the third value in our character tuples, which tracks whether they've moved in a round"""
    for rowI,row in enumerate(inp):
        for colI,col in enumerate(row):
            if col[0] in ["G","E"]:
                char_tup = (col[0],col[1],False)
                inp[rowI][colI] = char_tup
    return inp

t0 = """#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######"""

t1 = """#######
#G..#E#
#E#E.E#
#G.##.#
#...#E#
#...E.#
#######"""


t2 = """#######
#E..EG#
#.#G.E#
#E.##E#
#G..#.#
#..E#.#
#######"""


t3 = """#######
#E.G#.#
#.#G..#
#G.#.G#
#G..#.#
#...E.#
#######"""


t4 = """#######
#.E...#
#.#..G#
#.###.#
#E#G#G#
#...#G#
#######"""


t5 = """#########
#G......#
#.E.#...#
#..##..G#
#...##..#
#...#...#
#.G...G.#
#.....G.#
#########"""

def problem1(inp, print_=False):
    grid = setup_inp(inp)

    rounds = 0

    while True:
        #Count the current number of each character type
        #We can use this to determine if the game has ended in the middle or end of a round
        counts = count_characters(grid)

        seen = {}
        for rowI,row in enumerate(grid):
            for colI,col in enumerate(row):
                char = grid[rowI][colI]
                if isinstance(char, tuple):

                    #Indicates we already moved it this round
                    if char[2]:
                        continue

                    r,c = rowI,colI  #Keep track of our current coordinates in case we move
                    hero = char[0]
                    enemy = "G" if hero=="E" else "E"

                    counts[hero]-=1

                    move_to = bfs_move(grid, rowI, colI, hero, enemy)
                    if move_to:
                        r,c = move_to  #Need to update our current coordinates for the impending attack
                        grid = move_character(grid, rowI, colI, r, c, char)

                    grid, death = attack(grid, r, c, enemy)
                    if death:
                        #Check to see if it's over - all of one side dead
                        current_counts = count_characters(grid)
                        game_over = any(x==0 for x in current_counts.values())
                        #If game is over, we need to see if the round is complete or not
                        if game_over:
                            #Means we ended midround
                            if counts[hero]>0:
                                final_score = score_game(grid, rounds)
                            #Otherwise round is complete- add 1 to rounds when calculating
                            else:
                                rounds+=1
                                final_score = score_game(grid, rounds)
                            if print_:
                                print("GAME ENDED",rounds)
                                print_board(grid)

                            return final_score

        #Reset the variable that tracks whether a character has moved in a round
        grid = reset_moved_bools(grid)

        rounds += 1

        if print_:
            print(rounds)
            print_board(grid)



def problem2_loop(inp, damage_dict, print_=False):

    grid = setup_inp(inp)

    rounds = 0

    while True:
        #Count the current number of each character type
        #We can use this to determine if the game has ended in the middle or end of a round
        counts = count_characters(grid)

        seen = {}
        for rowI,row in enumerate(grid):
            for colI,col in enumerate(row):
                char = grid[rowI][colI]
                if isinstance(char, tuple):

                    #Indicates we already moved it this round
                    if char[2]:
                        continue

                    r,c = rowI,colI  #Keep track of our current coordinates in case we move
                    hero = char[0]
                    enemy = "G" if hero=="E" else "E"

                    counts[hero]-=1

                    move_to = bfs_move(grid, rowI, colI, hero, enemy)
                    if move_to:
                        r,c = move_to  #Need to update our current coordinates for the impending attack
                        grid = move_character(grid, rowI, colI, r, c, char)

                    damage = damage_dict[hero]
                    grid, death = attack(grid, r, c, enemy, damage)
                    if death and enemy=="E":
                        #FAILED
                        return False
                    #If goblin death, same logic as before
                    elif death:
                        #Check to see if it's over - all of one side dead
                        current_counts = count_characters(grid)
                        game_over = any(x==0 for x in current_counts.values())
                        #If game is over, we need to see if the round is complete or not
                        if game_over:
                            #Means we ended midround
                            if counts[hero]>0:
                                final_score = score_game(grid, rounds)
                            #Otherwise round is complete- add 1 to rounds when calculating
                            else:
                                rounds+=1
                                final_score = score_game(grid, rounds)
                            if print_:
                                print("GAME ENDED",rounds)
                                print_board(grid)

                            return final_score

        #Reset the variable that tracks whether a character has moved in a round
        grid = reset_moved_bools(grid)

        rounds += 1

        if print_:
            print(rounds)
            print_board(grid)

def problem2(inp, print_=False):
    score = False
    damage_dict = {"G":3, "E":3}
    while not score:
        damage_dict["E"] += 1
        score = problem2_loop(inp, damage_dict, print_)
    return score

if __name__=="__main__":
    with open("input15.txt","r") as f:
        data = f.read().strip()

    for row in data.split("\n"):
        print(row)

    assert problem1(t0)==27730
    assert problem1(t1)==36334
    assert problem1(t2)==39514
    assert problem1(t3)==27755
    assert problem1(t4)==28944
    assert problem1(t5)==18740

    print(problem1(data))
    print(problem2(data, True))
