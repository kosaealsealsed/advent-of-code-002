def solve():
    import sys
    
    # Read the entire file as lines
    lines = [line.rstrip('\n') for line in sys.stdin.readlines()]
    
    # Separate out the map portion vs. the moves portion
    # --------------------------------------------------
    #
    # According to the puzzle, the map is 50 lines high (each line 50+ columns wide)
    # enclosed by '##################################################' on top and bottom.
    # After the map, there's a line break or two, then an enormous move string that may
    # span multiple lines. We need to parse them carefully.

    warehouse_map = []
    move_lines = []
    parsing_map = True
    for line in lines:
        # End of map when we see a line that starts with something other than '#' or is blank,
        # or when we suspect the warehouse is fully read (since it’s a 50x50 or similar).
        if parsing_map:
            if line.strip() == '' or not line.startswith('#'):
                parsing_map = False
            else:
                warehouse_map.append(line)
        else:
            # Now we’re in the moves portion; puzzle states newlines in the move string are ignored
            move_lines.append(line.strip())
    
    # Combine all move-lines into one big string of moves
    moves = ''.join(move_lines)
    
    # Dimensions of the map
    height = len(warehouse_map)
    width  = len(warehouse_map[0]) if height > 0 else 0

    # Parse the map into a data structure
    # -----------------------------------
    boxes = set()     # set of (r, c) positions for boxes
    walls = set()     # set of (r, c) positions for walls
    robot = None      # (r, c) for robot
    
    for r, row in enumerate(warehouse_map):
        for c, ch in enumerate(row):
            if ch == '#':
                walls.add((r, c))
            elif ch == 'O':
                boxes.add((r, c))
            elif ch == '@':
                robot = (r, c)
            # We ignore '.' and everything else for the floor.
    
    # Helper for performing a move
    # ----------------------------
    # We want to move the robot in direction (dr, dc).
    # Return updated (robot, boxes).
    
    def attempt_move(robot_rc, boxes, dr, dc):
        (rr, cc) = robot_rc
        nr, nc = rr + dr, cc + dc  # new robot position if possible
        
        # If new robot spot is a wall, move blocked, do nothing
        if (nr, nc) in walls:
            return robot_rc, boxes
        
        # If new robot spot is empty floor, just move
        if (nr, nc) not in boxes:
            return (nr, nc), boxes
        
        # Else, (nr,nc) has a box. We need to push that box chain if possible.
        # Let’s gather all consecutive boxes in this direction (like a chain).
        # Start from the front-most box, going in the pushing direction, find how far the chain goes.
        
        # chain will hold all boxes in the line (nr, nc), (nr+dr, nc+dc), (nr+2*dr, nc+2*dc), ...
        chain = []
        check_r, check_c = nr, nc
        while (check_r, check_c) in boxes:
            chain.append((check_r, check_c))
            check_r += dr
            check_c += dc
        
        # Now check_r, check_c is the tile *after* the last box in the chain
        # If that tile is a wall or another box, we can’t push.
        if (check_r, check_c) in walls or (check_r, check_c) in boxes:
            # push fails, robot can’t move
            return robot_rc, boxes
        
        # Otherwise, we can push successfully.  We move the last box in chain forward by one,
        # then the second-last into the last's old location, etc.
        new_boxes = set(boxes)
        
        # Move the chain from last to first so we don’t overwrite
        for i in reversed(range(len(chain))):
            (br, bc) = chain[i]  # box row/col
            new_boxes.remove((br, bc))
            new_boxes.add((br + dr, bc + dc))
        
        # Finally, robot steps into the position vacated by the first box in the chain
        new_robot = (nr, nc)
        return new_robot, new_boxes

    # Directions map
    dir_map = {
        '^': (-1, 0),
        'v': (1, 0),
        '<': (0, -1),
        '>': (0, 1),
    }

    # Simulate all moves
    # ------------------
    rpos = robot
    bset = boxes
    for m in moves:
        dr, dc = dir_map[m]
        rpos, bset = attempt_move(rpos, bset, dr, dc)
    
    # Compute final sum of GPS coordinates
    # ------------------------------------
    # GPS(row, col) = 100*row + col
    # (Distance from top edge = row index, distance from left edge = col index)
    
    total = 0
    for (r, c) in bset:
        total += 100*r + c
    
    print(total)

if __name__ == "__main__":
    solve()
