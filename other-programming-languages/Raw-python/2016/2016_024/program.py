from itertools import permutations
from collections import deque

def read_input():
    grid = []
    positions = {}
    try:
        while True:
            line = input()
            grid.append(list(line))
    except EOFError:
        pass
    for y, row in enumerate(grid):
        for x, ch in enumerate(row):
            if ch.isdigit():
                positions[int(ch)] = (x, y)
    return grid, positions

def bfs(grid, start):
    queue = deque()
    visited = set()
    dist = {}
    queue.append((start[0], start[1], 0))
    visited.add((start[0], start[1]))
    while queue:
        x, y, d = queue.popleft()
        if grid[y][x].isdigit():
            num = int(grid[y][x])
            dist[num] = d
        for dx, dy in [(-1,0),(1,0),(0,-1),(0,1)]:
            nx, ny = x+dx, y+dy
            if 0 <= ny < len(grid) and 0 <= nx < len(grid[0]):
                if grid[ny][nx] != '#' and (nx, ny) not in visited:
                    visited.add((nx, ny))
                    queue.append((nx, ny, d+1))
    return dist

def main():
    grid, positions = read_input()
    nums = sorted(positions.keys())
    distances = {}
    for num in nums:
        dist = bfs(grid, positions[num])
        distances[num] = dist
    min_total = None
    nums_to_visit = [n for n in nums if n != 0]
    for perm in permutations(nums_to_visit):
        total = 0
        prev = 0
        for num in perm:
            total += distances[prev][num]
            prev = num
        if min_total is None or total < min_total:
            min_total = total
    print(min_total)

if __name__ == "__main__":
    main()
