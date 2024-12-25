require 'set'
require 'rbtree'

# Using Deque from RBTree gem
class Deque
  def initialize
    @deque = []
  end

  def push(value)
    @deque.push(value)
  end

  def unshift(value)
    @deque.unshift(value)
  end

  def pop
    @deque.pop
  end

  def shift
    @deque.shift
  end

  def empty?
    @deque.empty?
  end

  def to_a
    @deque.dup
  end
end

# Part 1: Solve the maze with minimal cost
def solve_maze(maze_lines)
  directions = [
    [0, 1],   # East
    [1, 0],   # South
    [0, -1],  # West
    [-1, 0]   # North
  ]

  rows = maze_lines.length
  cols = maze_lines[0].length

  start = nil
  finish = nil
  rows.times do |r|
    cols.times do |c|
      case maze_lines[r][c]
      when 'S'
        start = [r, c]
      when 'E'
        finish = [r, c]
      end
    end
  end
  raise 'Could not find "S" or "E" in the maze.' if start.nil? || finish.nil?

  inf = Float::INFINITY
  dist = Array.new(rows) { Array.new(cols) { Array.new(4, inf) } }
  start_dir = 0
  dist[start[0]][start[1]][start_dir] = 0

  pq = [[0, start[0], start[1], start_dir]]
  visited = Set.new

  until pq.empty?
    pq.sort_by!(&:first)
    cost, r, c, d = pq.shift

    return cost if [r, c] == finish

    next if visited.include?([r, c, d])
    visited.add([r, c, d])

    current_dist = dist[r][c][d]
    next if cost > current_dist

    dr, dc = directions[d]
    nr, nc = r + dr, c + dc
    if nr.between?(0, rows - 1) && nc.between?(0, cols - 1) && maze_lines[nr][nc] != '#'
      new_cost = cost + 1
      if new_cost < dist[nr][nc][d]
        dist[nr][nc][d] = new_cost
        pq.push([new_cost, nr, nc, d])
      end
    end

    left_dir = (d - 1) % 4
    new_cost = cost + 1000
    if new_cost < dist[r][c][left_dir]
      dist[r][c][left_dir] = new_cost
      pq.push([new_cost, r, c, left_dir])
    end

    right_dir = (d + 1) % 4
    new_cost = cost + 1000
    if new_cost < dist[r][c][right_dir]
      dist[r][c][right_dir] = new_cost
      pq.push([new_cost, r, c, right_dir])
    end
  end

  nil
end

# Part 2: Count tiles on at least one best path
def solve_part2(maze_lines)
  directions = [
    [0, 1],   # East
    [1, 0],   # South
    [0, -1],  # West
    [-1, 0]   # North
  ]

  rows = maze_lines.length
  cols = maze_lines[0].length

  start = nil
  finish = nil
  rows.times do |r|
    cols.times do |c|
      case maze_lines[r][c]
      when 'S'
        start = [r, c]
      when 'E'
        finish = [r, c]
      end
    end
  end
  raise 'Could not find "S" or "E" in the maze.' if start.nil? || finish.nil?

  inf = Float::INFINITY
  dist = Array.new(rows) { Array.new(cols) { Array.new(4, inf) } }
  start_dir = 0
  dist[start[0]][start[1]][start_dir] = 0

  pq = [[0, start[0], start[1], start_dir]]
  visited = Set.new

  until pq.empty?
    pq.sort_by!(&:first)
    cost, r, c, d = pq.shift

    next if visited.include?([r, c, d])
    visited.add([r, c, d])

    current_dist = dist[r][c][d]
    next if cost > current_dist

    dr, dc = directions[d]
    nr, nc = r + dr, c + dc
    if nr.between?(0, rows - 1) && nc.between?(0, cols - 1) && maze_lines[nr][nc] != '#'
      new_cost = cost + 1
      if new_cost < dist[nr][nc][d]
        dist[nr][nc][d] = new_cost
        pq.push([new_cost, nr, nc, d])
      end
    end

    left_dir = (d - 1) % 4
    new_cost = cost + 1000
    if new_cost < dist[r][c][left_dir]
      dist[r][c][left_dir] = new_cost
      pq.push([new_cost, r, c, left_dir])
    end

    right_dir = (d + 1) % 4
    new_cost = cost + 1000
    if new_cost < dist[r][c][right_dir]
      dist[r][c][right_dir] = new_cost
      pq.push([new_cost, r, c, right_dir])
    end
  end

  min_cost_end = dist[finish[0]][finish[1]].min
  return 0 if min_cost_end == inf

  on_best_path = Array.new(rows) { Array.new(cols, false) }
  queue = Deque.new
  4.times { |d| queue.push([finish[0], finish[1], d]) if dist[finish[0]][finish[1]][d] == min_cost_end }

  visited_rev = Set.new(queue.to_a)

  until queue.empty?
    r, c, d = queue.shift
    on_best_path[r][c] = true

    cost_here = dist[r][c][d]
    dr, dc = directions[d]
    r_prev, c_prev = r - dr, c - dc
    if r_prev.between?(0, rows - 1) && c_prev.between?(0, cols - 1) && maze_lines[r_prev][c_prev] != '#'
      if dist[r_prev][c_prev][d] == cost_here - 1 && !visited_rev.include?([r_prev, c_prev, d])
        visited_rev.add([r_prev, c_prev, d])
        queue.push([r_prev, c_prev, d])
      end
    end

    [-1, 1].each do |offset|
      d_pre = (d + offset) % 4
      if dist[r][c][d_pre] == cost_here - 1000 && !visited_rev.include?([r, c, d_pre])
        visited_rev.add([r, c, d_pre])
        queue.push([r, c, d_pre])
      end
    end
  end

  on_best_path.flatten.count(true)
end

# Main program
def main
  lines = File.readlines('\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-016\\input.txt', chomp: true)
  part1 = solve_maze(lines)
  puts "Lowest possible score: #{part1}"

  part2 = solve_part2(lines)
  puts "Number of tiles on at least one best path: #{part2}"
end

if __FILE__ == $PROGRAM_NAME
  main
end
