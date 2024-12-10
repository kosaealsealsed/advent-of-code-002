INPUT_FILE = '\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-010\\input.txt'

def read_map(filename)
  grid = []
  File.readlines(filename).each do |line|
    line.strip!
    next if line.empty?
    grid << line.chars.map(&:to_i)
  end
  grid
end

def neighbors(r, c, rows, cols)
  directions = [[-1, 0], [1, 0], [0, -1], [0, 1]]
  directions.each_with_object([]) do |(dr, dc), result|
    nr, nc = r + dr, c + dc
    result << [nr, nc] if nr.between?(0, rows - 1) && nc.between?(0, cols - 1)
  end
end

def find_trailhead_scores(grid)
  rows = grid.size
  cols = rows > 0 ? grid[0].size : 0

  trailheads = []
  rows.times do |r|
    cols.times do |c|
      trailheads << [r, c] if grid[r][c] == 0
    end
  end

  total_score = 0

  trailheads.each do |start_r, start_c|
    visited = {}
    queue = [[start_r, start_c]]
    visited[[start_r, start_c]] = true

    reachable_nines = {}

    until queue.empty?
      r, c = queue.shift
      current_height = grid[r][c]

      if current_height == 9
        reachable_nines[[r, c]] = true
      else
        next_height = current_height + 1
        neighbors(r, c, rows, cols).each do |nr, nc|
          next if visited[[nr, nc]] || grid[nr][nc] != next_height
          visited[[nr, nc]] = true
          queue << [nr, nc]
        end
      end
    end

    total_score += reachable_nines.keys.size
  end

  total_score
end

def count_paths(r, c, grid, dp, rows, cols)
  return dp[r][c] unless dp[r][c].nil?

  current_height = grid[r][c]

  if current_height == 9
    dp[r][c] = 1
    return 1
  end

  total_paths = 0
  next_height = current_height + 1
  neighbors(r, c, rows, cols).each do |nr, nc|
    if grid[nr][nc] == next_height
      total_paths += count_paths(nr, nc, grid, dp, rows, cols)
    end
  end

  dp[r][c] = total_paths
  total_paths
end

def calculate_total_rating(grid)
  rows = grid.size
  cols = rows > 0 ? grid[0].size : 0

  trailheads = []
  rows.times do |r|
    cols.times do |c|
      trailheads << [r, c] if grid[r][c] == 0
    end
  end

  dp = Array.new(rows) { Array.new(cols) }

  total_rating = 0
  trailheads.each do |tr, tc|
    total_rating += count_paths(tr, tc, grid, dp, rows, cols)
  end

  total_rating
end

def main
  start_time_part1 = Time.now
  grid = read_map(INPUT_FILE)
  total_score = find_trailhead_scores(grid)
  end_time_part1 = Time.now
  puts "Part 1 Result: #{total_score}"
  puts "Time taken for Part 1: #{(end_time_part1 - start_time_part1).round(9)} s"

  start_time_part2 = Time.now
  total_rating = calculate_total_rating(grid)
  end_time_part2 = Time.now
  puts "Part 2 Result: #{total_rating}"
  puts "Time taken for Part 2: #{(end_time_part2 - start_time_part2).round(9)} s"
end

main
