#!/usr/bin/env ruby

require 'set'

# GuardPatrolLoopDetector class encapsulates all functionalities
class GuardPatrolLoopDetector
  # Direction mappings
  DIRECTION_MAP = {
    '^' => 0,
    '>' => 1,
    'v' => 2,
    '<' => 3
  }.freeze

  # Direction offsets: Up, Right, Down, Left
  DIRECTION_OFFSETS = [
    [-1, 0],  # Up
    [0, 1],   # Right
    [1, 0],   # Down
    [0, -1]   # Left
  ].freeze

  # Position class to represent a position in the grid
  Position = Struct.new(:row, :col) do
    def eql?(other)
      self.row == other.row && self.col == other.col
    end

    def hash
      [row, col].hash
    end
  end

  # State class to represent the state of the guard (position and direction)
  State = Struct.new(:row, :col, :direction) do
    def eql?(other)
      self.row == other.row && self.col == other.col && self.direction == other.direction
    end

    def hash
      [row, col, direction].hash
    end
  end

  # Pair class to hold two related objects
  Pair = Struct.new(:first, :second)

  # Entry point of the program
  def self.run
    file_path = '\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-006\\input.txt' # Specify the path to your input file here

    begin
      # Part 1: Count distinct positions visited without obstructions
      distinct_positions = count_distinct_positions_visited(file_path)
      puts "Number of distinct positions visited: #{distinct_positions}"

      # Part 2: Detect loops with obstructions and measure execution times
      count_obstruction_positions(file_path)
    rescue => e
      STDERR.puts "Error: #{e.message}"
    end
  end

  # Part 1: Counts the number of distinct positions visited by the guard without any obstructions.
  def self.count_distinct_positions_visited(file_path)
    # Parse the grid
    grid = parse_grid(file_path)

    # Find the guard's starting position and direction
    guard_info = find_guard(grid)
    guard_pos = guard_info.first
    guard_dir = guard_info.second

    # Initialize visited positions set
    visited_positions = Set.new
    visited_positions.add(guard_pos)

    # Simulate the guard's movement
    loop do
      dr, dc = DIRECTION_OFFSETS[guard_dir]
      new_r = guard_pos.row + dr
      new_c = guard_pos.col + dc

      # Check boundaries
      if new_r < 0 || new_r >= grid.size || new_c < 0 || new_c >= grid[0].size
        break # Guard exits the mapped area
      end

      if grid[new_r][new_c] == '#'
        # Turn right if obstacle ahead
        guard_dir = (guard_dir + 1) % 4
      else
        # Move forward
        guard_pos = Position.new(new_r, new_c)
        visited_positions.add(guard_pos)
      end
    end

    # Number of distinct positions visited
    visited_positions.size
  end

  # Part 2: Counts the number of obstruction positions that cause the guard to loop indefinitely.
  # Also measures and prints execution times.
  def self.count_obstruction_positions(file_path)
    # Start total timing
    total_start_time = Process.clock_gettime(Process::CLOCK_MONOTONIC)

    # Parse the grid
    grid = parse_grid(file_path)

    # Find the guard's starting position and direction
    guard_info = find_guard(grid)
    guard_pos = guard_info.first
    guard_dir = guard_info.second

    # Time to find obstruction positions
    obstruction_start_time = Process.clock_gettime(Process::CLOCK_MONOTONIC)
    possible_obstructions = get_possible_obstructions(grid, guard_pos)
    obstruction_end_time = Process.clock_gettime(Process::CLOCK_MONOTONIC)
    obstruction_time = obstruction_end_time - obstruction_start_time

    # Print the first header and line: [time_obstruction_positions] [total_obstruction_positions]
    puts "time, denominator"
    printf("%.9f %d\n", obstruction_time, possible_obstructions.size)

    # Print header for batches
    puts "batch, batch time, cumulative time"

    # Initialize loop counter
    loop_count = 0
    total = possible_obstructions.size

    # Initialize timing for batches
    batch_size = 1000
    batch_start_time = Process.clock_gettime(Process::CLOCK_MONOTONIC)
    cumulative_time = obstruction_time # cumulative_time includes obstruction_time

    possible_obstructions.each_with_index do |obstruction, idx|
      grid[obstruction.row][obstruction.col] = '#' # Place obstruction

      if simulate_movement(grid, guard_pos, guard_dir)
        loop_count += 1 # Found a position that causes a loop
      end

      grid[obstruction.row][obstruction.col] = '.' # Remove obstruction

      # Check if batch size is reached or it's the last position
      if ((idx + 1) % batch_size == 0) || ((idx + 1) == total)
        batch_end_time = Process.clock_gettime(Process::CLOCK_MONOTONIC)
        batch_time = batch_end_time - batch_start_time
        cumulative_time += batch_time
        printf("%d %.9f %.9f\n", idx + 1, batch_time, cumulative_time)
        batch_start_time = Process.clock_gettime(Process::CLOCK_MONOTONIC) # Reset batch start time
      end
    end

    # End total timing
    total_end_time = Process.clock_gettime(Process::CLOCK_MONOTONIC)
    total_time = total_end_time - total_start_time # Total time from start to end

    # Print final answer header and line: [answer] [answer_time]
    puts "answer, answer time"
    printf("%d %.9f\n", loop_count, total_time)
  end

  private

  # Parses the grid from the given file.
  def self.parse_grid(file_path)
    grid_list = []
    File.foreach(file_path) do |line|
      grid_list << line.strip.chars
    end

    if grid_list.empty?
      raise ArgumentError, "The grid is empty."
    end

    # Ensure all rows have the same length
    cols = grid_list.first.size
    grid_list.each_with_index do |row, index|
      unless row.size == cols
        raise ArgumentError, "Inconsistent row lengths in the grid at row #{index}."
      end
    end

    grid_list
  end

  # Finds the guard's starting position and direction.
  def self.find_guard(grid)
    grid.each_with_index do |row, r|
      row.each_with_index do |cell, c|
        if DIRECTION_MAP.key?(cell)
          guard_pos = Position.new(r, c)
          guard_dir = DIRECTION_MAP[cell]
          grid[r][c] = '.' # Clear the starting position
          return Pair.new(guard_pos, guard_dir)
        end
      end
    end
    raise ArgumentError, "Guard not found in the grid."
  end

  # Retrieves all possible obstruction positions excluding the guard's starting position and already obstructed cells.
  def self.get_possible_obstructions(grid, guard_pos)
    possible = []
    grid.each_with_index do |row, r|
      row.each_with_index do |cell, c|
        if (r != guard_pos.row || c != guard_pos.col) && cell == '.'
          possible << Position.new(r, c)
        end
      end
    end
    possible
  end

  # Simulates the guard's movement on the grid.
  # Returns true if a loop is detected, false if the guard exits the grid.
  def self.simulate_movement(grid, start_pos, start_dir)
    visited_states = Set.new
    r = start_pos.row
    c = start_pos.col
    direction = start_dir

    loop do
      current_state = State.new(r, c, direction)
      return true if visited_states.include?(current_state)

      visited_states.add(current_state)

      dr, dc = DIRECTION_OFFSETS[direction]
      new_r = r + dr
      new_c = c + dc

      # Check boundaries
      if new_r < 0 || new_r >= grid.size || new_c < 0 || new_c >= grid[0].size
        return false # Guard exits the grid
      end

      if grid[new_r][new_c] == '#'
        # Turn right if obstacle ahead
        direction = (direction + 1) % 4
      else
        # Move forward
        r = new_r
        c = new_c
      end
    end
  end
end

# Execute the program
GuardPatrolLoopDetector.run if __FILE__ == $PROGRAM_NAME
