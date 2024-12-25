require 'set'

# Main class for computing antinodes using two methods: pairwise and line-drawing.
class AntinodeCalculator
  # Reads the grid map from a file.
  #
  # @param filename [String] The name of the file containing the grid map.
  # @return [Array<String>] An array of strings representing the grid.
  def self.read_map(filename)
    File.readlines(filename, chomp: true)
  end

  # Computes unique antinode positions using the pairwise method.
  #
  # @param grid [Array<String>] An array of strings representing the grid map.
  # @return [Set<Array<Integer>>] A set of unique antinode positions.
  def self.compute_antinodes_pairwise(grid)
    rows = grid.size
    cols = rows.positive? ? grid[0].size : 0

    # Group antennas by frequency
    antennas_by_freq = Hash.new { |h, k| h[k] = [] }
    grid.each_with_index do |row, r|
      row.each_char.with_index do |ch, c|
        antennas_by_freq[ch] << [r, c] unless ch == '.'
      end
    end

    antinodes = Set.new

    antennas_by_freq.each_value do |coords|
      n = coords.size
      next if n < 2

      coords.each_with_index do |a, i|
        r_a, c_a = a
        (i + 1...n).each do |j|
          b = coords[j]
          r_b, c_b = b

          # Compute P1 = 2B - A
          p1_r, p1_c = 2 * r_b - r_a, 2 * c_b - c_a
          antinodes.add([p1_r, p1_c]) if p1_r.between?(0, rows - 1) && p1_c.between?(0, cols - 1)

          # Compute P2 = 2A - B
          p2_r, p2_c = 2 * r_a - r_b, 2 * c_a - c_b
          antinodes.add([p2_r, p2_c]) if p2_r.between?(0, rows - 1) && p2_c.between?(0, cols - 1)
        end
      end
    end

    antinodes
  end

  # Computes unique antinode positions using the line-drawing method.
  #
  # @param grid [Array<String>] An array of strings representing the grid map.
  # @return [Set<Array<Integer>>] A set of unique antinode positions.
  def self.compute_antinodes_lines(grid)
    rows = grid.size
    cols = rows.positive? ? grid[0].size : 0

    # Group antennas by frequency
    antennas_by_freq = Hash.new { |h, k| h[k] = [] }
    grid.each_with_index do |row, r|
      row.each_char.with_index do |ch, c|
        antennas_by_freq[ch] << [r, c] unless ch == '.'
      end
    end

    antinodes = Set.new

    antennas_by_freq.each_value do |coords|
      n = coords.size
      next if n < 2

      coords.each_with_index do |a, i|
        r_a, c_a = a
        (i + 1...n).each do |j|
          r_b, c_b = coords[j]
          add_line_points(r_a, c_a, r_b, c_b, rows, cols, antinodes)
        end
      end
    end

    antinodes
  end

  # Adds all points along a line between two antennas to the antinodes set.
  #
  # @param r_a [Integer] Row of the first antenna.
  # @param c_a [Integer] Column of the first antenna.
  # @param r_b [Integer] Row of the second antenna.
  # @param c_b [Integer] Column of the second antenna.
  # @param rows [Integer] Number of rows in the grid.
  # @param cols [Integer] Number of columns in the grid.
  # @param antinodes [Set<Array<Integer>>] Set to store unique antinode positions.
  def self.add_line_points(r_a, c_a, r_b, c_b, rows, cols, antinodes)
    dr = r_b - r_a
    dc = c_b - c_a
    g = gcd(dr.abs, dc.abs)
    dr /= g
    dc /= g

    # Add points in the forward direction
    r_p, c_p = r_a, c_a
    while r_p.between?(0, rows - 1) && c_p.between?(0, cols - 1)
      antinodes.add([r_p, c_p])
      r_p += dr
      c_p += dc
    end

    # Add points in the backward direction
    r_p, c_p = r_a - dr, c_a - dc
    while r_p.between?(0, rows - 1) && c_p.between?(0, cols - 1)
      antinodes.add([r_p, c_p])
      r_p -= dr
      c_p -= dc
    end
  end

  # Computes the greatest common divisor of two integers.
  #
  # @param a [Integer] First integer.
  # @param b [Integer] Second integer.
  # @return [Integer] The greatest common divisor of a and b.
  def self.gcd(a, b)
    b.zero? ? a : gcd(b, a % b)
  end

  # Main function to run the program.
  #
  # @param filename [String] Input file name containing the grid.
  def self.main(filename)
    grid = read_map(filename)

    overall_start = Time.now

    # Part 1: Compute using Pairwise method
    start_time = Time.now
    pairwise_antinodes = compute_antinodes_pairwise(grid)
    part1_time = Time.now - start_time
    puts "Part 001 finished in #{format_time(part1_time)}"
    puts "Number of unique antinodes (Pairwise method): #{pairwise_antinodes.size}"

    # Part 2: Compute using Line-drawing method
    start_time = Time.now
    line_antinodes = compute_antinodes_lines(grid)
    part2_time = Time.now - start_time
    puts "Part 002 finished in #{format_time(part2_time)}"
    puts "Number of unique antinodes (Line-drawing method): #{line_antinodes.size}"

    # Compute total computation time
    computation_total_time = part1_time + part2_time
    puts "Total computation time: #{format_time(computation_total_time)}"

    # Compute overall runtime (includes overhead)
    overall_total_time = Time.now - overall_start
    puts "Overall total time: #{format_time(overall_total_time)}"
  end

  # Formats the elapsed time as a human-readable string.
  #
  # @param seconds [Float] The elapsed time in seconds.
  # @return [String] The formatted time as a string.
  def self.format_time(seconds)
    format('%.9f s', seconds)
  end
end

# Run the program with the input file
AntinodeCalculator.main('\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-008\\input.txt')
