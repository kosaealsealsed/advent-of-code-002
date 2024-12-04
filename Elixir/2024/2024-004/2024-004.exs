defmodule WordSearch do
  @directions [
    {0, 1},   # Right
    {1, 0},   # Down
    {1, 1},   # Diagonal-right-down
    {1, -1},  # Diagonal-left-down
    {0, -1},  # Left
    {-1, 0},  # Up
    {-1, -1}, # Diagonal-left-up
    {-1, 1}   # Diagonal-right-up
  ]

  @target_word "XMAS"

  def read_grid(file_path) do
    File.read!(file_path)
    |> String.split("\n", trim: true)
    |> Enum.map(&String.graphemes/1)
  end

  def check_word(grid, word, x, y, dx, dy) do
    Enum.all?(0..(String.length(word) - 1), fn i ->
      nx = x + i * dx
      ny = y + i * dy

      if nx < 0 or ny < 0 or nx >= length(grid) or ny >= length(Enum.at(grid, 0)) do
        false
      else
        Enum.at(grid, nx) |> Enum.at(ny) == String.at(word, i)
      end
    end)
  end

  def count_occurrences(grid, word) do
    for x <- 0..(length(grid) - 1),
        y <- 0..(length(Enum.at(grid, 0)) - 1),
        {dx, dy} <- @directions,
        reduce: 0 do
      count ->
        if check_word(grid, word, x, y, dx, dy), do: count + 1, else: count
    end
  end

  def count_xmas_patterns(grid) do
    for x <- 1..(length(grid) - 2),
        y <- 1..(length(Enum.at(grid, 0)) - 2),
        reduce: 0 do
      count ->
        center = Enum.at(grid, x) |> Enum.at(y)
        top_left = Enum.at(grid, x - 1) |> Enum.at(y - 1)
        top_right = Enum.at(grid, x - 1) |> Enum.at(y + 1)
        bottom_left = Enum.at(grid, x + 1) |> Enum.at(y - 1)
        bottom_right = Enum.at(grid, x + 1) |> Enum.at(y + 1)

        if center == "A" and
             ((top_left == "M" and top_right == "S" and bottom_left == "M" and bottom_right == "S") or
              (top_left == "S" and top_right == "M" and bottom_left == "S" and bottom_right == "M") or
              (top_left == "M" and top_right == "M" and bottom_left == "S" and bottom_right == "S") or
              (top_left == "S" and top_right == "S" and bottom_left == "M" and bottom_right == "M")) do
          count + 1
        else
          count
        end
    end
  end

  def main do
    file_path = "/uploads/input.txt"
    grid = read_grid(file_path)

    xmas_count = count_occurrences(grid, @target_word)
    IO.puts("Count of XMAS occurrences: #{xmas_count}")

    pattern_count = count_xmas_patterns(grid)
    IO.puts("Total XMAS patterns: #{pattern_count}")
  end
end

WordSearch.main()
