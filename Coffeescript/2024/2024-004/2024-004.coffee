fs = require 'fs'

# Function to read the grid from the input file
readGrid = (filePath) ->
  data = fs.readFileSync(filePath, 'utf8')
  data.trim().split('\n').map (line) -> line.split ''

# Define the target word and movement directions
targetWord = "XMAS"
directions = [
  [0, 1]    # Right
  [1, 0]    # Down
  [1, 1]    # Diagonal-right-down
  [1, -1]   # Diagonal-left-down
  [0, -1]   # Left
  [-1, 0]   # Up
  [-1, -1]  # Diagonal-left-up
  [-1, 1]   # Diagonal-right-up
]

# Function to check if the word exists in a given direction
checkWord = (grid, x, y, dx, dy) ->
  rows = grid.length
  cols = grid[0].length
  for i in [0...targetWord.length]
    nx = x + i * dx
    ny = y + i * dy
    return false if nx < 0 or ny < 0 or nx >= rows or ny >= cols
    return false if grid[nx][ny] != targetWord[i]
  return true

# Function to count all occurrences of the target word
countOccurrences = (grid) ->
  count = 0
  rows = grid.length
  cols = grid[0].length
  for x in [0...rows]
    for y in [0...cols]
      for [dx, dy] in directions
        count++ if checkWord(grid, x, y, dx, dy)
  count

# Function to count all X-MAS patterns
countXmasPatterns = (grid) ->
  count = 0
  rows = grid.length
  cols = grid[0].length
  for x in [1...(rows - 1)]
    for y in [1...(cols - 1)]
      center = grid[x][y]
      topLeft = grid[x - 1][y - 1]
      topRight = grid[x - 1][y + 1]
      bottomLeft = grid[x + 1][y - 1]
      bottomRight = grid[x + 1][y + 1]
      if center == "A"
        if (topLeft == "M" and topRight == "S" and bottomLeft == "M" and bottomRight == "S") or
           (topLeft == "S" and topRight == "M" and bottomLeft == "S" and bottomRight == "M") or
           (topLeft == "M" and topRight == "M" and bottomLeft == "S" and bottomRight == "S") or
           (topLeft == "S" and topRight == "S" and bottomLeft == "M" and bottomRight == "M")
          count++
  count

# Main execution
main = ->
  filePath = '/uploads/input.txt'
  grid = readGrid(filePath)
  console.log "Count of XMAS occurrences: #{countOccurrences(grid)}"
  console.log "Total XMAS patterns: #{countXmasPatterns(grid)}"

main()
