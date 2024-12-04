import scala.io.Source

object XMASSolver {

  def main(args: Array[String]): Unit = {
    val filePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-004\\input.txt"
    
    // Read the file and store the grid
    val grid = Source.fromFile(filePath).getLines().toArray
    val rows = grid.length
    val cols = grid(0).length

    // Count occurrences of "XMAS"
    val xmasCount = countXMAS(grid, rows, cols)
    println(s"Count of XMAS: $xmasCount")

    // Count all X-MAS patterns
    val xmasPatterns = countAllXMASPatterns(grid, rows, cols)
    println(s"Total X-MAS patterns: $xmasPatterns")
  }

  // Count occurrences of "XMAS" in all directions
  def countXMAS(grid: Array[String], rows: Int, cols: Int): Int = {
    val targetWord = "XMAS"
    val directions = Seq(
      (0, 1),   // Right
      (1, 0),   // Down
      (1, 1),   // Diagonal-right-down
      (1, -1),  // Diagonal-left-down
      (0, -1),  // Left
      (-1, 0),  // Up
      (-1, -1), // Diagonal-left-up
      (-1, 1)   // Diagonal-right-up
    )

    // Helper function to check if the word exists in a given direction
    def checkWord(x: Int, y: Int, dx: Int, dy: Int): Boolean = {
      (0 until targetWord.length).forall { i =>
        val nx = x + i * dx
        val ny = y + i * dy
        nx >= 0 && ny >= 0 && nx < rows && ny < cols && grid(nx)(ny) == targetWord(i)
      }
    }

    // Traverse the grid to count occurrences
    var count = 0
    for {
      r <- 0 until rows
      c <- 0 until cols
      (dx, dy) <- directions
    } if (checkWord(r, c, dx, dy)) count += 1

    count
  }

  // Count all X-MAS patterns
  def countAllXMASPatterns(grid: Array[String], rows: Int, cols: Int): Int = {
    // Traverse the grid ensuring bounds for a 3x3 X-MAS pattern
    def checkPattern(center: Char, topLeft: Char, topRight: Char, bottomLeft: Char, bottomRight: Char): Boolean = {
      (center == 'A' && (
        (topLeft == 'M' && topRight == 'S' && bottomLeft == 'M' && bottomRight == 'S') || // Pattern 1: M.S
        (topLeft == 'S' && topRight == 'M' && bottomLeft == 'S' && bottomRight == 'M') || // Pattern 2: S.M
        (topLeft == 'M' && topRight == 'M' && bottomLeft == 'S' && bottomRight == 'S') || // Pattern 3: M.M
        (topLeft == 'S' && topRight == 'S' && bottomLeft == 'M' && bottomRight == 'M')    // Pattern 4: S.S
      ))
    }

    var count = 0
    for {
      r <- 1 until rows - 1
      c <- 1 until cols - 1
    } {
      val center = grid(r)(c)
      val topLeft = grid(r - 1)(c - 1)
      val topRight = grid(r - 1)(c + 1)
      val bottomLeft = grid(r + 1)(c - 1)
      val bottomRight = grid(r + 1)(c + 1)

      if (checkPattern(center, topLeft, topRight, bottomLeft, bottomRight)) count += 1
    }

    count
  }
}
