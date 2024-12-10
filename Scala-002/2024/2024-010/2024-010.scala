import scala.io.Source
import scala.collection.mutable
import java.time.Duration
import java.time.Instant

object AdventOfCode2024_010 {

  val INPUT_FILE = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-010\\input.txt"

  def readMap(filename: String): Array[Array[Int]] = {
    Source.fromFile(filename)
      .getLines()
      .filter(_.nonEmpty)
      .map(_.trim.map(_.asDigit).toArray)
      .toArray
  }

  def neighbors(r: Int, c: Int, rows: Int, cols: Int): Seq[(Int, Int)] = {
    Seq((-1, 0), (1, 0), (0, -1), (0, 1)).flatMap { case (dr, dc) =>
      val nr = r + dr
      val nc = c + dc
      if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) Some((nr, nc)) else None
    }
  }

  def findTrailheadScores(grid: Array[Array[Int]]): Int = {
    val rows = grid.length
    val cols = if (rows > 0) grid(0).length else 0

    val trailheads = for {
      r <- 0 until rows
      c <- 0 until cols
      if grid(r)(c) == 0
    } yield (r, c)

    var totalScore = 0

    for ((startR, startC) <- trailheads) {
      val visited = mutable.Set[(Int, Int)]()
      val queue = mutable.Queue((startR, startC))
      val reachableNines = mutable.Set[(Int, Int)]()

      visited.add((startR, startC))

      while (queue.nonEmpty) {
        val (r, c) = queue.dequeue()
        val currentHeight = grid(r)(c)

        if (currentHeight == 9) {
          reachableNines.add((r, c))
        } else {
          val nextHeight = currentHeight + 1
          for ((nr, nc) <- neighbors(r, c, rows, cols)) {
            if (!visited.contains((nr, nc)) && grid(nr)(nc) == nextHeight) {
              visited.add((nr, nc))
              queue.enqueue((nr, nc))
            }
          }
        }
      }

      totalScore += reachableNines.size
    }

    totalScore
  }

  def countPaths(r: Int, c: Int, grid: Array[Array[Int]], dp: Array[Array[Option[Int]]], rows: Int, cols: Int): Int = {
    dp(r)(c) match {
      case Some(value) => value
      case None =>
        val currentHeight = grid(r)(c)

        if (currentHeight == 9) {
          dp(r)(c) = Some(1)
          1
        } else {
          val nextHeight = currentHeight + 1
          val totalPaths = neighbors(r, c, rows, cols)
            .filter { case (nr, nc) => grid(nr)(nc) == nextHeight }
            .map { case (nr, nc) => countPaths(nr, nc, grid, dp, rows, cols) }
            .sum

          dp(r)(c) = Some(totalPaths)
          totalPaths
        }
    }
  }

  def calculateTotalRating(grid: Array[Array[Int]]): Int = {
    val rows = grid.length
    val cols = if (rows > 0) grid(0).length else 0

    val trailheads = for {
      r <- 0 until rows
      c <- 0 until cols
      if grid(r)(c) == 0
    } yield (r, c)

    val dp = Array.fill(rows, cols)(Option.empty[Int])

    trailheads.map { case (tr, tc) =>
      countPaths(tr, tc, grid, dp, rows, cols)
    }.sum
  }

  def main(args: Array[String]): Unit = {
    val startTimePart1 = Instant.now()
    val grid = readMap(INPUT_FILE)
    val totalScore = findTrailheadScores(grid)
    val endTimePart1 = Instant.now()

    println(s"Part 1 Result: $totalScore")
    println(f"Time taken for Part 1: ${Duration.between(startTimePart1, endTimePart1).toMillis / 1000.0}%.9f s")

    val startTimePart2 = Instant.now()
    val totalRating = calculateTotalRating(grid)
    val endTimePart2 = Instant.now()

    println(s"Part 2 Result: $totalRating")
    println(f"Time taken for Part 2: ${Duration.between(startTimePart2, endTimePart2).toMillis / 1000.0}%.9f s")
  }
}
