import scala.collection.mutable
import scala.io.Source
import scala.collection.mutable.{ArrayDeque, Set}

object MazeSolver {
  // Directions: 0=East, 1=South, 2=West, 3=North
  val directions = Array(
    (0, 1),   // East
    (1, 0),   // South
    (0, -1),  // West
    (-1, 0)   // North
  )

  def parseInput(filename: String): Array[String] = {
    Source.fromFile(filename).getLines().toArray
  }

  def solveMaze(mazeLines: Array[String]): Option[Int] = {
    val rows = mazeLines.length
    val cols = mazeLines(0).length

    // Find S and E
    var start: Option[(Int, Int)] = None
    var end: Option[(Int, Int)] = None

    for (r <- 0 until rows; c <- 0 until cols) {
      mazeLines(r)(c) match {
        case 'S' => start = Some((r, c))
        case 'E' => end = Some((r, c))
        case _   =>
      }
    }

    if (start.isEmpty || end.isEmpty) {
      throw new IllegalArgumentException("Could not find 'S' or 'E' in the maze.")
    }

    val (startRow, startCol) = start.get
    val (endRow, endCol) = end.get

    // Dijkstra's algorithm
    val INF = Int.MaxValue
    val dist = Array.fill(rows, cols, 4)(INF)
    val startDir = 0 // Facing East
    dist(startRow)(startCol)(startDir) = 0

    val pq = mutable.PriorityQueue[(Int, Int, Int, Int)]()(Ordering.by(-_._1))
    pq.enqueue((0, startRow, startCol, startDir))

    val visited = mutable.Set[(Int, Int, Int)]()

    while (pq.nonEmpty) {
      val (cost, r, c, d) = pq.dequeue()

      // Skip already visited states
      if (visited.contains((r, c, d))) {
        // Skip to the next iteration
      } else {
        visited.add((r, c, d))

        // If we've reached the end, return the cost
        if ((r, c) == (endRow, endCol)) {
          return Some(cost)
        }

        // Explore neighbors:
        // 1) Move forward (cost + 1) if not a wall
        val (dr, dc) = directions(d)
        val nr = r + dr
        val nc = c + dc
        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && mazeLines(nr)(nc) != '#') {
          val newCost = cost + 1
          if (newCost < dist(nr)(nc)(d)) {
            dist(nr)(nc)(d) = newCost
            pq.enqueue((newCost, nr, nc, d))
          }
        }

        // 2) Turn left (cost + 1000)
        val leftDir = (d + 3) % 4
        val leftCost = cost + 1000
        if (leftCost < dist(r)(c)(leftDir)) {
          dist(r)(c)(leftDir) = leftCost
          pq.enqueue((leftCost, r, c, leftDir))
        }

        // 3) Turn right (cost + 1000)
        val rightDir = (d + 1) % 4
        val rightCost = cost + 1000
        if (rightCost < dist(r)(c)(rightDir)) {
          dist(r)(c)(rightDir) = rightCost
          pq.enqueue((rightCost, r, c, rightDir))
        }
      }
    }

    None // If no path is found
  }

  def solvePart2(mazeLines: Array[String]): Int = {
    val rows = mazeLines.length
    val cols = mazeLines(0).length

    var start: Option[(Int, Int)] = None
    var end: Option[(Int, Int)] = None

    // Identify 'S' and 'E'
    for (r <- 0 until rows; c <- 0 until cols) {
      mazeLines(r)(c) match {
        case 'S' => start = Some((r, c))
        case 'E' => end = Some((r, c))
        case _   =>
      }
    }

    if (start.isEmpty || end.isEmpty) {
      throw new IllegalArgumentException("Could not find 'S' or 'E' in the maze.")
    }

    val (startRow, startCol) = start.get
    val (endRow, endCol) = end.get

    val INF = Int.MaxValue
    val dist = Array.fill(rows, cols, 4)(INF)
    val startDir = 0 // Facing East
    dist(startRow)(startCol)(startDir) = 0

    val pq = mutable.PriorityQueue[(Int, Int, Int, Int)]()(Ordering.by(-_._1))
    pq.enqueue((0, startRow, startCol, startDir))

    val visited = mutable.Set[(Int, Int, Int)]()

    while (pq.nonEmpty) {
      val (cost, r, c, d) = pq.dequeue()

      if (visited.contains((r, c, d))) {
        // Skip already visited states
      } else {
        visited.add((r, c, d))

        // Move forward
        val (dr, dc) = directions(d)
        val nr = r + dr
        val nc = c + dc
        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && mazeLines(nr)(nc) != '#') {
          val newCost = cost + 1
          if (newCost < dist(nr)(nc)(d)) {
            dist(nr)(nc)(d) = newCost
            pq.enqueue((newCost, nr, nc, d))
          }
        }

        // Turn left
        val leftDir = (d + 3) % 4
        val leftCost = cost + 1000
        if (leftCost < dist(r)(c)(leftDir)) {
          dist(r)(c)(leftDir) = leftCost
          pq.enqueue((leftCost, r, c, leftDir))
        }

        // Turn right
        val rightDir = (d + 1) % 4
        val rightCost = cost + 1000
        if (rightCost < dist(r)(c)(rightDir)) {
          dist(r)(c)(rightDir) = rightCost
          pq.enqueue((rightCost, r, c, rightDir))
        }
      }
    }

    // Find minimal cost to reach the end
    val minCostEnd = (0 until 4).map(d => dist(endRow)(endCol)(d)).min
    if (minCostEnd == INF) {
      return 0
    }

    // Mark cells on the best path
    val onBestPath = Array.fill(rows, cols)(false)

    val queue = ArrayDeque[(Int, Int, Int)]()
    for (d <- 0 until 4 if dist(endRow)(endCol)(d) == minCostEnd) {
      queue.append((endRow, endCol, d))
    }

    val visitedRev = mutable.Set[(Int, Int, Int)]()
    queue.foreach(visitedRev.add)

    while (queue.nonEmpty) {
      val (r, c, d) = queue.removeHead()
      onBestPath(r)(c) = true

      val costHere = dist(r)(c)(d)

      // Predecessor by moving forward
      val (dr, dc) = directions(d)
      val rPrev = r - dr
      val cPrev = c - dc
      if (rPrev >= 0 && rPrev < rows && cPrev >= 0 && cPrev < cols && mazeLines(rPrev)(cPrev) != '#') {
        if (dist(rPrev)(cPrev)(d) == costHere - 1 && !visitedRev.contains((rPrev, cPrev, d))) {
          visitedRev.add((rPrev, cPrev, d))
          queue.append((rPrev, cPrev, d))
        }
      }

      // Predecessor by turning
      for (dPrev <- Seq((d + 3) % 4, (d + 1) % 4)) {
        if (dist(r)(c)(dPrev) == costHere - 1000 && !visitedRev.contains((r, c, dPrev))) {
          visitedRev.add((r, c, dPrev))
          queue.append((r, c, dPrev))
        }
      }
    }

    // Count tiles on at least one best path
    onBestPath.flatten.count(_ == true)
  }

  def main(args: Array[String]): Unit = {
    val lines = parseInput("\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-016\\input.txt")
    solveMaze(lines) match {
      case Some(answer) => println(s"Lowest possible score: $answer")
      case None         => println("No path found!")
    }

    val result = solvePart2(lines)
    println(s"Number of tiles on at least one best path: $result")
  }
}
