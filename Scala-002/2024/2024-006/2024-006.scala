package com.peter_burbery.solve_advent_of_code_with_java_002.year_2024.day_006

import scala.io.Source
import scala.util.{Using, Try}
import scala.collection.mutable
import scala.collection.immutable.{Map => ImmutableMap}
import scala.util.control.Breaks.{break, breakable}

/**
 * GuardPatrolLoopDetector object to detect loops in a guard's patrol on a grid.
 */
object GuardPatrolLoopDetector {

  // Direction mappings
  private val DIRECTION_MAP: ImmutableMap[Char, Int] = ImmutableMap(
    '^' -> 0,
    '>' -> 1,
    'v' -> 2,
    '<' -> 3
  )

  private val DIRECTION_OFFSETS: Array[(Int, Int)] = Array(
    (-1, 0), // Up
    (0, 1),  // Right
    (1, 0),  // Down
    (0, -1)  // Left
  )

  def main(args: Array[String]): Unit = {
    val filePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-006\\input.txt" // Specify the path to your input file here

    try {
      // Part 1: Count distinct positions visited without obstructions
      val distinctPositions = countDistinctPositionsVisited(filePath)
      println(s"Number of distinct positions visited: $distinctPositions")

      // Part 2: Detect loops with obstructions and measure execution times
      countObstructionPositions(filePath)
    } catch {
      case e: Exception =>
        System.err.println(s"Error: ${e.getMessage}")
    }
  }

  /**
   * Part 1: Counts the number of distinct positions visited by the guard without any obstructions.
   *
   * @param filePath Path to the input file.
   * @return Number of distinct positions visited.
   */
  private def countDistinctPositionsVisited(filePath: String): Int = {
    // Parse the grid
    val grid = parseGrid(filePath)

    // Find the guard's starting position and direction
    val (guardPos, guardDir) = findGuard(grid)

    // Initialize visited positions set
    val visitedPositions: mutable.Set[Position] = mutable.Set(guardPos)

    var currentPos = guardPos
    var currentDir = guardDir

    // Simulate the guard's movement with breakable
    breakable {
      while (true) {
        val (dr, dc) = DIRECTION_OFFSETS(currentDir)
        val newR = currentPos.row + dr
        val newC = currentPos.col + dc

        // Check boundaries
        if (newR < 0 || newR >= grid.length || newC < 0 || newC >= grid(0).length) {
          break
        }

        if (grid(newR)(newC) == '#') {
          // Turn right if obstacle ahead
          currentDir = (currentDir + 1) % 4
        } else {
          // Move forward
          currentPos = Position(newR, newC)
          visitedPositions += currentPos
        }
      }
    }

    // Number of distinct positions visited
    visitedPositions.size
  }

  /**
   * Part 2: Counts the number of obstruction positions that cause the guard to loop indefinitely.
   * Also measures and prints execution times.
   *
   * @param filePath Path to the input file.
   */
  private def countObstructionPositions(filePath: String): Unit = {
    // Start total timing
    val totalStartTime = System.nanoTime()

    // Parse the grid
    val originalGrid = parseGrid(filePath)

    // Find the guard's starting position and direction
    val (guardPos, guardDir) = findGuard(originalGrid)

    // Time to find obstruction positions
    val obstructionStartTime = System.nanoTime()
    val possibleObstructions = getPossibleObstructions(originalGrid, guardPos)
    val obstructionEndTime = System.nanoTime()
    val obstructionTime = (obstructionEndTime - obstructionStartTime) / 1_000_000_000.0

    // Print the first header and line: [time_obstruction_positions] [total_obstruction_positions]
    println("time, denominator")
    println(f"$obstructionTime%.9f ${possibleObstructions.size}")

    // Print header for batches
    println("batch, batch time, cumulative time")

    // Initialize loop counter
    var loopCount = 0
    val total = possibleObstructions.size

    // Initialize timing for batches
    val batchSize = 1000
    var batchStartTime = System.nanoTime()
    var cumulativeTime = obstructionTime // cumulative_time includes obstruction_time

    for ((obstruction, idx) <- possibleObstructions.zipWithIndex) {
      // Create a mutable copy of the grid
      val grid = originalGrid.map(_.clone())
      grid(obstruction.row)(obstruction.col) = '#' // Place obstruction

      if (simulateMovement(grid, guardPos, guardDir)) {
        loopCount += 1 // Found a position that causes a loop
      }

      // Check if batch size is reached or it's the last position
      if ((idx + 1) % batchSize == 0 || (idx + 1) == total) {
        val batchEndTime = System.nanoTime()
        val batchTime = (batchEndTime - batchStartTime) / 1_000_000_000.0
        cumulativeTime += batchTime
        println(f"${idx + 1}%d $batchTime%.9f $cumulativeTime%.9f")
        batchStartTime = System.nanoTime() // Reset batch start time
      }
    }

    // End total timing
    val totalEndTime = System.nanoTime()
    val totalTime = (totalEndTime - totalStartTime) / 1_000_000_000.0 // Total time from start to end

    // Print final answer header and line: [answer] [answer_time]
    println("answer, answer time")
    println(f"$loopCount%d $totalTime%.9f")
  }

  /**
   * Parses the grid from the given file.
   *
   * @param filePath Path to the input file.
   * @return 2D character array representing the grid.
   */
  private def parseGrid(filePath: String): Array[Array[Char]] = {
    val gridList = mutable.ArrayBuffer[Array[Char]]()

    Using.resource(Source.fromFile(filePath)) { source =>
      for (line <- source.getLines()) {
        gridList += line.trim.toCharArray
      }
    }

    if (gridList.isEmpty) {
      throw new IllegalArgumentException("The grid is empty.")
    }

    // Ensure all rows have the same length
    val cols = gridList.head.length
    if (!gridList.forall(_.length == cols)) {
      throw new IllegalArgumentException("Inconsistent row lengths in the grid.")
    }

    gridList.toArray
  }

  /**
   * Finds the guard's starting position and direction.
   *
   * @param grid 2D character array representing the grid.
   * @return A tuple containing the starting Position and direction.
   */
  private def findGuard(grid: Array[Array[Char]]): (Position, Int) = {
    grid.zipWithIndex.flatMap { case (row, r) =>
      row.zipWithIndex.collect {
        case (cell, c) if DIRECTION_MAP.contains(cell) => (Position(r, c), DIRECTION_MAP(cell))
      }
    }.headOption match {
      case Some((guardPos, guardDir)) =>
        grid(guardPos.row)(guardPos.col) = '.' // Clear the starting position
        (guardPos, guardDir)
      case None =>
        throw new IllegalArgumentException("Guard not found in the grid.")
    }
  }

  /**
   * Retrieves all possible obstruction positions excluding the guard's starting position and already obstructed cells.
   *
   * @param grid     2D character array representing the grid.
   * @param guardPos The starting position of the guard.
   * @return List of possible obstruction Positions.
   */
  private def getPossibleObstructions(grid: Array[Array[Char]], guardPos: Position): List[Position] = {
    val possible = mutable.ListBuffer[Position]()
    for {
      r <- grid.indices
      c <- grid(r).indices
      if (r != guardPos.row || c != guardPos.col) && grid(r)(c) == '.'
    } {
      possible += Position(r, c)
    }
    possible.toList
  }

  /**
   * Simulates the guard's movement on the grid.
   *
   * @param grid     2D character array representing the grid.
   * @param startPos Starting position of the guard.
   * @param startDir Starting direction of the guard.
   * @return True if a loop is detected, False if the guard exits the grid.
   */
  private def simulateMovement(grid: Array[Array[Char]], startPos: Position, startDir: Int): Boolean = {
    val visitedStates: mutable.Set[State] = mutable.Set()
    var r = startPos.row
    var c = startPos.col
    var direction = startDir

    while (true) {
      val currentState = State(r, c, direction)
      if (visitedStates.contains(currentState)) {
        return true // Loop detected
      }
      visitedStates += currentState

      val (dr, dc) = DIRECTION_OFFSETS(direction)
      val newR = r + dr
      val newC = c + dc

      // Check boundaries
      if (newR < 0 || newR >= grid.length || newC < 0 || newC >= grid(0).length) {
        return false // Guard exits the grid
      }

      if (grid(newR)(newC) == '#') {
        // Turn right if obstacle ahead
        direction = (direction + 1) % 4
      } else {
        // Move forward
        r = newR
        c = newC
      }
    }

    false
  }

  /**
   * Case class to represent a position in the grid.
   *
   * @param row Row index.
   * @param col Column index.
   */
  private case class Position(row: Int, col: Int)

  /**
   * Case class to represent a state (position and direction) of the guard.
   *
   * @param row       Row index.
   * @param col       Column index.
   * @param direction Direction index.
   */
  private case class State(row: Int, col: Int, direction: Int)

  // Scala does not require a Pair class as tuples can be used instead.
}
