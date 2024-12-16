import java.util.PriorityQueue
import java.util.ArrayDeque

data class State(val cost: Int, val row: Int, val col: Int, val direction: Int): Comparable<State> {
    override fun compareTo(other: State): Int {
        return this.cost - other.cost
    }
}

fun solveMaze(mazeLines: List<String>): Int? {
    // Directions: 0=East, 1=South, 2=West, 3=North
    val directions = listOf(
        Pair(0, 1),   // East
        Pair(1, 0),   // South
        Pair(0, -1),  // West
        Pair(-1, 0)   // North
    )

    val rows = mazeLines.size
    val cols = mazeLines[0].length

    var start: Pair<Int, Int>? = null
    var end: Pair<Int, Int>? = null

    for (r in 0 until rows) {
        for (c in 0 until cols) {
            when (mazeLines[r][c]) {
                'S' -> start = Pair(r, c)
                'E' -> end = Pair(r, c)
            }
        }
    }

    if (start == null || end == null) {
        throw IllegalArgumentException("Could not find 'S' or 'E' in the maze.")
    }

    val INF = Int.MAX_VALUE
    val dist = Array(rows) { Array(cols) { IntArray(4) { INF } } }
    val startDir = 0 // facing East
    dist[start.first][start.second][startDir] = 0

    val pq = PriorityQueue<State>()
    pq.add(State(0, start.first, start.second, startDir))

    val visited = mutableSetOf<Triple<Int, Int, Int>>()

    while (pq.isNotEmpty()) {
        val (cost, r, c, d) = pq.poll()

        if (Pair(r, c) == end) {
            return cost
        }

        if (Triple(r, c, d) in visited) continue
        visited.add(Triple(r, c, d))

        val currentDist = dist[r][c][d]
        if (cost > currentDist) continue

        // 1) Move forward (cost + 1) if not a wall
        val (dr, dc) = directions[d]
        val nr = r + dr
        val nc = c + dc
        if (nr in 0 until rows && nc in 0 until cols && mazeLines[nr][nc] != '#') {
            val newCost = cost + 1
            if (newCost < dist[nr][nc][d]) {
                dist[nr][nc][d] = newCost
                pq.add(State(newCost, nr, nc, d))
            }
        }

        // 2) Turn left (cost + 1000)
        val leftDir = (d - 1 + 4) % 4
        val leftCost = cost + 1000
        if (leftCost < dist[r][c][leftDir]) {
            dist[r][c][leftDir] = leftCost
            pq.add(State(leftCost, r, c, leftDir))
        }

        // 3) Turn right (cost + 1000)
        val rightDir = (d + 1) % 4
        val rightCost = cost + 1000
        if (rightCost < dist[r][c][rightDir]) {
            dist[r][c][rightDir] = rightCost
            pq.add(State(rightCost, r, c, rightDir))
        }
    }

    return null // If no path found
}

fun solvePart2(mazeLines: List<String>): Int {
    val rows = mazeLines.size
    val cols = mazeLines[0].length

    // Directions: 0=East, 1=South, 2=West, 3=North
    val directions = listOf(
        Pair(0, 1),   // East
        Pair(1, 0),   // South
        Pair(0, -1),  // West
        Pair(-1, 0)   // North
    )

    var start: Pair<Int, Int>? = null
    var end: Pair<Int, Int>? = null

    for (r in 0 until rows) {
        for (c in 0 until cols) {
            when (mazeLines[r][c]) {
                'S' -> start = Pair(r, c)
                'E' -> end = Pair(r, c)
            }
        }
    }

    if (start == null || end == null) {
        throw IllegalArgumentException("Could not find 'S' or 'E' in the maze.")
    }

    val INF = Int.MAX_VALUE
    val dist = Array(rows) { Array(cols) { IntArray(4) { INF } } }
    val startDir = 0 // facing East
    dist[start.first][start.second][startDir] = 0

    val pq = PriorityQueue<State>()
    pq.add(State(0, start.first, start.second, startDir))
    val visited = mutableSetOf<Triple<Int, Int, Int>>()

    while (pq.isNotEmpty()) {
        val (cost, r, c, d) = pq.poll()

        if (Triple(r, c, d) in visited) continue
        visited.add(Triple(r, c, d))

        val currentDist = dist[r][c][d]
        if (cost > currentDist) continue

        // 1) Move forward
        val (dr, dc) = directions[d]
        val nr = r + dr
        val nc = c + dc
        if (nr in 0 until rows && nc in 0 until cols && mazeLines[nr][nc] != '#') {
            val newCost = cost + 1
            if (newCost < dist[nr][nc][d]) {
                dist[nr][nc][d] = newCost
                pq.add(State(newCost, nr, nc, d))
            }
        }

        // 2) Turn left
        val leftDir = (d - 1 + 4) % 4
        val leftCost = cost + 1000
        if (leftCost < dist[r][c][leftDir]) {
            dist[r][c][leftDir] = leftCost
            pq.add(State(leftCost, r, c, leftDir))
        }

        // 3) Turn right
        val rightDir = (d + 1) % 4
        val rightCost = cost + 1000
        if (rightCost < dist[r][c][rightDir]) {
            dist[r][c][rightDir] = rightCost
            pq.add(State(rightCost, r, c, rightDir))
        }
    }

    val minCostEnd = (0 until 4).minOfOrNull { dist[end.first][end.second][it] } ?: INF
    if (minCostEnd == INF) return 0

    val onBestPath = Array(rows) { BooleanArray(cols) { false } }
    val queue = ArrayDeque<Triple<Int, Int, Int>>()

    for (d in 0 until 4) {
        if (dist[end.first][end.second][d] == minCostEnd) {
            queue.add(Triple(end.first, end.second, d))
        }
    }

    val visitedRev = mutableSetOf<Triple<Int, Int, Int>>()
    visitedRev.addAll(queue)

    while (queue.isNotEmpty()) {
        val (r, c, d) = queue.removeFirst()
        onBestPath[r][c] = true

        val costHere = dist[r][c][d]

        val (dr, dc) = directions[d]
        val rPrev = r - dr
        val cPrev = c - dc
        if (rPrev in 0 until rows && cPrev in 0 until cols && mazeLines[rPrev][cPrev] != '#') {
            if (dist[rPrev][cPrev][d] == costHere - 1 && Triple(rPrev, cPrev, d) !in visitedRev) {
                visitedRev.add(Triple(rPrev, cPrev, d))
                queue.add(Triple(rPrev, cPrev, d))
            }
        }

        for (dPre in listOf((d - 1 + 4) % 4, (d + 1) % 4)) {
            if (dist[r][c][dPre] == costHere - 1000 && Triple(r, c, dPre) !in visitedRev) {
                visitedRev.add(Triple(r, c, dPre))
                queue.add(Triple(r, c, dPre))
            }
        }
    }

    return onBestPath.sumOf { row -> row.count { it } }
}

fun main() {
    val lines = java.io.File("input.txt").readLines()
    val part1Answer = solveMaze(lines)
    if (part1Answer == null) {
        println("No path found!")
    } else {
        println("Lowest possible score: $part1Answer")
    }

    val part2Answer = solvePart2(lines)
    println("Number of tiles on at least one best path: $part2Answer")
}