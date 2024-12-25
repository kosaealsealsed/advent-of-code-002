import java.io.File
import kotlin.math.abs

fun main() {
    // Define the file path
    val filePath = "input.txt"

    // Read the file into two lists
    val (leftList, rightList) = readInputFile(filePath)

    // Sort both lists
    val sortedLeft = leftList.sorted()
    val sortedRight = rightList.sorted()

    // Calculate the total distance
    val totalDistance = sortedLeft.zip(sortedRight).sumOf { (left, right) -> abs(left - right) }

    // Print the total distance
    println("Total Distance: $totalDistance")
}

// Function to read the input file and extract two lists
fun readInputFile(filePath: String): Pair<List<Int>, List<Int>> {
    val leftList = mutableListOf<Int>()
    val rightList = mutableListOf<Int>()

    File(filePath).useLines { lines ->
        lines.forEach { line ->
            val parts = line.split("\\s+".toRegex())
            if (parts.size == 2) {
                leftList.add(parts[0].toInt())
                rightList.add(parts[1].toInt())
            }
        }
    }

    return Pair(leftList, rightList)
}
