import java.io.File

fun main() {
    // Define the file path
    val filePath = "input.txt"

    // Read the file into two lists
    val (leftList, rightList) = readInputFile(filePath)

    // Convert the right list into a frequency map
    val rightListCounts = rightList.groupingBy { it }.eachCount()

    // Calculate the similarity score
    val similarityScore = leftList.sumOf { left -> left * (rightListCounts[left] ?: 0) }

    // Print the similarity score
    println("Similarity Score: $similarityScore")
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
