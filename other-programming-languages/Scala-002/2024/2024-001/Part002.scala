import scala.io.Source
import scala.collection.mutable

object SimilarityScoreCalculator {

  // Function to calculate the similarity score
  def calculateSimilarityScore(filePath: String): Unit = {
    // Read the file and extract the two lists
    val lines = Source.fromFile(filePath).getLines().toList

    // Initialize lists for left and right numbers
    var leftList = List[Int]()
    var rightList = List[Int]()

    // Process each line and extract the two numbers
    lines.foreach { line =>
      val parts = line.split("\\s+")
      if (parts.length == 2) {
        val left = parts(0).toInt
        val right = parts(1).toInt
        leftList = leftList :+ left
        rightList = rightList :+ right
      }
    }

    // Create a mutable Map to count occurrences of each number in the right list
    val rightListCounts = mutable.Map[Int, Int]()

    for (num <- rightList) {
      rightListCounts.put(num, rightListCounts.getOrElse(num, 0) + 1)
    }

    // Calculate the similarity score
    val similarityScore = leftList.foldLeft(0) { (score, left) =>
      score + (left * rightListCounts.getOrElse(left, 0))
    }

    // Output the similarity score
    println(s"Total Similarity Score: $similarityScore")
  }

  def main(args: Array[String]): Unit = {
    // Define the file path for input data
    val filePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-001\\input.txt"
    
    // Call the function to calculate the similarity score
    calculateSimilarityScore(filePath)
  }
}
