import scala.io.Source
import scala.math.abs

object LocationDistanceCalculator {

  def calculateTotalDistance(filePath: String): Unit = {
    // Read the file and extract the two lists
    val lines = Source.fromFile(filePath).getLines().toList

    // Initialize left and right lists to store numbers
    var leftList = List[Int]()
    var rightList = List[Int]()

    // Process each line and extract the two numbers
    lines.foreach { line =>
      val parts = line.split("\\s+")
      if (parts.length == 2) {
        leftList = leftList :+ parts(0).toInt
        rightList = rightList :+ parts(1).toInt
      }
    }

    // Sort both lists
    leftList = leftList.sorted
    rightList = rightList.sorted

    // Calculate the total distance by pairing corresponding elements and calculating the absolute difference
    val totalDistance = (leftList zip rightList).map { case (left, right) =>
      abs(left - right)
    }.sum

    // Output the total distance
    println(s"Total Distance: $totalDistance")
  }

  def main(args: Array[String]): Unit = {
    // Define the file path for input data
    val filePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-001\\input.txt"
    
    // Call the function to calculate the total distance
    calculateTotalDistance(filePath)
  }
}
