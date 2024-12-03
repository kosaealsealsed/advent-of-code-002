import scala.io.Source
import scala.util.matching.Regex

object CorruptedMemoryParser {

  // Function to calculate the sum of all mul(X, Y) operations
  def sumMulOperations(corruptedMemory: String): Int = {
    val pattern: Regex = """mul\((\d{1,3}),(\d{1,3})\)""".r
    var total = 0

    for (matchData <- pattern.findAllMatchIn(corruptedMemory)) {
      val num1 = matchData.group(1).toInt
      val num2 = matchData.group(2).toInt
      total += num1 * num2
    }

    total
  }

  // Function to calculate the sum of enabled mul(X, Y) operations
  def sumEnabledMulOperations(corruptedMemory: String): Int = {
    val pattern: Regex = """(do\(\)|don't\(\)|mul\((\d{1,3}),(\d{1,3})\))""".r
    var totalSum = 0
    var mulEnabled = true // mul instructions are enabled at the start

    for (matchData <- pattern.findAllMatchIn(corruptedMemory)) {
      val fullMatch = matchData.group(1)

      if (fullMatch == "do()") {
        mulEnabled = true
      } else if (fullMatch == "don't()") {
        mulEnabled = false
      } else if (mulEnabled && matchData.group(2) != null && matchData.group(3) != null) {
        val num1 = matchData.group(2).toInt
        val num2 = matchData.group(3).toInt
        totalSum += num1 * num2
      }
    }

    totalSum
  }

  def main(args: Array[String]): Unit = {
    // Read the corrupted memory from 'input.txt'
    val filePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-003\\input.txt"
    val corruptedMemory = Source.fromFile(filePath).getLines().mkString("\n")

    // Calculate the results
    val totalSumAllMulOperations = sumMulOperations(corruptedMemory)
    val totalSumEnabledMulOperations = sumEnabledMulOperations(corruptedMemory)

    // Output the results
    println(s"The sum of all mul operations is: $totalSumAllMulOperations")
    println(s"The sum of enabled mul operations is: $totalSumEnabledMulOperations")
  }
}
