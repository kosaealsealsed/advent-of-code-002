import scala.io.Source

object SafeReports {

  // Function to check if a report is safe
  def isSafe(report: List[Int]): Boolean = {
    val differences = report.sliding(2).map { case List(a, b) => b - a }.toList
    val allIncreasing = differences.forall(diff => diff >= 1 && diff <= 3)
    val allDecreasing = differences.forall(diff => diff <= -1 && diff >= -3)
    allIncreasing || allDecreasing
  }

  // Function to check if a report is safe with the Problem Dampener
  def isSafeWithDampener(report: List[Int]): Boolean = {
    if (isSafe(report)) return true
    report.indices.exists(i => isSafe(report.patch(i, Nil, 1)))
  }

  def main(args: Array[String]): Unit = {
    val inputPath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-002\\input.txt"

    // Read the input file
    val lines = Source.fromFile(inputPath).getLines().toList
    val reports = lines.map(line => line.split("\\s+").map(_.toInt).toList)

    // Count safe reports
    val safeCount = reports.count(isSafe)
    println(s"Safe reports: $safeCount")

    // Count safe reports with the Problem Dampener
    val safeWithDampenerCount = reports.count(isSafeWithDampener)
    println(s"Safe reports with dampener: $safeWithDampenerCount")
  }
}
