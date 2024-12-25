import scala.io.Source
import scala.collection.mutable.{Map, ListBuffer, Queue}
import scala.util.{Try, Success, Failure}

object UpdateProcessor {

  def main(args: Array[String]): Unit = {
    val filePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-005\\input.txt"

    // Read the entire file content
    val contentTry = Try(Source.fromFile(filePath).mkString.trim)

    contentTry match {
      case Failure(exception) =>
        System.err.println(s"Error reading the file: ${exception.getMessage}")
        sys.exit(1)

      case Success(content) =>
        // Split content into rules and updates based on two consecutive newlines
        val sections = content.split("\n\n")
        if (sections.length != 2) {
          System.err.println("Invalid input format. Expected two sections separated by two newlines.")
          sys.exit(1)
        }

        val rulesSection = sections(0)
        val updatesSection = sections(1)

        // Parse rules
        val rules = parseRules(rulesSection)

        // Parse updates
        val updates = parseUpdates(updatesSection)

        // Identify correctly ordered updates and their middle page numbers
        val (correctUpdates, middlePages) = identifyCorrectUpdates(updates, rules)

        // Calculate the sum of middle pages for correct updates
        val sumMiddlePages = middlePages.sum
        println(s"Sum of middle pages for correctly ordered updates: $sumMiddlePages")

        // Identify incorrectly ordered updates, correct them, and collect their middle pages
        val (incorrectUpdates, incorrectMiddlePages) = identifyIncorrectUpdates(updates, rules)

        // Calculate the sum of middle pages for corrected updates
        val sumIncorrectMiddlePages = incorrectMiddlePages.sum
        println(s"Sum of middle pages for corrected updates: $sumIncorrectMiddlePages")
    }
  }

  /**
   * Parses the rules section into a list of tuples.
   *
   * @param rulesSection The rules section as a string.
   * @return A list of tuples representing the rules.
   */
  def parseRules(rulesSection: String): List[(Int, Int)] = {
    rulesSection
      .split("\n")
      .filter(_.trim.nonEmpty)
      .flatMap { line =>
        val parts = line.split("\\|").map(_.trim)
        if (parts.length != 2) {
          System.err.println(s"Invalid rule format: $line")
          None
        } else {
          (Try(parts(0).toInt), Try(parts(1).toInt)) match {
            case (Success(x), Success(y)) => Some((x, y))
            case _ =>
              System.err.println(s"Invalid numbers in rule: $line")
              None
          }
        }
      }
      .toList
  }

  /**
   * Parses the updates section into a list of integer lists.
   *
   * @param updatesSection The updates section as a string.
   * @return A list of updates, each update being a list of integers.
   */
  def parseUpdates(updatesSection: String): List[List[Int]] = {
    updatesSection
      .split("\n")
      .filter(_.trim.nonEmpty)
      .flatMap { line =>
        val parts = line.split(",").map(_.trim)
        val updateTry = Try(parts.map(_.toInt).toList)
        updateTry match {
          case Success(update) => Some(update)
          case Failure(_) =>
            System.err.println(s"Invalid number in update: $line")
            None
        }
      }
      .toList
  }

  /**
   * Checks if an update is ordered according to the given rules.
   *
   * @param update The update as a list of integers.
   * @param rules  The list of rules as tuples.
   * @return True if the update is correctly ordered, False otherwise.
   */
  def isUpdateOrdered(update: List[Int], rules: List[(Int, Int)]): Boolean = {
    val indexMap: Map[Int, Int] = Map(update.zipWithIndex: _*)
    rules.forall { case (x, y) =>
      (!indexMap.contains(x) || !indexMap.contains(y)) || (indexMap(x) < indexMap(y))
    }
  }

  /**
   * Identifies correctly ordered updates and collects their middle pages.
   *
   * @param updates The list of all updates.
   * @param rules   The list of rules.
   * @return A tuple containing the list of correct updates and their middle pages.
   */
  def identifyCorrectUpdates(updates: List[List[Int]], rules: List[(Int, Int)]): (List[List[Int]], List[Int]) = {
    val correctUpdates = ListBuffer[List[Int]]()
    val middlePages = ListBuffer[Int]()

    for (update <- updates) {
      if (isUpdateOrdered(update, rules)) {
        correctUpdates += update
        middlePages += getMiddlePage(update)
      }
    }

    (correctUpdates.toList, middlePages.toList)
  }

  /**
   * Identifies incorrectly ordered updates, corrects them using topological sort,
   * and collects their middle pages.
   *
   * @param updates The list of all updates.
   * @param rules   The list of rules.
   * @return A tuple containing the list of corrected updates and their middle pages.
   */
  def identifyIncorrectUpdates(updates: List[List[Int]], rules: List[(Int, Int)]): (List[List[Int]], List[Int]) = {
    val incorrectUpdates = ListBuffer[List[Int]]()
    val incorrectMiddlePages = ListBuffer[Int]()

    for (update <- updates) {
      if (!isUpdateOrdered(update, rules)) {
        val correctedUpdate = topologicalSortUpdate(update, rules)
        if (correctedUpdate.nonEmpty) {
          incorrectUpdates += correctedUpdate
          incorrectMiddlePages += getMiddlePage(correctedUpdate)
        } else {
          System.err.println(s"Cycle detected or unable to sort update: ${update.mkString(", ")}")
        }
      }
    }

    (incorrectUpdates.toList, incorrectMiddlePages.toList)
  }

  /**
   * Performs a topological sort on an update according to the given rules.
   *
   * @param update The update as a list of integers.
   * @param rules  The list of rules as tuples.
   * @return A sorted list of integers if successful, or an empty list if a cycle is detected.
   */
  def topologicalSortUpdate(update: List[Int], rules: List[(Int, Int)]): List[Int] = {
    val graph: Map[Int, List[Int]] = Map().withDefaultValue(List())
    val inDegree: Map[Int, Int] = Map().withDefaultValue(0)
    val nodes: Set[Int] = update.toSet

    // Build the graph and in-degree map
    val mutableGraph = scala.collection.mutable.Map[Int, List[Int]]().withDefaultValue(List())
    val mutableInDegree = scala.collection.mutable.Map[Int, Int]().withDefaultValue(0)

    for (node <- nodes) {
      mutableGraph(node) = List()
      mutableInDegree(node) = 0
    }

    for ((x, y) <- rules) {
      if (nodes.contains(x) && nodes.contains(y)) {
        mutableGraph(x) = y :: mutableGraph(x)
        mutableInDegree(y) += 1
      }
    }

    // Initialize the queue with nodes having in-degree 0
    val queue: Queue[Int] = Queue()
    for (node <- nodes) {
      if (mutableInDegree(node) == 0) {
        queue.enqueue(node)
      }
    }

    val sortedUpdate = ListBuffer[Int]()

    while (queue.nonEmpty) {
      val current = queue.dequeue()
      sortedUpdate += current

      for (neighbor <- mutableGraph(current)) {
        mutableInDegree(neighbor) -= 1
        if (mutableInDegree(neighbor) == 0) {
          queue.enqueue(neighbor)
        }
      }
    }

    if (sortedUpdate.length == nodes.size) sortedUpdate.toList else List()
  }

  /**
   * Retrieves the middle page number from the update list.
   * For even-sized lists, it takes the upper middle (e.g., index `len/2`).
   *
   * @param update The update as a list of integers.
   * @return The middle page number.
   */
  def getMiddlePage(update: List[Int]): Int = {
    update(update.length / 2)
  }
}
