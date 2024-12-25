package com.peter_burbery.solve_advent_of_code_with_java_002.year_2024.day_005

import java.io.BufferedReader
import java.io.FileReader
import java.io.IOException

fun main() {
    val filePath = "input.txt"
    val contentBuilder = StringBuilder()

    // Reading the file content
    try {
        BufferedReader(FileReader(filePath)).use { br ->
            var line: String?
            while (br.readLine().also { line = it } != null) {
                contentBuilder.append(line).append("\n")
            }
        }
    } catch (e: IOException) {
        println("Error reading the file: ${e.message}")
        return
    }

    val content = contentBuilder.toString().trim()

    // Splitting content into rules and updates
    val sections = content.split("\n\n")
    if (sections.size != 2) {
        println("Invalid input format. Expected two sections separated by two newlines.")
        return
    }

    val rulesSection = sections[0]
    val updatesSection = sections[1]

    // Parsing rules
    val rules = mutableListOf<Pair<Int, Int>>()
    for (ruleLine in rulesSection.split("\n")) {
        val parts = ruleLine.split("|")
        if (parts.size != 2) {
            println("Invalid rule format: $ruleLine")
            continue
        }
        try {
            val x = parts[0].trim().toInt()
            val y = parts[1].trim().toInt()
            rules.add(x to y)
        } catch (e: NumberFormatException) {
            println("Invalid number in rule: $ruleLine")
        }
    }

    // Parsing updates
    val updates = mutableListOf<List<Int>>()
    for (updateLine in updatesSection.split("\n")) {
        val parts = updateLine.split(",")
        val update = mutableListOf<Int>()
        var valid = true
        for (part in parts) {
            try {
                update.add(part.trim().toInt())
            } catch (e: NumberFormatException) {
                println("Invalid number in update: $updateLine")
                valid = false
                break
            }
        }
        if (valid) updates.add(update)
    }

    // Identify correctly ordered updates and their middle page numbers
    val correctUpdates = mutableListOf<List<Int>>()
    val middlePages = mutableListOf<Int>()

    for (update in updates) {
        if (isUpdateOrdered(update, rules)) {
            correctUpdates.add(update)
            middlePages.add(getMiddlePage(update))
        }
    }

    // Calculate the sum of middle pages for correct updates
    val sumMiddlePages = middlePages.sumOf { it.toLong() }
    println("Sum of middle pages for correctly ordered updates: $sumMiddlePages")

    // Identify incorrectly ordered updates, correct them, and collect their middle pages
    val incorrectUpdates = mutableListOf<List<Int>>()
    val incorrectMiddlePages = mutableListOf<Int>()

    for (update in updates) {
        if (!isUpdateOrdered(update, rules)) {
            val correctedUpdate = topologicalSortUpdate(update, rules)
            if (correctedUpdate.isEmpty()) {
                println("Cycle detected or unable to sort update: $update")
                continue
            }
            incorrectUpdates.add(correctedUpdate)
            incorrectMiddlePages.add(getMiddlePage(correctedUpdate))
        }
    }

    // Calculate the sum of middle pages for corrected updates
    val sumIncorrectMiddlePages = incorrectMiddlePages.sumOf { it.toLong() }
    println("Sum of middle pages for corrected updates: $sumIncorrectMiddlePages")
}

/**
 * Checks if the given update follows all the specified rules.
 */
fun isUpdateOrdered(update: List<Int>, rules: List<Pair<Int, Int>>): Boolean {
    val indexMap = update.withIndex().associate { it.value to it.index }

    for ((x, y) in rules) {
        if (indexMap.containsKey(x) && indexMap.containsKey(y)) {
            if (indexMap[x]!! > indexMap[y]!!) return false
        }
    }
    return true
}

/**
 * Performs a topological sort on the given update based on the specified rules.
 */
fun topologicalSortUpdate(update: List<Int>, rules: List<Pair<Int, Int>>): List<Int> {
    val nodes = update.toSet()
    val graph = nodes.associateWith { mutableListOf<Int>() }
    val inDegree = nodes.associateWith { 0 }.toMutableMap()

    // Build the graph based on rules
    for ((x, y) in rules) {
        if (x in nodes && y in nodes) {
            graph[x]?.add(y)
            inDegree[y] = inDegree.getValue(y) + 1
        }
    }

    // Initialize the queue with nodes having in-degree 0
    val queue = ArrayDeque<Int>()
    for (node in nodes) {
        if (inDegree[node] == 0) queue.add(node)
    }

    val sortedUpdate = mutableListOf<Int>()

    while (queue.isNotEmpty()) {
        val current = queue.removeFirst()
        sortedUpdate.add(current)

        for (neighbor in graph[current]!!) {
            inDegree[neighbor] = inDegree.getValue(neighbor) - 1
            if (inDegree[neighbor] == 0) queue.add(neighbor)
        }
    }

    // Check if topological sort was possible (i.e., no cycles)
    return if (sortedUpdate.size != nodes.size) emptyList() else sortedUpdate
}

/**
 * Retrieves the middle page number from the update list.
 */
fun getMiddlePage(update: List<Int>): Int = update[update.size / 2]
