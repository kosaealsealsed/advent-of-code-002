import java.nio.file.*

def processFile(filePath) {
    // Read the file content
    def content = new File(filePath).text

    // Splitting content into rules and updates
    def sections = content.trim().split("\n\n")
    def rulesSection = sections[0]
    def updatesSection = sections[1]

    def rules = rulesSection.split("\n").collect { line ->
        def parts = line.split("\\|").collect { it.toInteger() }
        [parts[0], parts[1]]
    }

    def updates = updatesSection.split("\n").collect { line ->
        line.split(",").collect { it.toInteger() }
    }

    return [rules, updates]
}

def isUpdateOrdered(update, rules) {
    def indexMap = update.collectEntries { [(it): update.indexOf(it)] }

    rules.every { x, y ->
        !(indexMap.containsKey(x) && indexMap.containsKey(y) && indexMap[x] > indexMap[y])
    }
}

def topologicalSortUpdate(update, rules) {
    def graph = [:].withDefault { [] }
    def inDegree = [:].withDefault { 0 }
    def nodes = update as Set

    rules.each { x, y ->
        if (nodes.contains(x) && nodes.contains(y)) {
            graph[x] << y
            inDegree[y] += 1
            if (!inDegree.containsKey(x)) inDegree[x] = 0
        }
    }

    def queue = [] as Queue
    nodes.each { if (inDegree[it] == 0) queue << it }
    def sortedUpdate = []

    while (!queue.isEmpty()) {
        def current = queue.poll()
        sortedUpdate << current

        graph[current].each { neighbor ->
            inDegree[neighbor] -= 1
            if (inDegree[neighbor] == 0) queue << neighbor
        }
    }

    sortedUpdate
}

def filePath = '/uploads/input.txt'
def (rules, updates) = processFile(filePath)

// Identify correctly ordered updates and calculate their middle page numbers
def correctUpdates = []
def middlePages = []

updates.each { update ->
    if (isUpdateOrdered(update, rules)) {
        correctUpdates << update
        middlePages << update[update.size() / 2]
    }
}

def sumMiddlePages = middlePages.sum()
println "Sum of middle pages (correct updates): $sumMiddlePages"

// Correct the incorrectly ordered updates and find their middle page numbers
def incorrectUpdates = []
def incorrectMiddlePages = []

updates.each { update ->
    if (!isUpdateOrdered(update, rules)) {
        def correctedUpdate = topologicalSortUpdate(update, rules)
        incorrectUpdates << correctedUpdate
        incorrectMiddlePages << correctedUpdate[correctedUpdate.size() / 2]
    }
}

def sumIncorrectMiddlePages = incorrectMiddlePages.sum()
println "Sum of middle pages (corrected updates): $sumIncorrectMiddlePages"
