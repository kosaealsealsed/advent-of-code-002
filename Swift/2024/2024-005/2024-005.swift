import Foundation

// Read the file content
let filePath = "input.txt"
let fileContent = try String(contentsOfFile: filePath)
let sections = fileContent.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: "\n\n")

let rulesSection = sections[0]
let updatesSection = sections[1]

// Parse rules and updates
let rules = rulesSection.split(separator: "\n").map { line -> (Int, Int) in
    let parts = line.split(separator: "|").map { Int($0)! }
    return (parts[0], parts[1])
}

let updates = updatesSection.split(separator: "\n").map { line -> [Int] in
    line.split(separator: ",").map { Int($0)! }
}

// Helper function to check if an update follows the rules
func isUpdateOrdered(update: [Int], rules: [(Int, Int)]) -> Bool {
    let indexMap = Dictionary(uniqueKeysWithValues: update.enumerated().map { ($1, $0) })
    for (x, y) in rules {
        if let ix = indexMap[x], let iy = indexMap[y], ix > iy {
            return false
        }
    }
    return true
}

// Identify correctly ordered updates and their middle page numbers
var correctUpdates: [[Int]] = []
var middlePages: [Int] = []

for update in updates {
    if isUpdateOrdered(update: update, rules: rules) {
        correctUpdates.append(update)
        middlePages.append(update[update.count / 2])
    }
}

let sumMiddlePages = middlePages.reduce(0, +)

// Helper function to sort an update according to the rules
func topologicalSortUpdate(update: [Int], rules: [(Int, Int)]) -> [Int] {
    var graph: [Int: [Int]] = [:]
    var inDegree: [Int: Int] = [:]
    let nodes = Set(update)
    
    for (x, y) in rules {
        if nodes.contains(x) && nodes.contains(y) {
            graph[x, default: []].append(y)
            inDegree[y, default: 0] += 1
            inDegree[x, default: 0] = inDegree[x, default: 0]
        }
    }
    
    var queue: [Int] = inDegree.filter { $0.value == 0 }.map { $0.key }
    var sortedUpdate: [Int] = []
    
    while !queue.isEmpty {
        let current = queue.removeFirst()
        sortedUpdate.append(current)
        
        for neighbor in graph[current, default: []] {
            inDegree[neighbor, default: 0] -= 1
            if inDegree[neighbor] == 0 {
                queue.append(neighbor)
            }
        }
    }
    
    return sortedUpdate
}

// Correct the incorrectly ordered updates and find their middle page numbers
var incorrectUpdates: [[Int]] = []
var incorrectMiddlePages: [Int] = []

for update in updates {
    if !isUpdateOrdered(update: update, rules: rules) {
        let correctedUpdate = topologicalSortUpdate(update: update, rules: rules)
        incorrectUpdates.append(correctedUpdate)
        incorrectMiddlePages.append(correctedUpdate[correctedUpdate.count / 2])
    }
}

let sumIncorrectMiddlePages = incorrectMiddlePages.reduce(0, +)

// Print results
print("Sum of middle pages for correct updates: \(sumMiddlePages)")
print("Sum of middle pages for corrected updates: \(sumIncorrectMiddlePages)")
