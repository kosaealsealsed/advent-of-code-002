import Foundation

// Function to calculate the similarity score
func calculateSimilarityScore(filePath: String) -> Int {
    // Read the file content
    guard let fileContent = try? String(contentsOfFile: filePath) else {
        print("Failed to read file at \(filePath)")
        return 0
    }
    
    // Split file content into lines
    let lines = fileContent.split(separator: "\n")
    
    // Initialize two lists
    var leftList: [Int] = []
    var rightList: [Int] = []
    
    // Parse the lines into two lists
    for line in lines {
        let parts = line.split(separator: " ").compactMap { Int($0) }
        if parts.count == 2 {
            leftList.append(parts[0])
            rightList.append(parts[1])
        }
    }
    
    // Create a dictionary to count occurrences in the right list
    var rightListCounts: [Int: Int] = [:]
    for number in rightList {
        rightListCounts[number, default: 0] += 1
    }
    
    // Calculate the similarity score
    let similarityScore = leftList.reduce(0) { (result, left) -> Int in
        let count = rightListCounts[left] ?? 0
        return result + (left * count)
    }
    
    return similarityScore
}

// Input file path
let filePath = "input.txt"

// Calculate and print the similarity score
let similarityScore = calculateSimilarityScore(filePath: filePath)
print("Similarity Score: \(similarityScore)")
