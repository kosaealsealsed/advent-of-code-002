import Foundation

// Function to calculate total distance
func calculateTotalDistance(filePath: String) -> Int {
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
    
    // Sort both lists
    leftList.sort()
    rightList.sort()
    
    // Calculate the total distance
    let totalDistance = zip(leftList, rightList).reduce(0) { $0 + abs($1.0 - $1.1) }
    
    return totalDistance
}

// Input file path
let filePath = "input.txt"

// Calculate and print the total distance
let totalDistance = calculateTotalDistance(filePath: filePath)
print("Total Distance: \(totalDistance)")
