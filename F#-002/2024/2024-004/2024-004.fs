open System
open System.IO

// Read the grid from the input file
let readGrid filePath =
    File.ReadAllLines(filePath)
    |> Array.map (fun line -> line.ToCharArray())

// Define the target word and movement directions
let targetWord = "XMAS"
let directions = [
    (0, 1)   // Right
    (1, 0)   // Down
    (1, 1)   // Diagonal-right-down
    (1, -1)  // Diagonal-left-down
    (0, -1)  // Left
    (-1, 0)  // Up
    (-1, -1) // Diagonal-left-up
    (-1, 1)  // Diagonal-right-up
]

// Function to check if the word exists in a given direction
let checkWord grid (word: string) x y dx dy =
    let rows = Array.length grid
    let cols = Array.length grid.[0]
    let wordLength = word.Length
    let isValid = 
        [0 .. wordLength - 1]
        |> List.forall (fun i ->
            let nx = x + i * dx
            let ny = y + i * dy
            nx >= 0 && ny >= 0 && nx < rows && ny < cols && grid.[nx].[ny] = word.[i]
        )
    isValid

// Count all occurrences of the target word
let countOccurrences grid word =
    let rows = Array.length grid
    let cols = Array.length grid.[0]
    [ for x in 0 .. rows - 1 do
        for y in 0 .. cols - 1 do
            for (dx, dy) in directions do
                if checkWord grid word x y dx dy then yield 1 ]
    |> List.sum

// Count all XMAS patterns
let countXmasPatterns grid =
    let rows = Array.length grid
    let cols = Array.length grid.[0]
    [ for x in 1 .. rows - 2 do
        for y in 1 .. cols - 2 do
            let center = grid.[x].[y]
            let topLeft = grid.[x - 1].[y - 1]
            let topRight = grid.[x - 1].[y + 1]
            let bottomLeft = grid.[x + 1].[y - 1]
            let bottomRight = grid.[x + 1].[y + 1]
            if center = 'A' && (
                (topLeft = 'M' && topRight = 'S' && bottomLeft = 'M' && bottomRight = 'S') ||
                (topLeft = 'S' && topRight = 'M' && bottomLeft = 'S' && bottomRight = 'M') ||
                (topLeft = 'M' && topRight = 'M' && bottomLeft = 'S' && bottomRight = 'S') ||
                (topLeft = 'S' && topRight = 'S' && bottomLeft = 'M' && bottomRight = 'M')
            ) then yield 1 ]
    |> List.sum

// Main function
[<EntryPoint>]
let main argv =
    let filePath = "/uploads/input.txt"
    let grid = readGrid filePath
    let xmasCount = countOccurrences grid targetWord
    printfn "Count of XMAS occurrences: %d" xmasCount
    let xmasPatterns = countXmasPatterns grid
    printfn "Total XMAS patterns: %d" xmasPatterns
    0
