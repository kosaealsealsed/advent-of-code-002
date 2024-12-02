open System
open System.IO
open System.Collections.Generic

// Function to read the input file and extract two lists
let readInputFile filePath =
    let leftList = ResizeArray<int>()
    let rightList = ResizeArray<int>()
    File.ReadLines(filePath)
    |> Seq.iter (fun line ->
        let parts = line.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)
        if parts.Length = 2 then
            leftList.Add(int parts.[0])
            rightList.Add(int parts.[1])
    )
    leftList, rightList

// Function to calculate the similarity score
let calculateSimilarityScore leftList rightList =
    // Build a frequency dictionary for the right list
    let rightListCounts = Dictionary<int, int>()
    for num in rightList do
        if rightListCounts.ContainsKey(num) then
            rightListCounts.[num] <- rightListCounts.[num] + 1
        else
            rightListCounts.[num] <- 1

    // Calculate the similarity score
    leftList
    |> Seq.sumBy (fun left -> 
        let count = if rightListCounts.ContainsKey(left) then rightListCounts.[left] else 0
        left * count)

// Main program
[<EntryPoint>]
let main argv =
    let filePath = "/uploads/input.txt"
    try
        // Read the input file
        let leftList, rightList = readInputFile filePath

        // Calculate the similarity score
        let similarityScore = calculateSimilarityScore (List.ofSeq leftList) (List.ofSeq rightList)

        // Print the result
        printfn "Similarity Score: %d" similarityScore
        0 // Return success
    with
    | ex ->
        printfn "Error: %s" ex.Message
        1 // Return error
