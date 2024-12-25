open System
open System.IO

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

// Function to calculate the total distance
let calculateTotalDistance leftList rightList =
    let sortedLeft = leftList |> List.sort
    let sortedRight = rightList |> List.sort
    List.zip sortedLeft sortedRight
    |> List.sumBy (fun (left, right) -> abs (left - right))

// Main program
[<EntryPoint>]
let main argv =
    let filePath = "/uploads/input.txt"
    try
        // Read the input file
        let leftList, rightList = readInputFile filePath

        // Calculate the total distance
        let totalDistance = calculateTotalDistance (List.ofSeq leftList) (List.ofSeq rightList)

        // Print the result
        printfn "Total Distance: %d" totalDistance
        0 // Return success
    with
    | ex ->
        printfn "Error: %s" ex.Message
        1 // Return error
