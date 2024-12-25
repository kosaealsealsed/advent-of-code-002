//open System
//open System.IO

//// Step 1: Reading the file content
//let filePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-005\\input.txt" // File containing the rules and updates
//let content = File.ReadAllText(filePath)

//// Step 2: Splitting the content into rules and updates sections
//let sections = content.Trim().Split([| "\n\n" |], StringSplitOptions.None)
//let rulesSection = sections.[0] // The first part contains the rules
//let updatesSection = sections.[1] // The second part contains the updates

//// Step 3: Parsing the rules section into a list of tuples
//let rules =
//    rulesSection
//    |> fun section -> section.Split([| '\n' |], StringSplitOptions.RemoveEmptyEntries)
//    |> Array.map (fun line ->
//        let parts = line.Split('|') |> Array.map int
//        (parts.[0], parts.[1])
//    )
//    |> List.ofArray


//// Step 4: Parsing the updates section into a list of lists
//let updates =
//    updatesSection
//    |> fun section -> section.Split([| '\n' |], StringSplitOptions.RemoveEmptyEntries)
//    |> Array.map (fun line ->
//        line.Split(',') |> Array.map int |> List.ofArray
//    )
//    |> List.ofArray


//// Step 5: Define a helper function to check if an update follows the rules
//let isUpdateOrdered (update: int list) (rules: (int * int) list) =
//    // Create a mapping of page numbers to their indices
//    let indexMap = 
//        update
//        |> List.indexed
//        |> Map.ofList

//    // Check each rule to ensure it is respected
//    rules
//    |> List.forall (fun (x, y) ->
//        match Map.tryFind x indexMap, Map.tryFind y indexMap with
//        | Some xIndex, Some yIndex -> xIndex <= yIndex
//        | _ -> true
//    )

//// Step 6: Identify correctly ordered updates and calculate their middle page numbers
//let correctUpdates, middlePages =
//    updates
//    |> List.fold (fun (correctUpdates, middlePages) update ->
//        if isUpdateOrdered update rules then
//            let middleIndex = List.length update / 2
//            let middlePage = List.item middleIndex update
//            (update :: correctUpdates, middlePage :: middlePages)
//        else
//            (correctUpdates, middlePages)
//    ) ([], [])

//// Step 7: Calculate the sum of middle page numbers
//let sumMiddlePages = List.sum middlePages

//// Step 8: Output the result
//printfn "Sum of middle pages of correctly ordered updates: %d" sumMiddlePages
//open System
//open System.IO

//// Step 1: Read the file content
//let filePath = @"\\vmware-host\Shared Folders\C\advent-of-code-002\input-files\2024\2024-005\input.txt"
//let content = File.ReadAllText(filePath)

//// Step 2: Split content into rules and updates sections
//let sections = content.Trim().Split([| "\n\n" |], StringSplitOptions.None)
//let rulesSection = sections.[0]
//let updatesSection = sections.[1]

//// Step 3: Parse rules section into a list of tuples
//let rules =
//    rulesSection
//    |> fun section -> section.Split([| '\n' |], StringSplitOptions.RemoveEmptyEntries)
//    |> Array.map (fun line ->
//        let parts = line.Split('|') |> Array.map int
//        (parts.[0], parts.[1])
//    )
//    |> List.ofArray

//// Step 4: Parse updates section into a list of lists
//let updates =
//    updatesSection
//    |> fun section -> section.Split([| '\n' |], StringSplitOptions.RemoveEmptyEntries)
//    |> Array.map (fun line ->
//        line.Split(',') |> Array.map int |> List.ofArray
//    )
//    |> List.ofArray

//// Step 5: Define a function to check if an update follows the rules
//let isUpdateOrdered (update: int list) (rules: (int * int) list) =
//    let indexMap =
//        update
//        |> List.indexed
//        |> Map.ofList

//    rules
//    |> List.forall (fun (x, y) ->
//        match Map.tryFind x indexMap, Map.tryFind y indexMap with
//        | Some xIndex, Some yIndex -> xIndex <= yIndex
//        | _ -> true
//    )

//// Step 6: Identify correctly ordered updates and calculate their middle pages
//let correctUpdates, middlePages =
//    updates
//    |> List.fold (fun (correctUpdates, middlePages) update ->
//        if isUpdateOrdered update rules then
//            let middleIndex = List.length update / 2
//            let middlePage = List.item middleIndex update
//            (update :: correctUpdates, middlePage :: middlePages)
//        else
//            (correctUpdates, middlePages)
//    ) ([], [])

//// Step 7: Calculate the sum of middle page numbers
//let sumMiddlePages = List.sum middlePages

//// Step 8: Output the result
//printfn "Sum of middle pages of correctly ordered updates: %d" sumMiddlePages
open System
open System.IO

// Helper function to read file content and split into lines
let readFile (filePath: string) =
    File.ReadAllLines(filePath)
    |> Array.toList

// Helper function to split content into rules and updates sections
let parseRulesAndUpdates (content: string) =
    let sections = content.Split([| "\n\n" |], StringSplitOptions.None)
    let rulesSection = sections.[0]
    let updatesSection = sections.[1]

    let rules =
        rulesSection.Split([| '\n' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun line ->
            let parts = line.Split('|')
            let x = int parts.[0]
            let y = int parts.[1]
            (x, y)
        )
        |> List.ofArray

    let updates =
        updatesSection.Split([| '\n' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun line ->
            line.Split(',') |> Array.map int |> List.ofArray
        )
        |> List.ofArray

    (rules, updates)

// Create a map of indices for an update
let makeIndexMap (update: int list) =
    update
    |> List.indexed
    |> Map.ofList

// Check if an update follows the rules
let isUpdateOrdered (update: int list) (rules: (int * int) list) =
    let indexMap = makeIndexMap update
    rules
    |> List.forall (fun (x, y) ->
        match Map.tryFind x indexMap, Map.tryFind y indexMap with
        | Some ix, Some iy -> ix <= iy
        | _ -> true
    )

// Get the middle element of a list
let middleElement lst =
    let index = List.length lst / 2
    List.item index lst

// Calculate middle pages of correctly ordered updates
let calculateMiddlePages (updates: int list list) (rules: (int * int) list) =
    updates
    |> List.fold (fun (correctUpdates, middlePages) update ->
        if isUpdateOrdered update rules then
            let middlePage = middleElement update
            (update :: correctUpdates, middlePage :: middlePages)
        else
            (correctUpdates, middlePages)
    ) ([], [])

// Main procedure
let filePath = @"\\vmware-host\Shared Folders\C\advent-of-code-002\Ocaml\2024\2024-005\input.txt"
let content = File.ReadAllText(filePath)
let rules, updates = parseRulesAndUpdates content
let correctUpdates, middlePages = calculateMiddlePages updates rules
let sumMiddlePages = List.sum middlePages

// Print the result
printfn "Sum of middle pages: %d" sumMiddlePages
