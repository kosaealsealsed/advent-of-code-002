# This program processes an input file to identify rules and updates.
# It determines which updates are correctly ordered based on the rules
# and calculates the sum of the middle page numbers of correctly ordered updates.

import strutils, tables, sets

# Step 1: Reading the file content
let filePath = "input.txt" # File containing the rules and updates
let content = readFile(filePath)

# Step 2: Splitting the content into rules and updates sections
let sections = content.strip.split("\n\n") # Split by blank line
let rulesSection = sections[0] # The first part contains the rules
let updatesSection = sections[1] # The second part contains the updates

# Step 3: Parsing the rules section into a list of tuples
# Initialize an empty list to store rules
var rules: seq[(int, int)] = @[]

# Split the rules section into individual lines
let ruleLines = rulesSection.splitLines()

# Process each rule line
for line in ruleLines:
    # Split the line into two parts using the "|" delimiter
    let ruleParts = line.split("|")
    
    # Convert the parts into integers
    let x = parseInt(ruleParts[0])
    let y = parseInt(ruleParts[1])
    
    # Add the tuple of integers to the rules list
    rules.add((x, y))

# Step 4: Parsing the updates section into a list of lists
# Initialize an empty list to store updates
var updates: seq[seq[int]] = @[]

# Split the updates section into individual lines
let updateLines = updatesSection.splitLines()

# Process each update line
for line in updateLines:
    # Split the line into individual numbers using the "," delimiter
    let updateParts = line.split(",")
    
    # Initialize an empty list to store the converted integers for this update
    var update: seq[int] = @[]
    
    # Convert each part into an integer
    for part in updateParts:
        let number = parseInt(part) # Convert the string to an integer
        update.add(number) # Add the integer to the update list
    
    # Add the list of integers to the updates list
    updates.add(update)

# Step 5: Define a helper function to check if an update follows the rules
proc isUpdateOrdered(update: seq[int], rules: seq[(int, int)]): bool =
    # Create a mapping of page numbers to their indices
    var indexMap: Table[int, int] = initTable[int, int]()
    
    # Populate the index map with the positions of each page in the update
    for i in 0..<update.len:
        let page = update[i]
        indexMap[page] = i

    # Check each rule to ensure it is respected
    for rule in rules:
        let (x, y) = rule
        
        if x in indexMap and y in indexMap:
            let xIndex = indexMap[x]
            let yIndex = indexMap[y]
            
            # If x comes after y, the rule is violated
            if xIndex > yIndex:
                return false
    
    return true

# Step 6: Identify correctly ordered updates and calculate their middle page numbers
# Initialize lists for storing results
var correctUpdates: seq[seq[int]] = @[]
var middlePages: seq[int] = @[]

# Process each update
for update in updates:
    # Check if the update is correctly ordered
    if isUpdateOrdered(update, rules):
        # Add the update to the list of correct updates
        correctUpdates.add(update)
        
        # Find the middle page of the update
        let middleIndex = update.len div 2
        let middlePage = update[middleIndex]
        
        # Add the middle page to the list of middle pages
        middlePages.add(middlePage)

# Step 7: Calculate the sum of middle page numbers
var sumMiddlePages = 0

# Sum up the middle pages
for page in middlePages:
    sumMiddlePages += page

# Step 8: Output the result
echo "Sum of middle pages of correctly ordered updates: ", sumMiddlePages

# Helper function for topological sort of an update based on rules
proc topologicalSortUpdate(update: seq[int], rules: seq[(int, int)]): seq[int] =
    # Initialize data structures
    var graph: Table[int, seq[int]] = initTable[int, seq[int]]() # Graph as adjacency list
    var inDegree: Table[int, int] = initTable[int, int]() # In-degree counts
    let nodes = update.toHashSet() # Create a set of nodes from the update

    # Build the graph and in-degree counts
    for rule in rules:
        let x = rule[0]
        let y = rule[1]
        if x in nodes and y in nodes:
            if not (x in graph): graph[x] = @[]
            graph[x].add(y)
            inDegree[y] = inDegree.getOrDefault(y, 0) + 1
            if not (x in inDegree): inDegree[x] = 0

    # Initialize a queue for topological sorting
    var queue: seq[int] = @[]
    for node in nodes:
        if inDegree.getOrDefault(node, 0) == 0:
            queue.add(node)

    # Perform the topological sort
    var sortedUpdate: seq[int] = @[]
    while queue.len > 0: # Check if queue is not empty
        let current = queue[0] # Get the first element
        queue.delete(0) # Remove the first element
        sortedUpdate.add(current)
        for neighbor in graph.getOrDefault(current, @[]):
            inDegree[neighbor] -= 1
            if inDegree[neighbor] == 0:
                queue.add(neighbor)

    return sortedUpdate

# Step 6: Correct the incorrectly ordered updates and calculate their middle page numbers
var incorrectUpdates: seq[seq[int]] = @[]
var incorrectMiddlePages: seq[int] = @[]

# Process each update
for update in updates:
    if not isUpdateOrdered(update, rules):
        # Correct the order using topological sort
        let correctedUpdate = topologicalSortUpdate(update, rules)
        incorrectUpdates.add(correctedUpdate)
        
        # Calculate the middle page of the corrected update
        let middleIndex = correctedUpdate.len div 2
        incorrectMiddlePages.add(correctedUpdate[middleIndex])

# Calculate the sum of middle pages for corrected updates
var sumIncorrectMiddlePages = 0
for middlePage in incorrectMiddlePages:
    sumIncorrectMiddlePages += middlePage

# Output the result for corrected updates
echo "Sum of middle pages of corrected updates: ", sumIncorrectMiddlePages