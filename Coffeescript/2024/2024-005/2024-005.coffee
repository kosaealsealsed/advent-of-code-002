fs = require 'fs'
path = require 'path'

# Reading the file content
filePath = path.join __dirname, 'input.txt'
content = fs.readFileSync filePath, 'utf8'

# Splitting content into rules and updates
[rulesSection, updatesSection] = content.trim().split "\n\n"

rules = rulesSection.split("\n").map (line) ->
    [x, y] = line.split("|").map(Number)
    [x, y]

updates = updatesSection.split("\n").map (line) ->
    line.split(",").map(Number)

# Helper function to check if an update follows the rules
isUpdateOrdered = (update, rules) ->
    indexMap = new Map()
    update.forEach (page, index) ->
        indexMap.set page, index

    for [x, y] in rules
        if indexMap.has(x) and indexMap.has(y) and indexMap.get(x) > indexMap.get(y)
            return false
    true

# Identify correctly ordered updates and their middle page numbers
correctUpdates = []
middlePages = []

updates.forEach (update) ->
    if isUpdateOrdered(update, rules)
        correctUpdates.push update
        middlePages.push update[Math.floor(update.length / 2)]

# Calculate the sum of middle pages
sumMiddlePages = middlePages.reduce (acc, val) ->
  acc + val
console.log "Sum of middle pages for correctly ordered updates:", sumMiddlePages

# Helper function to sort an update according to the rules
topologicalSort = (update, rules) ->
    graph = new Map()
    inDegree = new Map()
    nodes = new Set(update)

    rules.forEach ([x, y]) ->
        if nodes.has(x) and nodes.has(y)
            if not graph.has(x) then graph.set(x, [])
            graph.get(x).push(y)

            inDegree.set y, (inDegree.get(y) || 0) + 1
            if not inDegree.has(x) then inDegree.set(x, 0)

    queue = Array.from(nodes).filter (node) -> (inDegree.get(node) || 0) is 0
    sortedUpdate = []

    while queue.length > 0
        current = queue.shift()
        sortedUpdate.push current

        (graph.get(current) || []).forEach (neighbor) ->
            inDegree.set neighbor, inDegree.get(neighbor) - 1
            if inDegree.get(neighbor) is 0 then queue.push(neighbor)

    sortedUpdate

# Correct the incorrectly ordered updates and find their middle page numbers
incorrectUpdates = []
incorrectMiddlePages = []

updates.forEach (update) ->
    if not isUpdateOrdered(update, rules)
        correctedUpdate = topologicalSort(update, rules)
        incorrectUpdates.push correctedUpdate
        incorrectMiddlePages.push correctedUpdate[Math.floor(correctedUpdate.length / 2)]

# Calculate the sum of middle pages for corrected updates
sumIncorrectMiddlePages = incorrectMiddlePages.reduce (acc, val) ->
  acc + val
console.log "Sum of middle pages for corrected updates:", sumIncorrectMiddlePages
