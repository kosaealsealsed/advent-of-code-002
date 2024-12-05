using DataStructures

# Reading the file content
file_path = "input.txt"
content = read(file_path, String)

# Splitting content into rules and updates
rules_section, updates_section = split(content, "\n\n", limit=2)
rules = [(parse(Int, x), parse(Int, y)) for line in split(rules_section, "\n") for (x, y) in [split(line, "|")]]
updates = [map(parse(Int, split(line, ","))) for line in split(updates_section, "\n")]

# Helper function to check if an update follows the rules
function is_update_ordered(update, rules)
    index_map = Dict(page => i for (i, page) in enumerate(update))
    for (x, y) in rules
        if haskey(index_map, x) && haskey(index_map, y) && index_map[x] > index_map[y]
            return false
        end
    end
    return true
end

# Identify correctly ordered updates and their middle page numbers
correct_updates = []
middle_pages = []

for update in updates
    if is_update_ordered(update, rules)
        push!(correct_updates, update)
        push!(middle_pages, update[Int(ceil(length(update) / 2))])
    end
end

# Calculate the sum of middle pages
sum_middle_pages = sum(middle_pages)

# Helper function to sort an update according to the rules
function topological_sort_update(update, rules)
    # Build a graph and in-degree count based on the rules
    graph = DefaultDict{Int, Vector{Int}}(Vector{Int})
    in_degree = DefaultDict{Int, Int}(0)
    nodes = Set(update)
    
    for (x, y) in rules
        if x in nodes && y in nodes
            push!(graph[x], y)
            in_degree[y] += 1
            if !haskey(in_degree, x)
                in_degree[x] = 0
            end
        end
    end

    # Perform topological sort
    queue = Deque([node for node in nodes if in_degree[node] == 0])
    sorted_update = []
    
    while !isempty(queue)
        current = popfirst!(queue)
        push!(sorted_update, current)
        
        for neighbor in graph[current]
            in_degree[neighbor] -= 1
            if in_degree[neighbor] == 0
                push!(queue, neighbor)
            end
        end
    end
    
    return sorted_update
end

# Correct the incorrectly ordered updates and find their middle page numbers
incorrect_updates = []
incorrect_middle_pages = []

for update in updates
    if !is_update_ordered(update, rules)
        corrected_update = topological_sort_update(update, rules)
        push!(incorrect_updates, corrected_update)
        push!(incorrect_middle_pages, corrected_update[Int(ceil(length(corrected_update) / 2))])
    end
end

# Calculate the sum of middle pages for corrected updates
sum_incorrect_middle_pages = sum(incorrect_middle_pages)

println("Sum of middle pages (correctly ordered): ", sum_middle_pages)
println("Sum of middle pages (corrected updates): ", sum_incorrect_middle_pages)
