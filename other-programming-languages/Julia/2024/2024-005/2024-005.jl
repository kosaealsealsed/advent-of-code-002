using Base.Iterators

# Step 1: Read file content
function read_file(file_path::String)::String
    open(file_path, "r") do file
        return read(file, String)
    end
end

# Step 2: Split content into rules and updates sections
function split_sections(content::String)::Tuple{String, String}
    sections = split(content, "\n\n")
    return sections[1], sections[2]
end

# Step 3: Parse rules section into a list of tuples
function parse_rules(rules_section::String)::Vector{Tuple{Int, Int}}
    return [Tuple(map(x -> parse(Int, x), split(line, "|"))) for line in split(rules_section, "\n")]
end

# Step 4: Parse updates section into a list of lists
function parse_updates(updates_section::String)::Vector{Vector{Int}}
    return [map(x -> parse(Int, x), split(line, ",")) for line in split(updates_section, "\n") if !isempty(strip(line))]
end


# Step 5: Check if an update follows the rules
function is_update_ordered(update::Vector{Int}, rules::Vector{Tuple{Int, Int}})::Bool
    index_map = Dict(page => idx for (idx, page) in enumerate(update))
    return all(index_map[x] <= index_map[y] for (x, y) in rules if x in keys(index_map) && y in keys(index_map))
end

# Step 6: Extract middle page from an update
function get_middle_page(update::Vector{Int})::Int
    middle_index = div(length(update), 2)
    return update[middle_index + 1] # +1 because Julia uses 1-based indexing
end

# Step 7: Sum elements of a list
function sum_list(lst::Vector{Int})::Int
    return sum(lst)
end

# Step 8: Topological sort to correct an update without a queue
function topological_sort_update(update::Vector{Int}, rules::Vector{Tuple{Int, Int}})::Vector{Int}
    graph = Dict{Int, Vector{Int}}()
    in_degree = Dict{Int, Int}()
    nodes = Set(update)

    # Build the graph and in-degree count based on the rules
    for (x, y) in filter(rule -> rule[1] in nodes && rule[2] in nodes, rules)
        push!(get!(graph, x, []), y)
        in_degree[y] = get(in_degree, y, 0) + 1
        in_degree[x] = get(in_degree, x, 0)
    end

    # Perform topological sort without a queue
    sorted_update = Int[]
    while !isempty(nodes)
        for node in collect(nodes)
            if get(in_degree, node, 0) == 0
                push!(sorted_update, node)
                delete!(nodes, node)
                for neighbor in get(graph, node, [])
                    in_degree[neighbor] -= 1
                end
                break
            end
        end

        if isempty(nodes) && length(sorted_update) < length(update)
            error("Graph contains a cycle, topological sort not possible")
        end
    end

    return sorted_update
end

# Step 9: Process updates to identify correctly and incorrectly ordered updates
function process_updates(updates::Vector{Vector{Int}}, rules::Vector{Tuple{Int, Int}})
    correct_updates = filter(update -> is_update_ordered(update, rules), updates)
    correct_middle_pages = map(get_middle_page, correct_updates)
    sum_correct_middle = sum_list(correct_middle_pages)

    incorrect_updates = map(
        update -> topological_sort_update(update, rules),
        filter(update -> !is_update_ordered(update, rules), updates)
    )
    incorrect_middle_pages = map(get_middle_page, incorrect_updates)
    sum_incorrect_middle = sum_list(incorrect_middle_pages)

    return sum_correct_middle, sum_incorrect_middle
end

# Step 10: Main function to orchestrate the processing
function main(file_path::String = "input.txt")
    content = read_file(file_path)
    rules_section, updates_section = split_sections(content)
    rules = parse_rules(rules_section)
    updates = parse_updates(updates_section)
    sum_correct, sum_incorrect = process_updates(updates, rules)
    println("Sum of middle pages of correctly ordered updates: $sum_correct")
    println("Sum of middle pages of corrected updates: $sum_incorrect")
end

# Execute the main function
if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
