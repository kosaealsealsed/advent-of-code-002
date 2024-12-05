# Step 1: Read file content
function content = read_file(file_path)
    fid = fopen(file_path, 'r');
    content = fread(fid, '*char')';
    fclose(fid);
end

# Step 2: Split content into rules and updates sections
function [rules_section, updates_section] = split_sections(content)
    sections = strsplit(strtrim(content), '\n\n');
    rules_section = sections{1};
    updates_section = sections{2};
end

# Step 3: Parse rules section into a list of tuples
function rules = parse_rules(rules_section)
    lines = strsplit(rules_section, '\n');
    rules = cellfun(@(line) sscanf(line, '%d|%d')', lines, 'UniformOutput', false);
    rules = vertcat(rules{:});
end

# Step 4: Parse updates section into a list of lists
function updates = parse_updates(updates_section)
    lines = strsplit(updates_section, '\n');
    updates = cellfun(@(line) sscanf(line, '%d,')', lines, 'UniformOutput', false);
end

# Step 5: Check if an update follows the rules
function result = is_update_ordered(update, rules)
    index_map = containers.Map(update, 1:numel(update));
    result = all(arrayfun(@(i) ...
        index_map.isKey(rules(i, 1)) && index_map.isKey(rules(i, 2)) && ...
        index_map(rules(i, 1)) <= index_map(rules(i, 2)), ...
        1:size(rules, 1)));
end

# Step 6: Extract middle page from an update
function middle_page = get_middle_page(update)
    middle_index = ceil(length(update) / 2);
    middle_page = update(middle_index);
end

# Step 7: Sum elements of a list
function total = sum_list(lst)
    total = sum(lst);
end

# Step 8: Topological sort to correct an update without a queue
function sorted_update = topological_sort_update(update, rules)
    nodes = unique(update);
    graph = containers.Map('KeyType', 'int32', 'ValueType', 'any');
    in_degree = containers.Map('KeyType', 'int32', 'ValueType', 'int32');

    # Initialize graph and in-degree map
    for i = 1:length(nodes)
        graph(nodes(i)) = [];
        in_degree(nodes(i)) = 0;
    end

    # Build the graph and in-degree count
    for i = 1:size(rules, 1)
        if ismember(rules(i, 1), nodes) && ismember(rules(i, 2), nodes)
            graph(rules(i, 1)) = [graph(rules(i, 1)), rules(i, 2)];
            in_degree(rules(i, 2)) = in_degree(rules(i, 2)) + 1;
        end
    end

    # Perform topological sort
    sorted_update = [];
    while ~isempty(nodes)
        for i = 1:length(nodes)
            if in_degree(nodes(i)) == 0
                sorted_update = [sorted_update, nodes(i)];
                neighbors = graph(nodes(i));
                for j = 1:length(neighbors)
                    in_degree(neighbors(j)) = in_degree(neighbors(j)) - 1;
                end
                nodes(i) = [];
                break;
            end
        end
        if isempty(sorted_update)
            error("Graph contains a cycle, topological sort not possible");
        end
    end
end

# Step 9: Process updates to identify correctly and incorrectly ordered updates
function [sum_correct, sum_incorrect] = process_updates(updates, rules)
    correct_updates = cellfun(@(update) is_update_ordered(update, rules), updates);
    correct_middle_pages = cellfun(@get_middle_page, updates(correct_updates));
    sum_correct = sum_list(correct_middle_pages);

    incorrect_updates = cellfun(@(update) ...
        topological_sort_update(update, rules), ...
        updates(~correct_updates), 'UniformOutput', false);
    incorrect_middle_pages = cellfun(@get_middle_page, incorrect_updates);
    sum_incorrect = sum_list(incorrect_middle_pages);
end

# Step 10: Main function to orchestrate the processing
function main(file_path)
    content = read_file(file_path);
    [rules_section, updates_section] = split_sections(content);
    rules = parse_rules(rules_section);
    updates = parse_updates(updates_section);
    [sum_correct, sum_incorrect] = process_updates(updates, rules);
    fprintf('Sum of middle pages of correctly ordered updates: %d\n', sum_correct);
    fprintf('Sum of middle pages of corrected updates: %d\n', sum_incorrect);
end

# Execute the main function
main('input.txt');
