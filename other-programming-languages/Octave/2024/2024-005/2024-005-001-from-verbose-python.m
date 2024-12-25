% This program processes an input file to identify rules and updates.
% It determines which updates are correctly ordered based on the rules
% and calculates the sum of the middle page numbers of correctly ordered updates.

% Step 1: Reading the file content
file_path = 'input.txt';  % File containing the rules and updates
file_content = fileread(file_path);

% Step 2: Splitting the content into rules and updates sections
sections = strsplit(strtrim(file_content), "\n\n");  % Split by blank line
rules_section = sections{1};  % The first part contains the rules
updates_section = sections{2};  % The second part contains the updates

% Step 3: Parsing the rules section into a list of tuples
rules = [];  % Initialize an empty matrix to store rules
rule_lines = strsplit(rules_section, "\n");  % Split the rules section into lines

% Process each rule line
for i = 1:length(rule_lines)
    rule_parts = strsplit(rule_lines{i}, "|");
    x = str2double(rule_parts{1});
    y = str2double(rule_parts{2});
    rules = [rules; x, y];  % Append the rule as a row in the matrix
end

% Step 4: Parsing the updates section into a list of lists
updates = {};  % Initialize a cell array to store updates
update_lines = strsplit(updates_section, "\n");  % Split the updates section into lines

% Process each update line
for i = 1:length(update_lines)
    update_parts = strsplit(update_lines{i}, ",");
    update = cellfun(@str2double, update_parts);  % Convert strings to numbers
    updates{end+1} = update;  % Append the update to the updates list
end

% Step 5: Define a helper function to check if an update follows the rules
function is_ordered = is_update_ordered(update, rules)
    % Create a mapping of page numbers to their indices
    index_map = containers.Map('KeyType', 'double', 'ValueType', 'double');
    for i = 1:length(update)
        index_map(update(i)) = i;
    end

    % Check each rule to ensure it is respected
    for i = 1:size(rules, 1)
        x = rules(i, 1);
        y = rules(i, 2);
        if isKey(index_map, x) && isKey(index_map, y)
            x_index = index_map(x);
            y_index = index_map(y);
            if x_index > y_index
                is_ordered = false;
                return;
            end
        end
    end
    is_ordered = true;
end

% Step 6: Identify correctly ordered updates and calculate their middle page numbers
correct_updates = {};  % List of correctly ordered updates
middle_pages = [];  % List of middle pages

for i = 1:length(updates)
    update = updates{i};
    if is_update_ordered(update, rules)
        correct_updates{end+1} = update;  % Add to correct updates
        middle_index = ceil(length(update) / 2);  % Find the middle index
        middle_pages(end+1) = update(middle_index);  % Add the middle page
    end
end

% Step 7: Calculate the sum of middle page numbers
sum_middle_pages = sum(middle_pages);

% Step 8: Output the result
fprintf('Sum of middle pages of correctly ordered updates: %d\n', sum_middle_pages);

% Step 1: Define a helper function to sort an update according to the rules
function sorted_update = topological_sort_update(update, rules)
    % Initialize data structures for the graph and in-degree counts
    nodes = unique(update);  % Unique nodes in the update
    graph = containers.Map('KeyType', 'double', 'ValueType', 'any');
    in_degree = containers.Map('KeyType', 'double', 'ValueType', 'double');
    
    % Initialize graph and in-degree for all nodes
    for i = 1:length(nodes)
        graph(nodes(i)) = [];
        in_degree(nodes(i)) = 0;
    end
    
    % Build the graph and in-degree count based on the rules
    for i = 1:size(rules, 1)
        x = rules(i, 1);
        y = rules(i, 2);
        if ismember(x, nodes) && ismember(y, nodes)
            graph(x) = [graph(x), y];
            in_degree(y) = in_degree(y) + 1;
        end
    end
    
    % Initialize a queue for the topological sort
    queue = [];
    for i = 1:length(nodes)
        if in_degree(nodes(i)) == 0
            queue = [queue, nodes(i)];
        end
    end
    
    % Perform the topological sort
    sorted_update = [];
    while ~isempty(queue)
        current = queue(1);  % Get the first element in the queue
        queue(1) = [];       % Remove it from the queue
        sorted_update = [sorted_update, current];  % Add it to the sorted update
        
        % Process neighbors
        neighbors = graph(current);
        for j = 1:length(neighbors)
            neighbor = neighbors(j);
            in_degree(neighbor) = in_degree(neighbor) - 1;
            if in_degree(neighbor) == 0
                queue = [queue, neighbor];
            end
        end
    end
end

% Step 2: Correct the incorrectly ordered updates and find their middle page numbers
incorrect_updates = {};  % Store incorrectly ordered updates after correction
incorrect_middle_pages = [];  % Store the middle page numbers of corrected updates

for i = 1:length(updates)
    update = updates{i};
    % Check if the update is not correctly ordered
    if ~is_update_ordered(update, rules)
        % Correct the order using the topological sort helper function
        corrected_update = topological_sort_update(update, rules);
        incorrect_updates{end+1} = corrected_update;  % Add to the list of corrected updates
        
        % Calculate the middle page number of the corrected update
        middle_index = ceil(length(corrected_update) / 2);
        middle_page = corrected_update(middle_index);
        incorrect_middle_pages(end+1) = middle_page;  % Add to the middle pages list
    end
end

% Step 3: Calculate the sum of middle pages for corrected updates
sum_incorrect_middle_pages = sum(incorrect_middle_pages);

% Step 4: Output the result
fprintf('Sum of middle pages of corrected updates: %d\n', sum_incorrect_middle_pages);
