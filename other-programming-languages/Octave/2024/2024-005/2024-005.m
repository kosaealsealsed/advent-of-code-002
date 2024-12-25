function [rules, updates] = read_file(file_path)
    % Reads and processes the input file, returning rules and updates as structured data.
    
    % Open the file for reading
    fid = fopen(file_path, 'r');
    
    % Initialize empty containers for rules and updates
    rules = {};
    updates = {};
    
    % Read the entire file line by line
    content = {};
    while ~feof(fid)
        content{end+1} = fgetl(fid);  % Read each line and add it to the content cell array
    end
    
    % Close the file
    fclose(fid);
    
    % Join all lines into one string and split into sections based on double newlines
    content_str = strjoin(content, '\n');
    sections = strsplit(content_str, '\n\n');
    
    % Parse rules and updates based on the sections
    rules_section = sections{1};
    updates_section = sections{2};
    
    % Parse rules
    rule_lines = strsplit(rules_section, '\n');
    for i = 1:length(rule_lines)
        parts = str2double(strsplit(rule_lines{i}, '|'));
        rules{end+1} = parts;
    end
    
    % Parse updates
    update_lines = strsplit(updates_section, '\n');
    for i = 1:length(update_lines)
        updates{end+1} = str2double(strsplit(update_lines{i}, ','));
    end
end


function ordered = is_update_ordered(update, rules)
    % Checks if the given update follows the specified rules.
    ordered = true;
    index_map = containers.Map(update, 1:length(update));
    
    for i = 1:length(rules)
        rule = rules{i};
        x = rule(1);
        y = rule(2);
        
        if isKey(index_map, num2str(x)) && isKey(index_map, num2str(y))
            if index_map(num2str(x)) > index_map(num2str(y))
                ordered = false;
                return;
            end
        end
    end
end

function middle_page = find_middle_page(update)
    % Returns the middle page of a given update.
    middle_page = update(floor(length(update) / 2) + 1);
end

function sum_middle_pages = calculate_middle_pages_sum(updates, rules)
    % Identifies correctly ordered updates and calculates the sum of their middle pages.
    middle_pages = [];
    for i = 1:length(updates)
        update = updates{i};
        if is_update_ordered(update, rules)
            middle_pages = [middle_pages, find_middle_page(update)];
        end
    end
    sum_middle_pages = sum(middle_pages);
end

function sorted_update = topological_sort_update(update, rules)
    % Performs topological sort on an update based on the given rules and returns the sorted update.
    graph = containers.Map();
    in_degree = containers.Map();
    nodes = unique(update);
    
    % Initialize graph and in-degree count
    for i = 1:length(nodes)
        graph(num2str(nodes(i))) = [];
        in_degree(num2str(nodes(i))) = 0;
    end
    
    % Build graph from rules
    for i = 1:length(rules)
        rule = rules{i};
        x = rule(1);
        y = rule(2);
        
        if ismember(x, nodes) && ismember(y, nodes)
            graph(num2str(x)) = [graph(num2str(x)), y];
            in_degree(num2str(y)) = in_degree(num2str(y)) + 1;
        end
    end
    
    % Topological sort using Kahn's algorithm
    queue = {};
    sorted_update = [];
    
    % Find nodes with zero in-degree
    for i = 1:length(nodes)
        node = num2str(nodes(i));
        if in_degree(node) == 0
            queue{end+1} = node;
        end
    end
    
    % Process nodes
    while length(queue) > 0
        current = queue{1};
        queue(1) = [];
        sorted_update = [sorted_update, str2double(current)];
        
        neighbors = graph(current);
        for i = 1:length(neighbors)
            neighbor = num2str(neighbors(i));
            in_degree(neighbor) = in_degree(neighbor) - 1;
            if in_degree(neighbor) == 0
                queue{end+1} = neighbor;
            end
        end
    end
end

function sum_incorrect_middle_pages = process_updates(updates, rules)
    % Processes updates to identify incorrectly ordered updates, corrects them,
    % and calculates the sum of their middle pages.
    corrected_middle_pages = [];
    
    for i = 1:length(updates)
        update = updates{i};
        if ~is_update_ordered(update, rules)
            corrected_update = topological_sort_update(update, rules);
            corrected_middle_pages = [corrected_middle_pages, find_middle_page(corrected_update)];
        end
    end
    sum_incorrect_middle_pages = sum(corrected_middle_pages);
end

% Main Execution
file_path = 'input.txt';
[rules, updates] = read_file(file_path);

% Calculate sum of middle pages for correctly ordered updates
sum_correct_middle_pages = calculate_middle_pages_sum(updates, rules);
disp(['Sum of middle pages for correctly ordered updates: ', num2str(sum_correct_middle_pages)]);

% Calculate sum of middle pages for corrected updates
sum_incorrect_middle_pages = process_updates(updates, rules);
disp(['Sum of middle pages for corrected updates: ', num2str(sum_incorrect_middle_pages)]);
