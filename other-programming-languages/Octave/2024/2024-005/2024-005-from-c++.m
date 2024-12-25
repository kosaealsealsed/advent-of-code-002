% Function to split a string by a delimiter and return a cell array of tokens
function tokens = split_string(str, delimiter)
    tokens = {};
    prev = 1;
    while true
        pos = strfind(str(prev:end), delimiter);
        if isempty(pos)
            break;
        end
        pos = pos(1) + prev - 1;
        tokens{end + 1} = str(prev:pos - 1);
        prev = pos + length(delimiter);
    end
    if prev <= length(str)
        tokens{end + 1} = str(prev:end);
    end
end

% Function to trim whitespace from both ends of a string
function trimmed = trim(s)
    trimmed = strtrim(s);
end

% Function to check if an update is ordered according to the rules
function ordered = is_update_ordered(update, rules)
    index_map = containers.Map();
    for i = 1:length(update)
        index_map(num2str(update(i))) = i;
    end
    
    ordered = true;
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

% Function to perform topological sort on an update according to the rules
function sorted_update = topological_sort_update(update, rules)
    nodes = unique(update);
    graph = containers.Map();
    in_degree = containers.Map();
    
    % Initialize graph and in-degree
    for i = 1:length(nodes)
        graph(num2str(nodes(i))) = [];
        in_degree(num2str(nodes(i))) = 0;
    end
    
    % Build the graph based on rules
    for i = 1:length(rules)
        rule = rules{i};
        x = rule(1);
        y = rule(2);
        if isKey(graph, num2str(x)) && isKey(graph, num2str(y))
            graph(num2str(x)) = [graph(num2str(x)), y];
            in_degree(num2str(y)) = in_degree(num2str(y)) + 1;
        end
    end
    
    % Initialize queue with nodes having in-degree 0
    q = {};
    for i = 1:length(nodes)
        node = nodes(i);
        if in_degree(num2str(node)) == 0
            q{end + 1} = node;
        end
    end
    
    % Perform topological sort
    sorted_update = [];
    while ~isempty(q)
        current = q{1};
        q(1) = [];
        sorted_update = [sorted_update, current];
        
        for i = 1:length(graph(num2str(current)))
            neighbor = graph(num2str(current))(i);
            in_degree(num2str(neighbor)) = in_degree(num2str(neighbor)) - 1;
            if in_degree(num2str(neighbor)) == 0
                q{end + 1} = neighbor;
            end
        end
    end
    
    % Check if topological sort was possible (i.e., no cycles)
    if length(sorted_update) ~= length(nodes)
        sorted_update = [];
    end
end

% Function to get the middle page of an update
function middle_page = get_middle_page(update)
    middle_page = update(floor(length(update) / 2) + 1);
end

% Main logic
file_path = "input.txt";
fid = fopen(file_path, "r");
if fid == -1
    error("Error: Unable to open the file %s", file_path);
end

content = fread(fid, '*char')';
fclose(fid);
content = trim(content);

% Split content into rules and updates based on two consecutive newlines
sections = split_string(content, "\n\n");
if length(sections) != 2
    error("Invalid input format. Expected two sections separated by two newlines.");
end

rules_section = sections{1};
updates_section = sections{2};

% Parse rules
rules = {};
rule_lines = split_string(rules_section, "\n");
for i = 1:length(rule_lines)
    rule_line = trim(rule_lines{i});
    if isempty(rule_line)
        continue;
    end
    parts = split_string(rule_line, "|");
    if length(parts) != 2
        error("Invalid rule format: %s", rule_line);
    end
    try
        x = str2double(trim(parts{1}));
        y = str2double(trim(parts{2}));
        rules{end + 1} = [x, y];
    catch
        error("Invalid number in rule: %s", rule_line);
    end
end

% Parse updates
updates = {};
update_lines = split_string(updates_section, "\n");
for i = 1:length(update_lines)
    update_line = trim(update_lines{i});
    if isempty(update_line)
        continue;
    end
    parts = split_string(update_line, ",");
    update = [];
    valid = true;
    for j = 1:length(parts)
        part = trim(parts{j});
        try
            page = str2double(part);
            update = [update, page];
        catch
            error("Invalid number in update: %s", update_line);
            valid = false;
            break;
        end
    end
    if valid
        updates{end + 1} = update;
    end
end

% Identify correctly ordered updates and their middle page numbers
correct_updates = {};
middle_pages = [];

for i = 1:length(updates)
    update = updates{i};
    if is_update_ordered(update, rules)
        correct_updates{end + 1} = update;
        middle_pages = [middle_pages, get_middle_page(update)];
    end
end

% Calculate the sum of middle pages for correct updates
sum_middle_pages = sum(middle_pages);
fprintf("Sum of middle pages for correctly ordered updates: %d\n", sum_middle_pages);

% Identify incorrectly ordered updates, correct them, and collect their middle pages
incorrect_updates = {};
incorrect_middle_pages = [];

for i = 1:length(updates)
    update = updates{i};
    if ~is_update_ordered(update, rules)
        corrected_update = topological_sort_update(update, rules);
        if isempty(corrected_update)
            fprintf("Cycle detected or unable to sort update: ");
            fprintf("%d ", update{:});
            fprintf("\n");
            continue;
        end
        incorrect_updates{end + 1} = corrected_update;
        incorrect_middle_pages = [incorrect_middle_pages, get_middle_page(corrected_update)];
    end
end

% Calculate the sum of middle pages for corrected updates
sum_incorrect_middle_pages = sum(incorrect_middle_pages);
fprintf("Sum of middle pages for corrected updates: %d\n", sum_incorrect_middle_pages);
