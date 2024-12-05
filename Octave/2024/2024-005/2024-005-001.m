% Let's first process the uploaded file to identify the rules and updates.
% Then, we will determine which updates are correctly ordered and calculate the sum of the middle page numbers.

% Reading the file content
file_path = 'input.txt';
fid = fopen(file_path, 'r');
content = fread(fid, '*char')';
fclose(fid);

% Splitting content into rules and updates
sections = strsplit(content, '\n\n');
rules_section = sections{1};
updates_section = sections{2};

% Parsing rules
rules_lines = strsplit(rules_section, '\n');
rules = zeros(length(rules_lines), 2);
for i = 1:length(rules_lines)
    rule = strsplit(rules_lines{i}, '|');
    rules(i, :) = [str2double(rule{1}), str2double(rule{2})];
end

% Parsing updates
updates_lines = strsplit(updates_section, '\n');
updates = cell(length(updates_lines), 1);
for i = 1:length(updates_lines)
    updates{i} = str2num(updates_lines{i});
end

% Helper function to check if an update follows the rules (optimized)
function ordered = is_update_ordered(update, rules)
    % Create a map for positions of each page in the update
    index_map = containers.Map('KeyType', 'int32', 'ValueType', 'int32');
    update_length = length(update);  % Avoid recomputing this multiple times
    for i = 1:update_length
        index_map(update(i)) = i;
    end
    
    % Check if any rule is violated
    ordered = true;
    for i = 1:size(rules, 1)
        x = rules(i, 1);
        y = rules(i, 2);
        
        if isKey(index_map, x) && isKey(index_map, y)
            if index_map(x) > index_map(y)
                ordered = false;
                return;  % Exit early as we found a violation
            end
        end
    end
end

% Optimized loop to process the updates
correct_updates = {};
middle_pages = [];
update_count = length(updates);  % Avoid recomputing in every iteration

for i = 1:update_count
    update = updates{i};
    if is_update_ordered(update, rules)
        correct_updates{end+1} = update;
        middle_pages(end+1) = update(floor(length(update) / 2) + 1);
    end
end

% Calculate the sum of middle pages
sum_middle_pages = sum(middle_pages);
disp(sum_middle_pages);
