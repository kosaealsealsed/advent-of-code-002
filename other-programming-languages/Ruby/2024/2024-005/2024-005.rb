require 'set'

# Reading the file content
file_path = 'input.txt'
content = File.read(file_path)

# Splitting content into rules and updates
rules_section, updates_section = content.strip.split("\n\n")
rules = rules_section.split("\n").map { |line| line.split("|").map(&:to_i) }
updates = updates_section.split("\n").map { |line| line.split(",").map(&:to_i) }

# Helper function to check if an update follows the rules
def update_ordered?(update, rules)
  index_map = update.each_with_index.to_h
  rules.all? do |x, y|
    !index_map.key?(x) || !index_map.key?(y) || index_map[x] <= index_map[y]
  end
end

# Identify correctly ordered updates and their middle page numbers
correct_updates = []
middle_pages = []

updates.each do |update|
  if update_ordered?(update, rules)
    correct_updates << update
    middle_pages << update[update.length / 2]
  end
end

# Calculate the sum of middle pages
sum_middle_pages = middle_pages.sum
puts "Sum of middle pages for correctly ordered updates: #{sum_middle_pages}"

# Helper function to sort an update according to the rules
def topological_sort(update, rules)
  # Build a graph and in-degree count based on the rules
  graph = Hash.new { |h, k| h[k] = [] }
  in_degree = Hash.new(0)
  nodes = update.to_set

  rules.each do |x, y|
    if nodes.include?(x) && nodes.include?(y)
      graph[x] << y
      in_degree[y] += 1
      in_degree[x] ||= 0
    end
  end

  # Perform topological sort
  queue = nodes.select { |node| in_degree[node] == 0 }
  sorted_update = []

  until queue.empty?
    current = queue.shift
    sorted_update << current

    graph[current].each do |neighbor|
      in_degree[neighbor] -= 1
      queue << neighbor if in_degree[neighbor] == 0
    end
  end

  sorted_update
end

# Correct the incorrectly ordered updates and find their middle page numbers
incorrect_updates = []
incorrect_middle_pages = []

updates.each do |update|
  unless update_ordered?(update, rules)
    corrected_update = topological_sort(update, rules)
    incorrect_updates << corrected_update
    incorrect_middle_pages << corrected_update[corrected_update.length / 2]
  end
end

# Calculate the sum of middle pages for corrected updates
sum_incorrect_middle_pages = incorrect_middle_pages.sum
puts "Sum of middle pages for corrected updates: #{sum_incorrect_middle_pages}"
