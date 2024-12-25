# Reading the file content
file_path = "/uploads/input.txt"
content = File.read(file_path)

# Splitting content into rules and updates
rules_section, updates_section = content.strip.split("\n\n", 2)

rules = rules_section.split("\n").map do |line|
  x, y = line.split("|").map(&.to_i)
  {x, y}
end

updates = updates_section.split("\n").map do |line|
  line.split(",").map(&.to_i)
end

# Helper function to check if an update follows the rules
def is_update_ordered(update : Array(Int32), rules : Array({Int32, Int32})) : Bool
  index_map = update.each_with_index.to_h
  rules.each do |x, y|
    if index_map.has_key?(x) && index_map.has_key?(y) && index_map[x] > index_map[y]
      return false
    end
  end
  true
end

# Identify correctly ordered updates and their middle page numbers
correct_updates = [] of Array(Int32)
middle_pages = [] of Int32

updates.each do |update|
  if is_update_ordered(update, rules)
    correct_updates << update
    middle_pages << update[update.size // 2]
  end
end

# Calculate the sum of middle pages
sum_middle_pages = middle_pages.sum
puts "Sum of middle pages for correctly ordered updates: #{sum_middle_pages}"

# Helper function to sort an update according to the rules
def topological_sort_update(update : Array(Int32), rules : Array({Int32, Int32})) : Array(Int32)
  graph = Hash(Int32, Array(Int32)).new { |h, k| h[k] = [] of Int32 }
  in_degree = Hash(Int32, Int32).new(0)
  nodes = update.to_set

  rules.each do |x, y|
    if nodes.includes?(x) && nodes.includes?(y)
      graph[x] << y
      in_degree[y] += 1
      in_degree[x] ||= 0
    end
  end

  queue = nodes.select { |node| in_degree[node] == 0 }.to_a
  sorted_update = [] of Int32

  while queue.size > 0
    current = queue.shift
    sorted_update << current

    graph[current].each do |neighbor|
      in_degree[neighbor] -= 1
      if in_degree[neighbor] == 0
        queue << neighbor
      end
    end
  end

  sorted_update
end

# Correct the incorrectly ordered updates and find their middle page numbers
incorrect_updates = [] of Array(Int32)
incorrect_middle_pages = [] of Int32

updates.each do |update|
  unless is_update_ordered(update, rules)
    corrected_update = topological_sort_update(update, rules)
    incorrect_updates << corrected_update
    incorrect_middle_pages << corrected_update[corrected_update.size // 2]
  end
end

# Calculate the sum of middle pages for corrected updates
sum_incorrect_middle_pages = incorrect_middle_pages.sum
puts "Sum of middle pages for corrected updates: #{sum_incorrect_middle_pages}"
