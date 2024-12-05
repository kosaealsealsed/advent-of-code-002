# This program processes an input file to identify rules and updates.
# It determines which updates are correctly ordered based on the rules
# and calculates the sum of the middle page numbers of correctly ordered updates.

# Step 1: Reading the file content
file_path <- "input.txt"  # File containing the rules and updates
content <- readLines(file_path)

# Step 2: Splitting the content into rules and updates sections
sections <- unlist(strsplit(paste(content, collapse = "\n"), "\n\n"))  # Split by blank line
rules_section <- sections[1]  # The first part contains the rules
updates_section <- sections[2]  # The second part contains the updates

# Step 3: Parsing the rules section into a list of tuples
# Initialize an empty list to store rules
rules <- list()

# Split the rules section into individual lines
rule_lines <- unlist(strsplit(rules_section, "\n"))

# Process each rule line
for (line in rule_lines) {
  # Split the line into two parts using the "|" delimiter
  rule_parts <- as.integer(unlist(strsplit(line, "\\|")))
  rules <- append(rules, list(rule_parts))
}

# Step 4: Parsing the updates section into a list of lists
# Initialize an empty list to store updates
updates <- list()

# Split the updates section into individual lines
update_lines <- unlist(strsplit(updates_section, "\n"))

# Process each update line
for (line in update_lines) {
  # Split the line into individual numbers using the "," delimiter
  update_parts <- as.integer(unlist(strsplit(line, ",")))
  updates <- append(updates, list(update_parts))
}

# Step 5: Define a helper function to check if an update follows the rules
is_update_ordered <- function(update, rules) {
  # Create a mapping of page numbers to their indices
  index_map <- setNames(seq_along(update), update)
  
  # Check each rule to ensure it is respected
  for (rule in rules) {
    x <- rule[1]
    y <- rule[2]
    
    if (x %in% names(index_map) && y %in% names(index_map)) {
      x_index <- index_map[as.character(x)]
      y_index <- index_map[as.character(y)]
      
      # If x comes after y, the rule is violated
      if (x_index > y_index) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

# Step 6: Identify correctly ordered updates and calculate their middle page numbers
# Initialize lists for storing results
correct_updates <- list()
middle_pages <- numeric()

# Process each update
for (update in updates) {
  # Check if the update is correctly ordered
  if (is_update_ordered(update, rules)) {
    # Add the update to the list of correct updates
    correct_updates <- append(correct_updates, list(update))
    
    # Find the middle page of the update
    middle_index <- ceiling(length(update) / 2)
    middle_page <- update[middle_index]
    
    # Add the middle page to the list of middle pages
    middle_pages <- c(middle_pages, middle_page)
  }
}

# Step 7: Calculate the sum of middle page numbers
sum_middle_pages <- sum(middle_pages)

# Step 8: Output the result
cat("Sum of middle pages of correctly ordered updates:", sum_middle_pages, "\n")

# This program processes an input file to identify rules and updates,
# correcting incorrectly ordered updates and calculating results.

# Step 1: Define a helper function to perform a topological sort on an update according to the rules
topological_sort_update <- function(update, rules) {
  graph <- list()
  in_degree <- list()
  nodes <- unique(update)
  
  for (node in nodes) {
    graph[[as.character(node)]] <- list()
    in_degree[[as.character(node)]] <- 0
  }
  
  for (rule in rules) {
    x <- rule[1]
    y <- rule[2]
    
    if (x %in% nodes && y %in% nodes) {
      graph[[as.character(x)]] <- append(graph[[as.character(x)]], y)
      in_degree[[as.character(y)]] <- in_degree[[as.character(y)]] + 1
    }
  }
  
  queue <- c()
  for (node in nodes) {
    if (in_degree[[as.character(node)]] == 0) {
      queue <- append(queue, node)
    }
  }
  
  sorted_update <- c()
  while (length(queue) > 0) {
    current <- queue[1]
    queue <- queue[-1]
    sorted_update <- append(sorted_update, current)
    
    for (neighbor in graph[[as.character(current)]]) {
      in_degree[[as.character(neighbor)]] <- in_degree[[as.character(neighbor)]] - 1
      if (in_degree[[as.character(neighbor)]] == 0) {
        queue <- append(queue, neighbor)
      }
    }
  }
  
  return(sorted_update)
}

# Step 2: Correct the incorrectly ordered updates and find their middle page numbers
incorrect_updates <- list()
incorrect_middle_pages <- c()

for (update in updates) {
  if (!is_update_ordered(update, rules)) {
    corrected_update <- topological_sort_update(update, rules)
    incorrect_updates <- append(incorrect_updates, list(corrected_update))
    middle_index <- ceiling(length(corrected_update) / 2)
    middle_page <- corrected_update[middle_index]
    incorrect_middle_pages <- c(incorrect_middle_pages, middle_page)
  }
}

# Step 3: Calculate the sum of middle pages for corrected updates
sum_incorrect_middle_pages <- sum(incorrect_middle_pages)

# Step 4: Output the result
cat("Sum of middle pages of corrected updates:", sum_incorrect_middle_pages, "\n")
