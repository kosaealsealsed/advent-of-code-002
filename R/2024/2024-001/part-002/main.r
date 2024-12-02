# Define the file path
file_path <- "input.txt"

# Read the file into a data frame
df <- read.table(file_path, header = FALSE)

# Extract the two columns into two lists
left_list <- df[[1]]
right_list <- df[[2]]

# Convert the right list into a table (dictionary equivalent in R)
right_list_counts <- table(right_list)

# Calculate the similarity score
similarity_score <- sum(sapply(left_list, function(left) {
  count <- ifelse(left %in% names(right_list_counts), right_list_counts[as.character(left)], 0)
  left * count
}))

# Print the similarity score
print(paste("Similarity Score:", similarity_score))
