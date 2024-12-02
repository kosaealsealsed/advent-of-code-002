# Define the file path
file_path <- "input.txt"

# Read the file into a data frame
df <- read.table(file_path, header = FALSE)

# Extract the two columns into two lists
left_list <- df[[1]]
right_list <- df[[2]]

# Sort both lists
sorted_left <- sort(left_list)
sorted_right <- sort(right_list)

# Calculate the total distance between the two lists
total_distance <- sum(abs(sorted_left - sorted_right))

# Print the total distance
print(paste("Total Distance:", total_distance))
