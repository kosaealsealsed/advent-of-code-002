# Function to calculate the total similarity score
def calculate_similarity_score(file_path)
  # Read the file and load the location IDs into two arrays
  left_list = []
  right_list = []

  # Open the file and process each line
  File.foreach(file_path) do |line|
    # Split the line by spaces and convert the values to integers
    left, right = line.split.map(&:to_i)
    left_list << left
    right_list << right
  end

  # Create a hash to count occurrences of each number in the right list
  right_list_counts = Hash.new(0)
  right_list.each { |num| right_list_counts[num] += 1 }

  # Calculate the similarity score
  similarity_score = 0
  left_list.each do |left|
    similarity_score += left * right_list_counts[left]
  end

  # Output the similarity score
  puts "Total Similarity Score: #{similarity_score}"
end

# File path for input data
file_path = '\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\Ruby\\2024\\2024-001\\input.txt'

# Call the function to calculate the similarity score
calculate_similarity_score(file_path)
