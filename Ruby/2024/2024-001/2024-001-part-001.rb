# Function to calculate the total distance between two sorted lists
def calculate_total_distance(file_path)
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

  # Sort both lists
  left_list.sort!
  right_list.sort!

  # Calculate the total distance
  total_distance = 0
  left_list.each_with_index do |left, index|
    right = right_list[index]
    total_distance += (left - right).abs
  end

  # Output the total distance
  puts "Total Distance: #{total_distance}"
end

# File path for input data
file_path = '\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\Ruby\\2024\\2024-001\\input.txt'

# Call the function to calculate the total distance
calculate_total_distance(file_path)
