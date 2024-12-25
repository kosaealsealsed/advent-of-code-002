#!/bin/bash

# Input file path
file_path="/mnt/z/advent-of-code-002/input-files/2024/2024-001/input.txt"

# Extract the two columns, sort them, and store them in arrays
mapfile -t left_list < <(awk '{print $1}' "$file_path" | sort -n)
mapfile -t right_list < <(awk '{print $2}' "$file_path" | sort -n)

# Calculate the total distance
total_distance=0
for i in "${!left_list[@]}"; do
  left=${left_list[i]}
  right=${right_list[i]}
  distance=$((left - right))
  distance=${distance#-} # Take absolute value
  total_distance=$((total_distance + distance))
done

# Output the result
echo "Total Distance: $total_distance"
