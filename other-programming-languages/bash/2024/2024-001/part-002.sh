#!/bin/bash

# Input file path
file_path="/mnt/z/advent-of-code-002/input-files/2024/2024-001/input.txt"

# Extract the two columns into arrays
mapfile -t left_list < <(awk '{print $1}' "$file_path")
mapfile -t right_list < <(awk '{print $2}' "$file_path")

# Create an associative array to count occurrences in the right list
declare -A right_list_counts
for num in "${right_list[@]}"; do
  if [[ -n ${right_list_counts[$num]} ]]; then
    right_list_counts[$num]=$((right_list_counts[$num] + 1))
  else
    right_list_counts[$num]=1
  fi
done

# Calculate the similarity score
similarity_score=0
for left in "${left_list[@]}"; do
  count=${right_list_counts[$left]:-0} # Default to 0 if the number is not in the right list
  similarity_score=$((similarity_score + left * count))
done

# Output the result
echo "Similarity Score: $similarity_score"

