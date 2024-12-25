#!/bin/bash

# Input file
file_path="input.txt"
target_word="XMAS"

# Read the grid into an array
IFS=$'\n' read -d '' -r -a grid < "$file_path"

# Get the grid dimensions
rows=${#grid[@]}
cols=${#grid[0]}

# Define directions for moving in the grid
directions=(
    "0 1"   # Right
    "1 0"   # Down
    "1 1"   # Diagonal-right-down
    "1 -1"  # Diagonal-left-down
    "0 -1"  # Left
    "-1 0"  # Up
    "-1 -1" # Diagonal-left-up
    "-1 1"  # Diagonal-right-up
)

# Function to check if a word exists in a given direction
check_word() {
    local x=$1 y=$2 dx=$3 dy=$4
    local len=${#target_word}
    for ((i = 0; i < len; i++)); do
        nx=$((x + i * dx))
        ny=$((y + i * dy))
        if ((nx < 0 || ny < 0 || nx >= rows || ny >= cols)); then
            return 1
        fi
        char="${grid[nx]:ny:1}"
        if [[ $char != ${target_word:i:1} ]]; then
            return 1
        fi
    done
    return 0
}

# Count occurrences of the target word
count_occurrences() {
    local count=0
    for ((r = 0; r < rows; r++)); do
        for ((c = 0; c < cols; c++)); do
            for dir in "${directions[@]}"; do
                read -r dx dy <<< "$dir"
                if check_word "$r" "$c" "$dx" "$dy"; then
                    ((count++))
                fi
            done
        done
    done
    echo "$count"
}

# Count all X-MAS patterns
count_xmas_patterns() {
    local count=0
    for ((r = 1; r < rows - 1; r++)); do
        for ((c = 1; c < cols - 1; c++)); do
            center="${grid[r]:c:1}"
            top_left="${grid[r-1]:c-1:1}"
            top_right="${grid[r-1]:c+1:1}"
            bottom_left="${grid[r+1]:c-1:1}"
            bottom_right="${grid[r+1]:c+1:1}"
            if [[ $center == "A" ]]; then
                if { [[ $top_left == "M" && $top_right == "S" && $bottom_left == "M" && $bottom_right == "S" ]] ||
                     [[ $top_left == "S" && $top_right == "M" && $bottom_left == "S" && $bottom_right == "M" ]] ||
                     [[ $top_left == "M" && $top_right == "M" && $bottom_left == "S" && $bottom_right == "S" ]] ||
                     [[ $top_left == "S" && $top_right == "S" && $bottom_left == "M" && $bottom_right == "M" ]]; }; then
                    ((count++))
                fi
            fi
        done
    done
    echo "$count"
}

# Main logic
word_count=$(count_occurrences)
echo "count is $word_count"

xmas_pattern_count=$(count_xmas_patterns)
echo "total xmas patterns is $xmas_pattern_count"
