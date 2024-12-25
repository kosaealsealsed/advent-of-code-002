use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};

fn calculate_similarity_score(file_path: &str) -> io::Result<()> {
    // Open the file
    let file = File::open(file_path)?;
    let reader = io::BufReader::new(file);

    // Initialize vectors to store the left and right list values
    let mut left_list = Vec::new();
    let mut right_list = Vec::new();

    // Read each line from the file
    for line in reader.lines() {
        let line = line?;
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() == 2 {
            let left: i32 = parts[0].parse().unwrap();
            let right: i32 = parts[1].parse().unwrap();
            left_list.push(left);
            right_list.push(right);
        }
    }

    // Create a hashmap to count occurrences of each number in the right list
    let mut right_list_counts = HashMap::new();
    for &num in &right_list {
        *right_list_counts.entry(num).or_insert(0) += 1;
    }

    // Calculate the similarity score
    let mut similarity_score = 0;
    for &left in &left_list {
        similarity_score += left * *right_list_counts.get(&left).unwrap_or(&0);
    }

    // Output the similarity score
    println!("Total Similarity Score: {}", similarity_score);

    Ok(())
}

fn main() -> io::Result<()> {
    // Define the file path for input data
    let file_path = r"\\vmware-host\Shared Folders\C\advent-of-code-002\input-files\2024\2024-001\input.txt";

    // Call the function to calculate the similarity score
    calculate_similarity_score(file_path)
}
