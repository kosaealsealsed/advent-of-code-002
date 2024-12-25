use std::fs::File;
use std::io::{self, BufRead};

fn calculate_total_distance(file_path: &str) -> io::Result<()> {
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

    // Sort both lists
    left_list.sort();
    right_list.sort();

    // Calculate the total distance
    let mut total_distance = 0;
    for (left, right) in left_list.iter().zip(right_list.iter()) {
        total_distance += (left - right).abs();
    }

    // Output the total distance
    println!("Total Distance: {}", total_distance);

    Ok(())
}

fn main() -> io::Result<()> {
    // Define the file path for input data
    let file_path = r"\\vmware-host\Shared Folders\C\advent-of-code-002\input-files\2024\2024-001\input.txt";

    // Call the function to calculate the total distance
    calculate_total_distance(file_path)
}
