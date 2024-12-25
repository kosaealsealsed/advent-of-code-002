use regex::Regex;
use std::fs;

fn sum_mul_operations(corrupted_memory: &str) -> i32 {
    // Define the regex pattern for mul(X, Y)
    let re = Regex::new(r"mul\((\d{1,3}),(\d{1,3})\)").unwrap();
    let mut total = 0;

    // Iterate over all matches and calculate the sum of products
    for caps in re.captures_iter(corrupted_memory) {
        let num1: i32 = caps[1].parse().unwrap();
        let num2: i32 = caps[2].parse().unwrap();
        total += num1 * num2;
    }

    total
}

fn sum_enabled_mul_operations(corrupted_memory: &str) -> i32 {
    // Define the regex pattern for do(), don't(), and mul(X, Y)
    let re = Regex::new(r"(do\(\)|don't\(\)|mul\((\d{1,3}),(\d{1,3})\))").unwrap();
    let mut total_sum = 0;
    let mut mul_enabled = true;

    // Iterate over all matches in order
    for caps in re.captures_iter(corrupted_memory) {
        let full_match = &caps[1];

        if full_match == "do()" {
            mul_enabled = true;
        } else if full_match == "don't()" {
            mul_enabled = false;
        } else if mul_enabled && caps.get(2).is_some() && caps.get(3).is_some() {
            // This is a mul(X, Y) instruction
            let num1: i32 = caps[2].parse().unwrap();
            let num2: i32 = caps[3].parse().unwrap();
            total_sum += num1 * num2;
        }
    }

    total_sum
}

fn main() {
    // Read the corrupted memory from 'input.txt'
    let file_path = r"\\vmware-host\Shared Folders\C\advent-of-code-002\input-files\2024\2024-003\input.txt";
    let corrupted_memory = fs::read_to_string(file_path).expect("Failed to read input file");

    // Calculate the results
    let total_sum_all_mul_operations = sum_mul_operations(&corrupted_memory);
    let total_sum_enabled_mul_operations = sum_enabled_mul_operations(&corrupted_memory);

    // Output the results
    println!("The sum of all mul operations is: {}", total_sum_all_mul_operations);
    println!("The sum of enabled mul operations is: {}", total_sum_enabled_mul_operations);
}
