use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

// Function to check if a report is safe
fn is_safe(report: &[i32]) -> bool {
    let differences: Vec<i32> = report.windows(2).map(|w| w[1] - w[0]).collect();

    let all_increasing = differences.iter().all(|&diff| diff >= 1 && diff <= 3);
    let all_decreasing = differences.iter().all(|&diff| diff <= -1 && diff >= -3);

    all_increasing || all_decreasing
}

// Function to check if a report is safe with the Problem Dampener
fn is_safe_with_dampener(report: &[i32]) -> bool {
    if is_safe(report) {
        return true;
    }

    for i in 0..report.len() {
        let mut modified_report = report.to_vec();
        modified_report.remove(i);
        if is_safe(&modified_report) {
            return true;
        }
    }

    false
}

fn main() -> io::Result<()> {
    let input_path = "input.txt";

    // Open the input file
    let input = File::open(&input_path)?;
    let reader = io::BufReader::new(input);

    // Read and process the reports
    let mut reports: Vec<Vec<i32>> = Vec::new();

    for line in reader.lines() {
        let line = line?;
        let report: Vec<i32> = line.split_whitespace().map(|s| s.parse().unwrap()).collect();
        reports.push(report);
    }

    // Count safe reports
    let safe_count = reports.iter().filter(|&report| is_safe(report)).count();
    println!("Safe reports: {}", safe_count);

    // Count safe reports with the Problem Dampener
    let safe_with_dampener_count = reports.iter().filter(|&report| is_safe_with_dampener(report)).count();
    println!("Safe reports with dampener: {}", safe_with_dampener_count);

    Ok(())
}
