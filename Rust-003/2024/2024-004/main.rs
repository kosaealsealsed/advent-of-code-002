use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() -> io::Result<()> {
    let file_path = "input.txt";

    // Read the file and store the grid
    let grid: Vec<String> = read_lines(file_path)?
        .filter_map(|line| line.ok())
        .collect();
    let rows = grid.len();
    let cols = grid[0].len();

    // Count occurrences of "XMAS"
    let xmas_count = count_xmas(&grid, rows, cols);
    println!("Count of XMAS: {}", xmas_count);

    // Count all X-MAS patterns
    let xmas_patterns = count_all_xmas_patterns(&grid, rows, cols);
    println!("Total X-MAS patterns: {}", xmas_patterns);

    Ok(())
}

// Count occurrences of "XMAS" in all directions
fn count_xmas(grid: &[String], rows: usize, cols: usize) -> usize {
    let target_word = "XMAS";
    let directions = [
        (0, 1),   // Right
        (1, 0),   // Down
        (1, 1),   // Diagonal-right-down
        (1, -1),  // Diagonal-left-down
        (0, -1),  // Left
        (-1, 0),  // Up
        (-1, -1), // Diagonal-left-up
        (-1, 1),  // Diagonal-right-up
    ];

    let mut count = 0;

    for r in 0..rows {
        for c in 0..cols {
            for &(dx, dy) in &directions {
                if check_word(grid, r, c, dx, dy, target_word, rows, cols) {
                    count += 1;
                }
            }
        }
    }

    count
}

// Check if a word exists in a given direction
fn check_word(
    grid: &[String],
    x: usize,
    y: usize,
    dx: isize,
    dy: isize,
    word: &str,
    rows: usize,
    cols: usize,
) -> bool {
    word.chars().enumerate().all(|(i, ch)| {
        let nx = x as isize + i as isize * dx;
        let ny = y as isize + i as isize * dy;
        nx >= 0
            && ny >= 0
            && (nx as usize) < rows
            && (ny as usize) < cols
            && grid[nx as usize].as_bytes()[ny as usize] as char == ch
    })
}

// Count all X-MAS patterns
fn count_all_xmas_patterns(grid: &[String], rows: usize, cols: usize) -> usize {
    let mut count = 0;

    for r in 1..rows - 1 {
        for c in 1..cols - 1 {
            let center = grid[r].as_bytes()[c] as char;
            let top_left = grid[r - 1].as_bytes()[c - 1] as char;
            let top_right = grid[r - 1].as_bytes()[c + 1] as char;
            let bottom_left = grid[r + 1].as_bytes()[c - 1] as char;
            let bottom_right = grid[r + 1].as_bytes()[c + 1] as char;

            if center == 'A' {
                // Pattern 1: M.S
                if top_left == 'M' && top_right == 'S' && bottom_left == 'M' && bottom_right == 'S' {
                    count += 1;
                }
                // Pattern 2: S.M
                else if top_left == 'S'
                    && top_right == 'M'
                    && bottom_left == 'S'
                    && bottom_right == 'M'
                {
                    count += 1;
                }
                // Pattern 3: M.M
                else if top_left == 'M'
                    && top_right == 'M'
                    && bottom_left == 'S'
                    && bottom_right == 'S'
                {
                    count += 1;
                }
                // Pattern 4: S.S
                else if top_left == 'S'
                    && top_right == 'S'
                    && bottom_left == 'M'
                    && bottom_right == 'M'
                {
                    count += 1;
                }
            }
        }
    }

    count
}

// Helper function to read lines from a file
fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}
