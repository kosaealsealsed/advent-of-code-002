use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;
use std::time::Instant;

const INPUT_FILE: &str = "input.txt";

fn read_map(filename: &str) -> Vec<Vec<i32>> {
    let mut grid = Vec::new();
    if let Ok(lines) = read_lines(filename) {
        for line in lines {
            if let Ok(line) = line {
                let row: Vec<i32> = line
                    .trim()
                    .chars()
                    .filter_map(|ch| ch.to_digit(10).map(|d| d as i32))
                    .collect();
                grid.push(row);
            }
        }
    }
    grid
}

fn neighbors(r: usize, c: usize, rows: usize, cols: usize) -> Vec<(usize, usize)> {
    let directions = [(-1, 0), (1, 0), (0, -1), (0, 1)];
    directions
        .iter()
        .filter_map(|&(dr, dc)| {
            let nr = r as isize + dr;
            let nc = c as isize + dc;
            if nr >= 0 && nr < rows as isize && nc >= 0 && nc < cols as isize {
                Some((nr as usize, nc as usize))
            } else {
                None
            }
        })
        .collect()
}

fn find_trailhead_scores(grid: &[Vec<i32>]) -> i32 {
    let rows = grid.len();
    let cols = grid[0].len();

    let mut trailheads = Vec::new();
    for r in 0..rows {
        for c in 0..cols {
            if grid[r][c] == 0 {
                trailheads.push((r, c));
            }
        }
    }

    let mut total_score = 0;

    for (start_r, start_c) in trailheads {
        let mut visited = HashSet::new();
        let mut queue = VecDeque::new();
        let mut reachable_nines = HashSet::new();

        queue.push_back((start_r, start_c));
        visited.insert((start_r, start_c));

        while let Some((r, c)) = queue.pop_front() {
            let current_height = grid[r][c];

            if current_height == 9 {
                reachable_nines.insert((r, c));
            } else {
                let next_height = current_height + 1;
                for (nr, nc) in neighbors(r, c, rows, cols) {
                    if !visited.contains(&(nr, nc)) && grid[nr][nc] == next_height {
                        visited.insert((nr, nc));
                        queue.push_back((nr, nc));
                    }
                }
            }
        }

        total_score += reachable_nines.len() as i32;
    }

    total_score
}

fn count_paths(
    r: usize,
    c: usize,
    grid: &[Vec<i32>],
    dp: &mut Vec<Vec<Option<i32>>>,
    rows: usize,
    cols: usize,
) -> i32 {
    if let Some(value) = dp[r][c] {
        return value;
    }

    let current_height = grid[r][c];

    if current_height == 9 {
        dp[r][c] = Some(1);
        return 1;
    }

    let next_height = current_height + 1;
    let mut total_paths = 0;

    for (nr, nc) in neighbors(r, c, rows, cols) {
        if grid[nr][nc] == next_height {
            total_paths += count_paths(nr, nc, grid, dp, rows, cols);
        }
    }

    dp[r][c] = Some(total_paths);
    total_paths
}

fn calculate_total_rating(grid: &[Vec<i32>]) -> i32 {
    let rows = grid.len();
    let cols = grid[0].len();

    let mut trailheads = Vec::new();
    for r in 0..rows {
        for c in 0..cols {
            if grid[r][c] == 0 {
                trailheads.push((r, c));
            }
        }
    }

    let mut dp = vec![vec![None; cols]; rows];

    let mut total_rating = 0;
    for (tr, tc) in trailheads {
        total_rating += count_paths(tr, tc, grid, &mut dp, rows, cols);
    }

    total_rating
}

fn main() {
    let start_part1 = Instant::now();
    let grid = read_map(INPUT_FILE);
    let total_score = find_trailhead_scores(&grid);
    let duration_part1 = start_part1.elapsed();

    println!("Part 1 Result: {}", total_score);
    println!("Time taken for Part 1: {:.9} s", duration_part1.as_secs_f64());

    let start_part2 = Instant::now();
    let total_rating = calculate_total_rating(&grid);
    let duration_part2 = start_part2.elapsed();

    println!("Part 2 Result: {}", total_rating);
    println!("Time taken for Part 2: {:.9} s", duration_part2.as_secs_f64());
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}
