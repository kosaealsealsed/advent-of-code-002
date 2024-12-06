use std::collections::HashSet;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;
use std::time::Instant;

// Direction mappings
const DIRECTION_MAP: [(&str, usize); 4] = [("^", 0), (">", 1), ("v", 2), ("<", 3)];
const DIRECTION_OFFSETS: [[i32; 2]; 4] = [
    [-1, 0], // Up
    [0, 1],  // Right
    [1, 0],  // Down
    [0, -1], // Left
];

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
struct Position {
    row: i32,
    col: i32,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
struct State {
    position: Position,
    direction: usize,
}

fn main() {
    let file_path = "input.txt"; // Specify the path to your input file here

    match count_distinct_positions_visited(file_path) {
        Ok(distinct_positions) => {
            println!("Number of distinct positions visited: {}", distinct_positions);
        }
        Err(err) => {
            eprintln!("Error: {}", err);
        }
    }

    if let Err(err) = count_obstruction_positions(file_path) {
        eprintln!("Error: {}", err);
    }
}

/// Part 1: Counts the number of distinct positions visited by the guard without any obstructions.
fn count_distinct_positions_visited(file_path: &str) -> io::Result<usize> {
    let grid = parse_grid(file_path)?;

    let (mut guard_pos, mut guard_dir) = find_guard(&grid)?;

    let mut visited_positions: HashSet<Position> = HashSet::new();
    visited_positions.insert(guard_pos.clone());

    loop {
        let dr = DIRECTION_OFFSETS[guard_dir][0];
        let dc = DIRECTION_OFFSETS[guard_dir][1];
        let new_r = guard_pos.row + dr;
        let new_c = guard_pos.col + dc;

        if new_r < 0 || new_r as usize >= grid.len() || new_c < 0 || new_c as usize >= grid[0].len() {
            break;
        }

        if grid[new_r as usize][new_c as usize] == '#' {
            guard_dir = (guard_dir + 1) % 4;
        } else {
            guard_pos = Position { row: new_r, col: new_c };
            visited_positions.insert(guard_pos.clone());
        }
    }

    Ok(visited_positions.len())
}

/// Part 2: Counts the number of obstruction positions that cause the guard to loop indefinitely.
fn count_obstruction_positions(file_path: &str) -> io::Result<()> {
    let grid = parse_grid(file_path)?;

    let (guard_pos, guard_dir) = find_guard(&grid)?;
    let possible_obstructions = get_possible_obstructions(&grid, &guard_pos);

    let total = possible_obstructions.len();

    let obstruction_start = Instant::now();
    let obstruction_time = obstruction_start.elapsed().as_secs_f64();

    println!("time, denominator");
    println!("{:.9} {}", obstruction_time, total);

    println!("batch, batch time, cumulative time");

    let mut loop_count = 0;
    let mut cumulative_time = obstruction_time;

    const BATCH_SIZE: usize = 1000;
    let mut batch_start = Instant::now();

    for (idx, obstruction) in possible_obstructions.iter().enumerate() {
        let mut grid_clone = grid.clone();
        grid_clone[obstruction.row as usize][obstruction.col as usize] = '#';

        if simulate_movement(&grid_clone, &guard_pos, guard_dir) {
            loop_count += 1;
        }

        grid_clone[obstruction.row as usize][obstruction.col as usize] = '.';

        if (idx + 1) % BATCH_SIZE == 0 || (idx + 1) == total {
            let batch_time = batch_start.elapsed().as_secs_f64();
            cumulative_time += batch_time;

            println!(
                "{} {:.9} {:.9}",
                idx + 1,
                batch_time,
                cumulative_time
            );

            batch_start = Instant::now();
        }
    }

    let total_time = obstruction_start.elapsed().as_secs_f64();

    println!("answer, answer time");
    println!("{}, {:.9}", loop_count, total_time);

    Ok(())
}

/// Parses the grid from the given file.
fn parse_grid(file_path: &str) -> io::Result<Vec<Vec<char>>> {
    let mut grid = Vec::new();

    if let Ok(lines) = read_lines(file_path) {
        for line in lines {
            let line = line?;
            grid.push(line.chars().collect());
        }
    }

    if grid.is_empty() {
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "The grid is empty."));
    }

    Ok(grid)
}

/// Finds the guard's starting position and direction.
fn find_guard(grid: &[Vec<char>]) -> io::Result<(Position, usize)> {
    for (r, row) in grid.iter().enumerate() {
        for (c, &cell) in row.iter().enumerate() {
            if let Some(dir) = DIRECTION_MAP.iter().find_map(|&(ch, dir)| {
                if cell == ch.chars().next().unwrap() {
                    Some(dir)
                } else {
                    None
                }
            }) {
                return Ok((Position { row: r as i32, col: c as i32 }, dir));
            }
        }
    }

    Err(io::Error::new(
        io::ErrorKind::InvalidInput,
        "Guard not found in the grid.",
    ))
}

/// Retrieves all possible obstruction positions excluding the guard's starting position and already obstructed cells.
fn get_possible_obstructions(grid: &[Vec<char>], guard_pos: &Position) -> Vec<Position> {
    let mut possible = Vec::new();

    for (r, row) in grid.iter().enumerate() {
        for (c, &cell) in row.iter().enumerate() {
            if (r as i32 != guard_pos.row || c as i32 != guard_pos.col) && cell == '.' {
                possible.push(Position {
                    row: r as i32,
                    col: c as i32,
                });
            }
        }
    }

    possible
}

/// Simulates the guard's movement on the grid.
fn simulate_movement(grid: &[Vec<char>], start_pos: &Position, start_dir: usize) -> bool {
    let mut visited_states: HashSet<State> = HashSet::new();
    let mut r = start_pos.row;
    let mut c = start_pos.col;
    let mut direction = start_dir;

    loop {
        let current_state = State {
            position: Position { row: r, col: c },
            direction,
        };

        if visited_states.contains(&current_state) {
            return true;
        }
        visited_states.insert(current_state);

        let dr = DIRECTION_OFFSETS[direction][0];
        let dc = DIRECTION_OFFSETS[direction][1];
        let new_r = r + dr;
        let new_c = c + dc;

        if new_r < 0 || new_r as usize >= grid.len() || new_c < 0 || new_c as usize >= grid[0].len()
        {
            return false;
        }

        if grid[new_r as usize][new_c as usize] == '#' {
            direction = (direction + 1) % 4;
        } else {
            r = new_r;
            c = new_c;
        }
    }
}

/// Reads lines from a file.
fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}
