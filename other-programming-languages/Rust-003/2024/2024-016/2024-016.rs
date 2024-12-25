use std::collections::{BinaryHeap, HashSet, VecDeque};
use std::cmp::Reverse;

fn solve_maze(maze_lines: &[String]) -> Option<usize> {
    // Directions: 0=East, 1=South, 2=West, 3=North
    let directions = [
        (0, 1),   // East
        (1, 0),   // South
        (0, -1),  // West
        (-1, 0),  // North
    ];

    let rows = maze_lines.len();
    let cols = maze_lines[0].len();

    // Find S and E
    let mut start = None;
    let mut end = None;
    for (r, line) in maze_lines.iter().enumerate() {
        for (c, ch) in line.chars().enumerate() {
            match ch {
                'S' => start = Some((r, c)),
                'E' => end = Some((r, c)),
                _ => {}
            }
        }
    }

    let start = start.ok_or("Could not find 'S' in the maze.").unwrap();
    let end = end.ok_or("Could not find 'E' in the maze.").unwrap();

    // Dijkstra's algorithm
    const INF: usize = usize::MAX;
    let mut dist = vec![vec![vec![INF; 4]; cols]; rows];
    let start_dir = 0; // facing East
    dist[start.0][start.1][start_dir] = 0;

    let mut pq = BinaryHeap::new();
    pq.push(Reverse((0, start.0, start.1, start_dir)));

    let mut visited = HashSet::new();

    while let Some(Reverse((cost, r, c, d))) = pq.pop() {
        // If we've reached E, this is the minimal cost
        if (r, c) == end {
            return Some(cost);
        }

        if !visited.insert((r, c, d)) {
            continue;
        }

        let current_dist = dist[r][c][d];
        if cost > current_dist {
            continue;
        }

        // Explore neighbors:
        // 1) Move forward (cost + 1) if not a wall
        let (dr, dc) = directions[d];
        let nr = r as isize + dr;
        let nc = c as isize + dc;
        if nr >= 0 && nr < rows as isize && nc >= 0 && nc < cols as isize {
            let (nr, nc) = (nr as usize, nc as usize);
            if maze_lines[nr].chars().nth(nc).unwrap() != '#' {
                let new_cost = cost + 1;
                if new_cost < dist[nr][nc][d] {
                    dist[nr][nc][d] = new_cost;
                    pq.push(Reverse((new_cost, nr, nc, d)));
                }
            }
        }

        // 2) Turn left (cost + 1000)
        let left_dir = (d + 3) % 4;
        let new_cost = cost + 1000;
        if new_cost < dist[r][c][left_dir] {
            dist[r][c][left_dir] = new_cost;
            pq.push(Reverse((new_cost, r, c, left_dir)));
        }

        // 3) Turn right (cost + 1000)
        let right_dir = (d + 1) % 4;
        let new_cost = cost + 1000;
        if new_cost < dist[r][c][right_dir] {
            dist[r][c][right_dir] = new_cost;
            pq.push(Reverse((new_cost, r, c, right_dir)));
        }
    }

    None // If no path is found
}

fn solve_part2(maze_lines: &[String]) -> usize {
    let rows = maze_lines.len();
    let cols = maze_lines[0].len();

    // Directions: 0=East, 1=South, 2=West, 3=North
    let directions = [
        (0, 1),   // East
        (1, 0),   // South
        (0, -1),  // West
        (-1, 0),  // North
    ];

    let mut start = None;
    let mut end = None;

    for (r, line) in maze_lines.iter().enumerate() {
        for (c, ch) in line.chars().enumerate() {
            match ch {
                'S' => start = Some((r, c)),
                'E' => end = Some((r, c)),
                _ => {}
            }
        }
    }

    let start = start.expect("Could not find 'S' in the maze.");
    let end = end.expect("Could not find 'E' in the maze.");

    const INF: usize = usize::MAX;
    let mut dist = vec![vec![vec![INF; 4]; cols]; rows];
    dist[start.0][start.1][0] = 0;

    let mut pq = BinaryHeap::new();
    pq.push(Reverse((0, start.0, start.1, 0)));
    let mut visited = HashSet::new();

    while let Some(Reverse((cost, r, c, d))) = pq.pop() {
        if !visited.insert((r, c, d)) {
            continue;
        }

        let current_dist = dist[r][c][d];
        if cost > current_dist {
            continue;
        }

        let (dr, dc) = directions[d];
        let nr = r as isize + dr;
        let nc = c as isize + dc;
        if nr >= 0 && nr < rows as isize && nc >= 0 && nc < cols as isize {
            let (nr, nc) = (nr as usize, nc as usize);
            if maze_lines[nr].chars().nth(nc).unwrap() != '#' {
                let new_cost = cost + 1;
                if new_cost < dist[nr][nc][d] {
                    dist[nr][nc][d] = new_cost;
                    pq.push(Reverse((new_cost, nr, nc, d)));
                }
            }
        }

        for &turn in &[-1, 1] {
            let new_dir = (d as isize + turn).rem_euclid(4) as usize;
            let new_cost = cost + 1000;
            if new_cost < dist[r][c][new_dir] {
                dist[r][c][new_dir] = new_cost;
                pq.push(Reverse((new_cost, r, c, new_dir)));
            }
        }
    }

    let min_cost_end = (0..4).map(|d| dist[end.0][end.1][d]).min().unwrap();
    if min_cost_end == INF {
        return 0;
    }

    let mut on_best_path = vec![vec![false; cols]; rows];
    let mut queue = VecDeque::new();
    for d in 0..4 {
        if dist[end.0][end.1][d] == min_cost_end {
            queue.push_back((end.0, end.1, d));
        }
    }

    let mut visited_rev = HashSet::new();

    while let Some((r, c, d)) = queue.pop_front() {
        on_best_path[r][c] = true;
        let cost_here = dist[r][c][d];

        let (dr, dc) = directions[d];
        let prev_r = r as isize - dr;
        let prev_c = c as isize - dc;
        if prev_r >= 0 && prev_r < rows as isize && prev_c >= 0 && prev_c < cols as isize {
            let (prev_r, prev_c) = (prev_r as usize, prev_c as usize);
            if maze_lines[prev_r].chars().nth(prev_c).unwrap() != '#' {
                if cost_here >= 1 && dist[prev_r][prev_c][d] == cost_here - 1 {
                    if visited_rev.insert((prev_r, prev_c, d)) {
                        queue.push_back((prev_r, prev_c, d));
                    }
                }
            }
        }

        for &turn in &[-1, 1] {
            let prev_dir = (d as isize - turn).rem_euclid(4) as usize;
            let prev_cost = if cost_here >= 1000 { cost_here - 1000 } else { INF };
            if dist[r][c][prev_dir] == prev_cost {
                if visited_rev.insert((r, c, prev_dir)) {
                    queue.push_back((r, c, prev_dir));
                }
            }
        }
    }

    on_best_path.into_iter().flatten().filter(|&x| x).count()
}

fn main() {
    let input = std::fs::read_to_string("input.txt").expect("Failed to read input file");
    let maze_lines: Vec<String> = input.lines().map(|line| line.to_string()).collect();

    match solve_maze(&maze_lines) {
        Some(answer) => println!("Lowest possible score: {}", answer),
        None => println!("No path found!"),
    }

    let part2_result = solve_part2(&maze_lines);
    println!("Number of tiles on at least one best path: {}", part2_result);
}
