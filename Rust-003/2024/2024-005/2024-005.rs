use std::collections::{HashMap, HashSet, VecDeque};
use std::error::Error;
use std::fs;
use std::path::Path;

/// Splits a string by a delimiter and returns a vector of string slices.
/// The returned slices have the same lifetime as the input string.
fn split<'a>(s: &'a str, delimiter: &str) -> Vec<&'a str> {
    s.split(delimiter).collect()
}

/// Trims whitespace from both ends of a string slice.
/// The returned slice has the same lifetime as the input string.
fn trim<'a>(s: &'a str) -> &'a str {
    s.trim()
}

/// Checks if an update is ordered according to the given rules.
fn is_update_ordered(update: &[i32], rules: &[(i32, i32)]) -> bool {
    let index_map: HashMap<i32, usize> = update.iter().cloned().enumerate().map(|(i, x)| (x, i)).collect();

    for &(x, y) in rules {
        if let (Some(&ix), Some(&iy)) = (index_map.get(&x), index_map.get(&y)) {
            if ix > iy {
                return false;
            }
        }
    }

    true
}

/// Performs a topological sort on an update according to the given rules.
/// Returns an empty vector if a cycle is detected or sorting is impossible.
fn topological_sort_update(update: &[i32], rules: &[(i32, i32)]) -> Vec<i32> {
    let mut graph: HashMap<i32, Vec<i32>> = HashMap::new();
    let mut in_degree: HashMap<i32, usize> = HashMap::new();
    let nodes: HashSet<i32> = update.iter().cloned().collect();

    // Initialize graph and in-degree
    for &node in &nodes {
        graph.entry(node).or_insert_with(Vec::new);
        in_degree.entry(node).or_insert(0);
    }

    // Build the graph based on rules
    for &(x, y) in rules {
        if nodes.contains(&x) && nodes.contains(&y) {
            graph.get_mut(&x).unwrap().push(y);
            *in_degree.get_mut(&y).unwrap() += 1;
        }
    }

    // Initialize queue with nodes having in-degree 0
    let mut queue: VecDeque<i32> = in_degree.iter()
        .filter(|&(_, &deg)| deg == 0)
        .map(|(&node, _)| node)
        .collect();

    let mut sorted_update: Vec<i32> = Vec::new();

    while let Some(current) = queue.pop_front() {
        sorted_update.push(current);

        for &neighbor in &graph[&current] {
            if let Some(degree) = in_degree.get_mut(&neighbor) {
                *degree -= 1;
                if *degree == 0 {
                    queue.push_back(neighbor);
                }
            }
        }
    }

    if sorted_update.len() == nodes.len() {
        sorted_update
    } else {
        // Cycle detected or unable to sort
        Vec::new()
    }
}

/// Retrieves the middle page number from the update list.
/// For even-sized lists, it takes the upper middle (e.g., index `len/2`).
fn get_middle_page(update: &[i32]) -> i32 {
    update[update.len() / 2]
}

fn main() -> Result<(), Box<dyn Error>> {
    let file_path = Path::new("input.txt");

    // Read the entire file content
    let content = fs::read_to_string(file_path)?;
    let content = content.trim();

    // Split content into rules and updates based on two consecutive newlines
    let sections: Vec<&str> = split(content, "\n\n");
    if sections.len() != 2 {
        eprintln!("Invalid input format. Expected two sections separated by two newlines.");
        return Ok(());
    }

    let rules_section = sections[0];
    let updates_section = sections[1];

    // Parse rules
    let mut rules: Vec<(i32, i32)> = Vec::new();
    let rule_lines = split(rules_section, "\n");
    for rule_line_raw in rule_lines {
        let rule_line = trim(rule_line_raw);
        if rule_line.is_empty() {
            continue;
        }
        let parts: Vec<&str> = split(rule_line, "|");
        if parts.len() != 2 {
            eprintln!("Invalid rule format: {}", rule_line);
            continue;
        }
        let x: i32 = match parts[0].trim().parse() {
            Ok(num) => num,
            Err(_) => {
                eprintln!("Invalid number in rule: {}", rule_line);
                continue;
            }
        };
        let y: i32 = match parts[1].trim().parse() {
            Ok(num) => num,
            Err(_) => {
                eprintln!("Invalid number in rule: {}", rule_line);
                continue;
            }
        };
        rules.push((x, y));
    }

    // Parse updates
    let mut updates: Vec<Vec<i32>> = Vec::new();
    let update_lines = split(updates_section, "\n");
    for update_line_raw in update_lines {
        let update_line = trim(update_line_raw);
        if update_line.is_empty() {
            continue;
        }
        let parts: Vec<&str> = split(update_line, ",");
        let mut update: Vec<i32> = Vec::new();
        let mut valid = true;
        for part_raw in parts {
            let part = trim(part_raw);
            match part.parse::<i32>() {
                Ok(num) => update.push(num),
                Err(_) => {
                    eprintln!("Invalid number in update: {}", update_line);
                    valid = false;
                    break;
                }
            }
        }
        if valid {
            updates.push(update);
        }
    }

    // Identify correctly ordered updates and their middle page numbers
    let mut correct_updates: Vec<Vec<i32>> = Vec::new();
    let mut middle_pages: Vec<i32> = Vec::new();

    for update in &updates {
        if is_update_ordered(update, &rules) {
            correct_updates.push(update.clone());
            middle_pages.push(get_middle_page(update));
        }
    }

    // Calculate the sum of middle pages for correct updates
    let sum_middle_pages: i32 = middle_pages.iter().sum();
    println!("Sum of middle pages for correctly ordered updates: {}", sum_middle_pages);

    // Identify incorrectly ordered updates, correct them, and collect their middle pages
    let mut incorrect_updates: Vec<Vec<i32>> = Vec::new();
    let mut incorrect_middle_pages: Vec<i32> = Vec::new();

    for update in &updates {
        if !is_update_ordered(update, &rules) {
            let corrected_update = topological_sort_update(update, &rules);
            if corrected_update.is_empty() {
                eprintln!("Cycle detected or unable to sort update: {:?}", update);
                continue;
            }
            incorrect_updates.push(corrected_update.clone());
            incorrect_middle_pages.push(get_middle_page(&corrected_update));
        }
    }

    // Calculate the sum of middle pages for corrected updates
    let sum_incorrect_middle_pages: i32 = incorrect_middle_pages.iter().sum();
    println!("Sum of middle pages for corrected updates: {}", sum_incorrect_middle_pages);

    Ok(())
}
