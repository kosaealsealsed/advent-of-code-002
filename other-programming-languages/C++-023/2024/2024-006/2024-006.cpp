#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <unordered_set>
#include <unordered_map>
#include <chrono>
#include <stdexcept>
#include <utility>
#include <algorithm>
#include <functional>
#include <cctype>
#include <iomanip>

// Direction mappings
const std::unordered_map<char, int> DIRECTION_MAP = {
    {'^', 0},
    {'>', 1},
    {'v', 2},
    {'<', 3}
};

// Direction offsets: Up, Right, Down, Left
const int DIRECTION_OFFSETS[4][2] = {
    {-1, 0}, // Up
    {0, 1},  // Right
    {1, 0},  // Down
    {0, -1}  // Left
};

/**
 * Struct to represent a position in the grid.
 */
struct Position {
    int row;
    int col;

    bool operator==(const Position& other) const {
        return row == other.row && col == other.col;
    }
};

/**
 * Struct to represent a state (position and direction) of the guard.
 */
struct State {
    int row;
    int col;
    int direction;

    bool operator==(const State& other) const {
        return row == other.row && col == other.col && direction == other.direction;
    }
};

/**
 * Custom hash function for Position to be used in unordered_set.
 */
struct PositionHash {
    std::size_t operator()(const Position& pos) const {
        return std::hash<int>()(pos.row) ^ (std::hash<int>()(pos.col) << 1);
    }
};

/**
 * Custom hash function for State to be used in unordered_set.
 */
struct StateHash {
    std::size_t operator()(const State& state) const {
        return std::hash<int>()(state.row) ^ (std::hash<int>()(state.col) << 1) ^ (std::hash<int>()(state.direction) << 2);
    }
};

/**
 * Helper function to trim whitespace from both ends of a string.
 */
std::string trim(const std::string& s) {
    size_t start = 0;
    while (start < s.length() && std::isspace(s[start])) {
        start++;
    }
    size_t end = s.length();
    while (end > start && std::isspace(s[end - 1])) {
        end--;
    }
    return s.substr(start, end - start);
}

/**
 * Parses the grid from the given file.
 *
 * @param filePath Path to the input file.
 * @return 2D character vector representing the grid.
 * @throws std::runtime_error If the file cannot be read or grid is invalid.
 */
std::vector<std::vector<char>> parseGrid(const std::string& filePath) {
    std::vector<std::vector<char>> grid;
    std::ifstream infile(filePath);
    if (!infile.is_open()) {
        throw std::runtime_error("Unable to open file: " + filePath);
    }
    std::string line;
    while (std::getline(infile, line)) {
        std::string trimmed = trim(line);
        if (!trimmed.empty()) {
            grid.emplace_back(std::vector<char>(trimmed.begin(), trimmed.end()));
        }
    }
    infile.close();

    if (grid.empty()) {
        throw std::runtime_error("The grid is empty.");
    }

    // Ensure all rows have the same length
    size_t cols = grid[0].size();
    for (const auto& row : grid) {
        if (row.size() != cols) {
            throw std::runtime_error("Inconsistent row lengths in the grid.");
        }
    }

    return grid;
}

/**
 * Finds the guard's starting position and direction.
 *
 * @param grid 2D character vector representing the grid.
 * @return A pair containing the starting Position and direction.
 * @throws std::runtime_error If the guard is not found in the grid.
 */
std::pair<Position, int> findGuard(std::vector<std::vector<char>>& grid) {
    for (int r = 0; r < static_cast<int>(grid.size()); ++r) {
        for (int c = 0; c < static_cast<int>(grid[0].size()); ++c) {
            char cell = grid[r][c];
            auto it = DIRECTION_MAP.find(cell);
            if (it != DIRECTION_MAP.end()) {
                Position guardPos = Position{r, c};
                int guardDir = it->second;
                grid[r][c] = '.'; // Clear the starting position
                return {guardPos, guardDir};
            }
        }
    }
    throw std::runtime_error("Guard not found in the grid.");
}

/**
 * Retrieves all possible obstruction positions excluding the guard's starting position and already obstructed cells.
 *
 * @param grid     2D character vector representing the grid.
 * @param guardPos The starting position of the guard.
 * @return Vector of possible obstruction Positions.
 */
std::vector<Position> getPossibleObstructions(const std::vector<std::vector<char>>& grid, const Position& guardPos) {
    std::vector<Position> possible;
    for (int r = 0; r < static_cast<int>(grid.size()); ++r) {
        for (int c = 0; c < static_cast<int>(grid[0].size()); ++c) {
            if ((r != guardPos.row || c != guardPos.col) && grid[r][c] == '.') {
                possible.push_back(Position{r, c});
            }
        }
    }
    return possible;
}

/**
 * Simulates the guard's movement on the grid.
 *
 * @param grid      2D character vector representing the grid.
 * @param startPos  Starting position of the guard.
 * @param startDir  Starting direction of the guard.
 * @return True if a loop is detected, False if the guard exits the grid.
 */
bool simulateMovement(const std::vector<std::vector<char>>& grid, Position startPos, int startDir) {
    std::unordered_set<State, StateHash> visitedStates;
    int r = startPos.row;
    int c = startPos.col;
    int direction = startDir;

    while (true) {
        State currentState = State{r, c, direction};
        if (visitedStates.find(currentState) != visitedStates.end()) {
            return true; // Loop detected
        }
        visitedStates.insert(currentState);

        int dr = DIRECTION_OFFSETS[direction][0];
        int dc = DIRECTION_OFFSETS[direction][1];
        int newR = r + dr;
        int newC = c + dc;

        // Check boundaries
        if (newR < 0 || newR >= static_cast<int>(grid.size()) || newC < 0 || newC >= static_cast<int>(grid[0].size())) {
            return false; // Guard exits the grid
        }

        if (grid[newR][newC] == '#') {
            // Turn right if obstacle ahead
            direction = (direction + 1) % 4;
        } else {
            // Move forward
            r = newR;
            c = newC;
        }
    }
}

/**
 * Counts the number of distinct positions visited by the guard without any obstructions.
 *
 * @param grid      2D character vector representing the grid.
 * @param guardPos  Starting position of the guard.
 * @param guardDir  Starting direction of the guard.
 * @return Number of distinct positions visited.
 */
int countDistinctPositionsVisited(const std::vector<std::vector<char>>& grid, Position guardPos, int guardDir) {
    std::unordered_set<Position, PositionHash> visitedPositions;
    visitedPositions.insert(guardPos);

    Position currentPos = guardPos;
    int currentDir = guardDir;

    while (true) {
        int dr = DIRECTION_OFFSETS[currentDir][0];
        int dc = DIRECTION_OFFSETS[currentDir][1];
        int newR = currentPos.row + dr;
        int newC = currentPos.col + dc;

        // Check boundaries
        if (newR < 0 || newR >= static_cast<int>(grid.size()) || newC < 0 || newC >= static_cast<int>(grid[0].size())) {
            break; // Guard exits the mapped area
        }

        if (grid[newR][newC] == '#') {
            // Turn right if obstacle ahead
            currentDir = (currentDir + 1) % 4;
        } else {
            // Move forward
            currentPos = Position{newR, newC};
            visitedPositions.insert(currentPos);
        }
    }

    return static_cast<int>(visitedPositions.size());
}

/**
 * Counts the number of obstruction positions that cause the guard to loop indefinitely.
 * Also measures and prints execution times.
 *
 * @param grid      2D character vector representing the grid.
 * @param guardPos  Starting position of the guard.
 * @param guardDir  Starting direction of the guard.
 */
void countObstructionPositions(const std::vector<std::vector<char>>& originalGrid, Position guardPos, int guardDir) {
    // Start total timing
    auto totalStartTime = std::chrono::high_resolution_clock::now();

    // Copy the original grid to modify
    std::vector<std::vector<char>> grid = originalGrid;

    // Time to find obstruction positions
    auto obstructionStartTime = std::chrono::high_resolution_clock::now();
    std::vector<Position> possibleObstructions = getPossibleObstructions(grid, guardPos);
    auto obstructionEndTime = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> obstructionDuration = obstructionEndTime - obstructionStartTime;

    // Print the first header and line: [time_obstruction_positions] [total_obstruction_positions]
    std::cout << "time, denominator" << std::endl;
    std::cout << std::fixed << std::setprecision(9) << obstructionDuration.count() << " " << possibleObstructions.size() << std::endl;

    // Print header for batches
    std::cout << "batch, batch time, cumulative time" << std::endl;

    // Initialize loop counter
    int loopCount = 0;
    int total = static_cast<int>(possibleObstructions.size());

    // Initialize timing for batches
    const int batchSize = 1000;
    auto batchStartTime = std::chrono::high_resolution_clock::now();
    double cumulativeTime = obstructionDuration.count(); // cumulative_time includes obstruction_time

    for (int idx = 0; idx < total; ++idx) {
        Position obstruction = possibleObstructions[idx];
        grid[obstruction.row][obstruction.col] = '#'; // Place obstruction

        if (simulateMovement(grid, guardPos, guardDir)) {
            loopCount++; // Found a position that causes a loop
        }

        grid[obstruction.row][obstruction.col] = '.'; // Remove obstruction

        // Check if batch size is reached or it's the last position
        if ((idx + 1) % batchSize == 0 || (idx + 1) == total) {
            auto batchEndTime = std::chrono::high_resolution_clock::now();
            std::chrono::duration<double> batchDuration = batchEndTime - batchStartTime;
            double batchTime = batchDuration.count();
            cumulativeTime += batchTime;
            std::cout << (idx + 1) << " " << std::fixed << std::setprecision(9) << batchTime << " " << cumulativeTime << std::endl;
            batchStartTime = std::chrono::high_resolution_clock::now(); // Reset batch start time
        }
    }

    // End total timing
    auto totalEndTime = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> totalDuration = totalEndTime - totalStartTime;
    double totalTime = totalDuration.count(); // Total time from start to end

    // Print final answer header and line: [answer] [answer_time]
    std::cout << "answer, answer time" << std::endl;
    std::cout << loopCount << " " << std::fixed << std::setprecision(9) << totalTime << std::endl;
}

int main() {
    std::string filePath = "input.txt"; // Specify the path to your input file here
    try {
        // Parse the grid
        std::vector<std::vector<char>> grid = parseGrid(filePath);

        // Find the guard's starting position and direction
        auto [guardPos, guardDir] = findGuard(grid);

        // Part 1: Count distinct positions visited without obstructions
        int distinctPositions = countDistinctPositionsVisited(grid, guardPos, guardDir);
        std::cout << "Number of distinct positions visited: " << distinctPositions << std::endl;

        // Part 2: Detect loops with obstructions and measure execution times
        countObstructionPositions(grid, guardPos, guardDir);
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
