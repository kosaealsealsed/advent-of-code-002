#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <queue>
#include <set>
#include <array>
#include <algorithm>
#include <stdexcept>
#include <limits>

constexpr int INF = std::numeric_limits<int>::max();

std::vector<std::string> parseInput(const std::string& filename) {
    std::vector<std::string> lines;
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Could not open file: " + filename);
    }

    std::string line;
    while (std::getline(file, line)) {
        lines.push_back(line);
    }

    return lines;
}

int solveMaze(const std::vector<std::string>& mazeLines) {
    std::array<std::array<int, 2>, 4> directions = {{{0, 1}, {1, 0}, {0, -1}, {-1, 0}}};

    int rows = mazeLines.size();
    int cols = mazeLines[0].size();
    std::array<int, 2> start = {-1, -1}, end = {-1, -1};

    for (int r = 0; r < rows; ++r) {
        for (int c = 0; c < cols; ++c) {
            char ch = mazeLines[r][c];
            if (ch == 'S') {
                start = {r, c};
            } else if (ch == 'E') {
                end = {r, c};
            }
        }
    }

    if (start[0] == -1 || end[0] == -1) {
        throw std::invalid_argument("Could not find 'S' or 'E' in the maze.");
    }

    std::vector<std::vector<std::vector<int>>> dist(rows, std::vector<std::vector<int>>(cols, std::vector<int>(4, INF)));
    dist[start[0]][start[1]][0] = 0;

    using State = std::array<int, 4>; // cost, row, col, direction
    auto compare = [](const State& a, const State& b) { return a[0] > b[0]; };
    std::priority_queue<State, std::vector<State>, decltype(compare)> pq(compare);

    pq.push({0, start[0], start[1], 0});
    std::set<std::string> visited;

    while (!pq.empty()) {
        State current = pq.top();
        pq.pop();

        int cost = current[0], r = current[1], c = current[2], d = current[3];

        if (r == end[0] && c == end[1]) {
            return cost;
        }

        std::string stateKey = std::to_string(r) + "," + std::to_string(c) + "," + std::to_string(d);
        if (visited.count(stateKey)) continue;
        visited.insert(stateKey);

        // Move forward
        int nr = r + directions[d][0];
        int nc = c + directions[d][1];
        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && mazeLines[nr][nc] != '#') {
            int newCost = cost + 1;
            if (newCost < dist[nr][nc][d]) {
                dist[nr][nc][d] = newCost;
                pq.push({newCost, nr, nc, d});
            }
        }

        // Turn left
        int leftDir = (d + 3) % 4;
        int leftCost = cost + 1000;
        if (leftCost < dist[r][c][leftDir]) {
            dist[r][c][leftDir] = leftCost;
            pq.push({leftCost, r, c, leftDir});
        }

        // Turn right
        int rightDir = (d + 1) % 4;
        int rightCost = cost + 1000;
        if (rightCost < dist[r][c][rightDir]) {
            dist[r][c][rightDir] = rightCost;
            pq.push({rightCost, r, c, rightDir});
        }
    }

    return -1; // No path found
}

int solvePart2(const std::vector<std::string>& mazeLines) {
    std::array<std::array<int, 2>, 4> directions = {{{0, 1}, {1, 0}, {0, -1}, {-1, 0}}};

    int rows = mazeLines.size();
    int cols = mazeLines[0].size();
    std::array<int, 2> start = {-1, -1}, end = {-1, -1};

    for (int r = 0; r < rows; ++r) {
        for (int c = 0; c < cols; ++c) {
            char ch = mazeLines[r][c];
            if (ch == 'S') {
                start = {r, c};
            } else if (ch == 'E') {
                end = {r, c};
            }
        }
    }

    if (start[0] == -1 || end[0] == -1) {
        throw std::invalid_argument("Could not find 'S' or 'E' in the maze.");
    }

    std::vector<std::vector<std::vector<int>>> dist(rows, std::vector<std::vector<int>>(cols, std::vector<int>(4, INF)));
    dist[start[0]][start[1]][0] = 0;

    using State = std::array<int, 4>; // cost, row, col, direction
    auto compare = [](const State& a, const State& b) { return a[0] > b[0]; };
    std::priority_queue<State, std::vector<State>, decltype(compare)> pq(compare);

    pq.push({0, start[0], start[1], 0});
    std::set<std::string> visited;

    while (!pq.empty()) {
        State current = pq.top();
        pq.pop();

        int cost = current[0], r = current[1], c = current[2], d = current[3];

        std::string stateKey = std::to_string(r) + "," + std::to_string(c) + "," + std::to_string(d);
        if (visited.count(stateKey)) continue;
        visited.insert(stateKey);

        // Move forward
        int nr = r + directions[d][0];
        int nc = c + directions[d][1];
        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && mazeLines[nr][nc] != '#') {
            int newCost = cost + 1;
            if (newCost < dist[nr][nc][d]) {
                dist[nr][nc][d] = newCost;
                pq.push({newCost, nr, nc, d});
            }
        }

        // Turn left
        int leftDir = (d + 3) % 4;
        int leftCost = cost + 1000;
        if (leftCost < dist[r][c][leftDir]) {
            dist[r][c][leftDir] = leftCost;
            pq.push({leftCost, r, c, leftDir});
        }

        // Turn right
        int rightDir = (d + 1) % 4;
        int rightCost = cost + 1000;
        if (rightCost < dist[r][c][rightDir]) {
            dist[r][c][rightDir] = rightCost;
            pq.push({rightCost, r, c, rightDir});
        }
    }

    int minCostEnd = INF;
    for (int d = 0; d < 4; ++d) {
        minCostEnd = std::min(minCostEnd, dist[end[0]][end[1]][d]);
    }

    if (minCostEnd == INF) {
        return 0;
    }

    std::vector<std::vector<bool>> onBestPath(rows, std::vector<bool>(cols, false));
    std::queue<std::array<int, 3>> queue; // row, col, direction

    for (int d = 0; d < 4; ++d) {
        if (dist[end[0]][end[1]][d] == minCostEnd) {
            queue.push({end[0], end[1], d});
        }
    }

    std::set<std::string> visitedReverse;

    while (!queue.empty()) {
        auto current = queue.front();
        queue.pop();

        int r = current[0], c = current[1], d = current[2];
        onBestPath[r][c] = true;

        int costHere = dist[r][c][d];

        int nr = r - directions[d][0];
        int nc = c - directions[d][1];
        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
            if (mazeLines[nr][nc] != '#' && dist[nr][nc][d] == costHere - 1) {
                std::string reverseKey = std::to_string(nr) + "," + std::to_string(nc) + "," + std::to_string(d);
                if (!visitedReverse.count(reverseKey)) {
                    visitedReverse.insert(reverseKey);
                    queue.push({nr, nc, d});
                }
            }
        }

        for (int prevDir : {(d + 3) % 4, (d + 1) % 4}) {
            if (dist[r][c][prevDir] == costHere - 1000) {
                std::string reverseKey = std::to_string(r) + "," + std::to_string(c) + "," + std::to_string(prevDir);
                if (!visitedReverse.count(reverseKey)) {
                    visitedReverse.insert(reverseKey);
                    queue.push({r, c, prevDir});
                }
            }
        }
    }

    int tilesOnBestPath = 0;
    for (int r = 0; r < rows; ++r) {
        for (int c = 0; c < cols; ++c) {
            if (onBestPath[r][c]) tilesOnBestPath++;
        }
    }

    return tilesOnBestPath;
}

int main() {
    try {
        std::vector<std::string> mazeLines = parseInput("input.txt");

        int part1Result = solveMaze(mazeLines);
        std::cout << "Lowest possible score (Part 1): " << part1Result << std::endl;

        int part2Result = solvePart2(mazeLines);
        std::cout << "Number of tiles on at least one best path (Part 2): " << part2Result << std::endl;

    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
    }

    return 0;
}
