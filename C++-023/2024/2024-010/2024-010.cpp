#include <iostream>
#include <fstream>
#include <vector>
#include <queue>
#include <set>
#include <chrono>
#include <string>

using namespace std;

const string INPUT_FILE = R"(input.txt)";

vector<vector<int>> readMap(const string& filename) {
    vector<vector<int>> grid;
    ifstream file(filename);
    string line;

    while (getline(file, line)) {
        if (!line.empty()) {
            vector<int> row;
            for (char ch : line) {
                row.push_back(ch - '0');
            }
            grid.push_back(row);
        }
    }

    return grid;
}

vector<pair<int, int>> neighbors(int r, int c, int rows, int cols) {
    vector<pair<int, int>> result;
    vector<pair<int, int>> directions = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};

    for (auto [dr, dc] : directions) {
        int nr = r + dr, nc = c + dc;
        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
            result.emplace_back(nr, nc);
        }
    }

    return result;
}

int findTrailheadScores(const vector<vector<int>>& grid) {
    int rows = grid.size();
    int cols = rows > 0 ? grid[0].size() : 0;

    vector<pair<int, int>> trailheads;
    for (int r = 0; r < rows; ++r) {
        for (int c = 0; c < cols; ++c) {
            if (grid[r][c] == 0) {
                trailheads.emplace_back(r, c);
            }
        }
    }

    int totalScore = 0;

    for (auto [startR, startC] : trailheads) {
        set<pair<int, int>> visited;
        queue<pair<int, int>> q;
        set<pair<int, int>> reachableNines;

        q.push({startR, startC});
        visited.insert({startR, startC});

        while (!q.empty()) {
            auto [r, c] = q.front();
            q.pop();

            int currentHeight = grid[r][c];

            if (currentHeight == 9) {
                reachableNines.insert({r, c});
            } else {
                int nextHeight = currentHeight + 1;
                for (auto [nr, nc] : neighbors(r, c, rows, cols)) {
                    if (!visited.count({nr, nc}) && grid[nr][nc] == nextHeight) {
                        visited.insert({nr, nc});
                        q.push({nr, nc});
                    }
                }
            }
        }

        totalScore += reachableNines.size();
    }

    return totalScore;
}

int countPaths(int r, int c, const vector<vector<int>>& grid, vector<vector<int>>& dp, int rows, int cols) {
    if (dp[r][c] != -1) {
        return dp[r][c];
    }

    int currentHeight = grid[r][c];

    if (currentHeight == 9) {
        dp[r][c] = 1;
        return 1;
    }

    int totalPaths = 0;
    int nextHeight = currentHeight + 1;
    for (auto [nr, nc] : neighbors(r, c, rows, cols)) {
        if (grid[nr][nc] == nextHeight) {
            totalPaths += countPaths(nr, nc, grid, dp, rows, cols);
        }
    }

    dp[r][c] = totalPaths;
    return totalPaths;
}

int calculateTotalRating(const vector<vector<int>>& grid) {
    int rows = grid.size();
    int cols = rows > 0 ? grid[0].size() : 0;

    vector<pair<int, int>> trailheads;
    for (int r = 0; r < rows; ++r) {
        for (int c = 0; c < cols; ++c) {
            if (grid[r][c] == 0) {
                trailheads.emplace_back(r, c);
            }
        }
    }

    vector<vector<int>> dp(rows, vector<int>(cols, -1));

    int totalRating = 0;
    for (auto [tr, tc] : trailheads) {
        totalRating += countPaths(tr, tc, grid, dp, rows, cols);
    }

    return totalRating;
}

int main() {
    auto startTimePart1 = chrono::high_resolution_clock::now();
    auto grid = readMap(INPUT_FILE);
    int totalScore = findTrailheadScores(grid);
    auto endTimePart1 = chrono::high_resolution_clock::now();

    cout << "Part 1 Result: " << totalScore << endl;
    cout << "Time taken for Part 1: "
         << chrono::duration<double>(endTimePart1 - startTimePart1).count()
         << " s" << endl;

    auto startTimePart2 = chrono::high_resolution_clock::now();
    int totalRating = calculateTotalRating(grid);
    auto endTimePart2 = chrono::high_resolution_clock::now();

    cout << "Part 2 Result: " << totalRating << endl;
    cout << "Time taken for Part 2: "
         << chrono::duration<double>(endTimePart2 - startTimePart2).count()
         << " s" << endl;

    return 0;
}
