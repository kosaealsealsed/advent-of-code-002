#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <set>
#include <map>
#include <array>
#include <algorithm>
#include <chrono>
#include <numeric>
#include <sstream>

using namespace std;
using namespace std::chrono;

/**
 * Reads the grid map from a file.
 *
 * @param filename The name of the file containing the grid map.
 * @return A vector of strings representing the grid.
 */
vector<string> readMap(const string& filename) {
    vector<string> grid;
    ifstream file(filename);
    string line;
    if (!file.is_open()) {
        throw runtime_error("Failed to open file: " + filename);
    }
    while (getline(file, line)) {
        grid.push_back(line);
    }
    return grid;
}

/**
 * Computes unique antinode positions using the pairwise method.
 *
 * @param grid A vector of strings representing the grid map.
 * @return A set of unique antinode positions.
 */
set<array<int, 2>> computeAntinodesPairwise(const vector<string>& grid) {
    size_t rows = grid.size();
    size_t cols = rows > 0 ? grid[0].size() : 0;

    map<char, vector<array<int, 2>>> antennasByFreq;

    for (size_t r = 0; r < rows; ++r) {
        for (size_t c = 0; c < cols; ++c) {
            char ch = grid[r][c];
            if (ch != '.') {
                antennasByFreq[ch].push_back({static_cast<int>(r), static_cast<int>(c)});
            }
        }
    }

    set<array<int, 2>> antinodes;

    for (const auto& [freq, coords] : antennasByFreq) {
        size_t n = coords.size();
        if (n < 2) continue;

        for (size_t i = 0; i < n; ++i) {
            auto [rA, cA] = coords[i];
            for (size_t j = i + 1; j < n; ++j) {
                auto [rB, cB] = coords[j];

                // Compute P1 = 2B - A
                int p1_r = 2 * rB - rA;
                int p1_c = 2 * cB - cA;
                if (p1_r >= 0 && p1_r < static_cast<int>(rows) && p1_c >= 0 && p1_c < static_cast<int>(cols)) {
                    antinodes.insert({p1_r, p1_c});
                }

                // Compute P2 = 2A - B
                int p2_r = 2 * rA - rB;
                int p2_c = 2 * cA - cB;
                if (p2_r >= 0 && p2_r < static_cast<int>(rows) && p2_c >= 0 && p2_c < static_cast<int>(cols)) {
                    antinodes.insert({p2_r, p2_c});
                }
            }
        }
    }
    return antinodes;
}

/**
 * Computes the greatest common divisor of two integers.
 *
 * @param a First integer.
 * @param b Second integer.
 * @return The greatest common divisor of a and b.
 */
int gcd(int a, int b) {
    return b == 0 ? a : gcd(b, a % b);
}

/**
 * Adds all points along a line between two antennas to the antinodes set.
 *
 * @param rA Row of the first antenna.
 * @param cA Column of the first antenna.
 * @param rB Row of the second antenna.
 * @param cB Column of the second antenna.
 * @param rows Number of rows in the grid.
 * @param cols Number of columns in the grid.
 * @param antinodes Set to store unique antinode positions.
 */
void addLinePoints(int rA, int cA, int rB, int cB, int rows, int cols, set<array<int, 2>>& antinodes) {
    int dr = rB - rA, dc = cB - cA;
    int g = gcd(abs(dr), abs(dc));
    dr /= g;
    dc /= g;

    // Add points in the forward direction
    int rP = rA, cP = cA;
    while (rP >= 0 && rP < rows && cP >= 0 && cP < cols) {
        antinodes.insert({rP, cP});
        rP += dr;
        cP += dc;
    }

    // Add points in the backward direction
    rP = rA - dr;
    cP = cA - dc;
    while (rP >= 0 && rP < rows && cP >= 0 && cP < cols) {
        antinodes.insert({rP, cP});
        rP -= dr;
        cP -= dc;
    }
}

/**
 * Computes unique antinode positions using the line-drawing method.
 *
 * @param grid A vector of strings representing the grid map.
 * @return A set of unique antinode positions.
 */
set<array<int, 2>> computeAntinodesLines(const vector<string>& grid) {
    size_t rows = grid.size();
    size_t cols = rows > 0 ? grid[0].size() : 0;

    map<char, vector<array<int, 2>>> antennasByFreq;

    for (size_t r = 0; r < rows; ++r) {
        for (size_t c = 0; c < cols; ++c) {
            char ch = grid[r][c];
            if (ch != '.') {
                antennasByFreq[ch].push_back({static_cast<int>(r), static_cast<int>(c)});
            }
        }
    }

    set<array<int, 2>> antinodes;

    for (const auto& [freq, coords] : antennasByFreq) {
        size_t n = coords.size();
        if (n < 2) continue;

        for (size_t i = 0; i < n; ++i) {
            auto [rA, cA] = coords[i];
            for (size_t j = i + 1; j < n; ++j) {
                auto [rB, cB] = coords[j];
                addLinePoints(rA, cA, rB, cB, rows, cols, antinodes);
            }
        }
    }
    return antinodes;
}

/**
 * Formats the elapsed time in seconds with nanoseconds as a human-readable string.
 *
 * @param nanos The elapsed time in nanoseconds.
 * @return The formatted time as a string.
 */
string formatTime(long long nanos) {
    stringstream ss;
    ss << fixed << setprecision(9) << nanos / 1e9 << " s";
    return ss.str();
}

int main() {
    try {
        auto grid = readMap("input.txt");

        auto overallStart = high_resolution_clock::now();

        // Part 1: Compute using Pairwise method
        auto startTime = high_resolution_clock::now();
        auto pairwiseAntinodes = computeAntinodesPairwise(grid);
        auto endTime = high_resolution_clock::now();
        auto part1Time = duration_cast<nanoseconds>(endTime - startTime).count();

        cout << "Part 1 finished in " << formatTime(part1Time) << "\n";
        cout << "Number of unique antinodes (Pairwise method): " << pairwiseAntinodes.size() << "\n";

        // Part 2: Compute using Line-drawing method
        startTime = high_resolution_clock::now();
        auto lineAntinodes = computeAntinodesLines(grid);
        endTime = high_resolution_clock::now();
        auto part2Time = duration_cast<nanoseconds>(endTime - startTime).count();

        cout << "Part 2 finished in " << formatTime(part2Time) << "\n";
        cout << "Number of unique antinodes (Line-drawing method): " << lineAntinodes.size() << "\n";

        auto overallEnd = high_resolution_clock::now();
        auto overallTime = duration_cast<nanoseconds>(overallEnd - overallStart).count();

        cout << "Overall total time: " << formatTime(overallTime) << "\n";

    } catch (const exception& e) {
        cerr << "Error: " << e.what() << "\n";
    }

    return 0;
}
