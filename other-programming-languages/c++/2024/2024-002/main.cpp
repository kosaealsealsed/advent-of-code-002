#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <algorithm>
#include <numeric>

using namespace std;

// Function to check if a report is safe
bool isSafe(const vector<int>& report) {
    vector<int> differences(report.size() - 1);
    for (size_t i = 0; i < report.size() - 1; ++i) {
        differences[i] = report[i + 1] - report[i];
    }

    bool allIncreasing = all_of(differences.begin(), differences.end(), [](int diff) {
        return diff >= 1 && diff <= 3;
    });

    bool allDecreasing = all_of(differences.begin(), differences.end(), [](int diff) {
        return diff <= -1 && diff >= -3;
    });

    return allIncreasing || allDecreasing;
}

// Function to check if a report is safe with the Problem Dampener
bool isSafeWithDampener(const vector<int>& report) {
    if (isSafe(report)) {
        return true;
    }

    for (size_t i = 0; i < report.size(); ++i) {
        vector<int> modifiedReport = report;
        modifiedReport.erase(modifiedReport.begin() + i);
        if (isSafe(modifiedReport)) {
            return true;
        }
    }

    return false;
}

int main() {
    // Input file path
    string inputPath = "input.txt";

    // Read the input file
    ifstream inputFile(inputPath);
    if (!inputFile) {
        cerr << "Error opening file: " << inputPath << endl;
        return 1;
    }

    vector<vector<int>> reports;
    string line;
    while (getline(inputFile, line)) {
        istringstream lineStream(line);
        vector<int> report;
        int number;
        while (lineStream >> number) {
            report.push_back(number);
        }
        reports.push_back(report);
    }

    // Count safe reports
    int safeCount = count_if(reports.begin(), reports.end(), isSafe);
    cout << "Safe reports: " << safeCount << endl;

    // Count safe reports with the Problem Dampener
    int safeWithDampenerCount = count_if(reports.begin(), reports.end(), isSafeWithDampener);
    cout << "Safe reports with dampener: " << safeWithDampenerCount << endl;

    return 0;
}
