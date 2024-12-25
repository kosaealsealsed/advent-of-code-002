#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <algorithm>
#include <stdexcept>
#include <chrono>
#include <iomanip>

using namespace std;

struct TestCase {
    long long target;
    vector<long long> numbers;

    TestCase(long long target, vector<long long> numbers) : target(target), numbers(numbers) {}
};

long long evaluateLeftToRight(const vector<long long>& numbers, const vector<string>& ops) {
    long long result = numbers[0];
    for (size_t i = 0; i < ops.size(); ++i) {
        const string& op = ops[i];
        long long nextNumber = numbers[i + 1];
        if (op == "+") {
            result += nextNumber;
        } else if (op == "*") {
            result *= nextNumber;
        } else {
            throw invalid_argument("Unsupported operator: " + op);
        }
    }
    return result;
}

long long evaluateWithConcat(const vector<long long>& numbers, const vector<string>& ops) {
    long long result = numbers[0];
    for (size_t i = 0; i < ops.size(); ++i) {
        const string& op = ops[i];
        long long nextNumber = numbers[i + 1];
        if (op == "+") {
            result += nextNumber;
        } else if (op == "*") {
            result *= nextNumber;
        } else if (op == "||") {
            result = stoll(to_string(result) + to_string(nextNumber));
        } else {
            throw invalid_argument("Unsupported operator: " + op);
        }
    }
    return result;
}

vector<TestCase> parseInput(const string& filePath) {
    vector<TestCase> testCases;
    ifstream file(filePath);
    if (!file.is_open()) {
        cerr << "Error opening file: " << filePath << endl;
        return testCases;
    }

    string line;
    while (getline(file, line)) {
        if (line.find(":") == string::npos) continue;

        size_t colonPos = line.find(":");
        string targetStr = line.substr(0, colonPos);
        string numbersStr = line.substr(colonPos + 1);

        long long target = stoll(targetStr);
        vector<long long> numbers;
        stringstream ss(numbersStr);
        long long num;
        while (ss >> num) {
            numbers.push_back(num);
        }

        testCases.emplace_back(target, numbers);
    }
    return testCases;
}

vector<vector<string>> generateOperatorCombinations(const vector<string>& operators, int length) {
    if (length == 0) return {{}};
    auto subCombinations = generateOperatorCombinations(operators, length - 1);
    vector<vector<string>> combinations;

    for (const auto& sub : subCombinations) {
        for (const string& op : operators) {
            auto newCombination = sub;
            newCombination.push_back(op);
            combinations.push_back(newCombination);
        }
    }
    return combinations;
}

long long solvePartOne(const vector<TestCase>& testCases) {
    vector<string> operators = {"+", "*"};
    long long validTestValuesSum = 0;

    for (const auto& testCase : testCases) {
        bool possible = false;
        int opsLength = testCase.numbers.size() - 1;
        auto allOps = generateOperatorCombinations(operators, opsLength);

        for (const auto& ops : allOps) {
            if (evaluateLeftToRight(testCase.numbers, ops) == testCase.target) {
                possible = true;
                break;
            }
        }
        if (possible) {
            validTestValuesSum += testCase.target;
        }
    }
    return validTestValuesSum;
}

pair<long long, double> solvePartTwo(const vector<TestCase>& testCases, double partOneTime) {
    vector<string> operators = {"+", "*", "||"};
    long long validTestValuesSum = 0;

    int totalCases = testCases.size();
    int progressInterval = max(1, totalCases / 10);

    double cumulative1 = 0.0;
    double cumulative2 = partOneTime;

    cout << "\nProgress    Interval(s)      Cumulative1(s)    Cumulative2(s)\n";
    cout << "-------------------------------------------------------------\n";

    auto start = chrono::high_resolution_clock::now();

    for (int index = 0; index < totalCases; ++index) {
        const auto& testCase = testCases[index];
        bool possible = false;
        int opsLength = testCase.numbers.size() - 1;
        auto allOps = generateOperatorCombinations(operators, opsLength);

        for (const auto& ops : allOps) {
            if (evaluateWithConcat(testCase.numbers, ops) == testCase.target) {
                possible = true;
                break;
            }
        }
        if (possible) {
            validTestValuesSum += testCase.target;
        }

        if ((index + 1) % progressInterval == 0 || (index + 1) == totalCases) {
            auto now = chrono::high_resolution_clock::now();
            double intervalElapsed = chrono::duration<double>(now - start).count();
            cumulative1 += intervalElapsed;
            cumulative2 = partOneTime + cumulative1;

            cout << setw(10) << (index + 1) << "/" << totalCases
                 << setw(20) << fixed << setprecision(9) << intervalElapsed
                 << setw(20) << cumulative1
                 << setw(20) << cumulative2 << endl;

            start = chrono::high_resolution_clock::now();
        }
    }
    return {validTestValuesSum, cumulative2};
}

int main() {
    string filePath = "input.txt"; // Change this to your actual input file location

    auto testCases = parseInput(filePath);

    cout << "Starting Part 1...\n";
    auto partOneStart = chrono::high_resolution_clock::now();
    long long partOneResult = solvePartOne(testCases);
    double partOneTime = chrono::duration<double>(chrono::high_resolution_clock::now() - partOneStart).count();
    cout << "Part 1 finished in " << fixed << setprecision(9) << partOneTime << " seconds\n";
    cout << "Part 1 Total Calibration Result: " << partOneResult << "\n";

    cout << "Starting Part 2...\n";
    auto [partTwoResult, cumulativeTime] = solvePartTwo(testCases, partOneTime);
    double partTwoTime = cumulativeTime - partOneTime;
    cout << "\nPart 2 finished in " << fixed << setprecision(9) << partTwoTime
         << " seconds, cumulative time " << cumulativeTime << " seconds\n";
    cout << "Part 2 Total Calibration Result: " << partTwoResult << "\n";

    return 0;
}
