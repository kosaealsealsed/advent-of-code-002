#include <iostream>
#include <fstream>
#include <regex>
#include <string>

using namespace std;

// Function to sum all mul(X, Y) operations
int sumMulOperations(const string& corruptedMemory) {
    regex pattern(R"(mul\((\d{1,3}),(\d{1,3})\))");
    smatch match;
    int total = 0;
    string::const_iterator searchStart(corruptedMemory.cbegin());

    // Find all matches and calculate the sum of products
    while (regex_search(searchStart, corruptedMemory.cend(), match, pattern)) {
        int num1 = stoi(match[1]);
        int num2 = stoi(match[2]);
        total += num1 * num2;
        searchStart = match.suffix().first;
    }

    return total;
}

// Function to sum only enabled mul(X, Y) operations
int sumEnabledMulOperations(const string& corruptedMemory) {
    regex pattern(R"((do\(\)|don't\(\)|mul\((\d{1,3}),(\d{1,3})\)))");
    smatch match;
    int totalSum = 0;
    bool mulEnabled = true; // mul instructions are enabled at the start
    string::const_iterator searchStart(corruptedMemory.cbegin());

    // Find all matches
    while (regex_search(searchStart, corruptedMemory.cend(), match, pattern)) {
        string fullMatch = match[1];

        if (fullMatch == "do()") {
            mulEnabled = true;
        } else if (fullMatch == "don't()") {
            mulEnabled = false;
        } else if (match[2].matched && match[3].matched) {
            // This is a mul(X,Y) instruction
            if (mulEnabled) {
                int num1 = stoi(match[2]);
                int num2 = stoi(match[3]);
                totalSum += num1 * num2;
            }
        }

        searchStart = match.suffix().first;
    }

    return totalSum;
}

int main() {
    // Read the corrupted memory from 'input.txt'
    ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        cerr << "Error: Unable to open file input.txt" << endl;
        return 1;
    }

    string corruptedMemory((istreambuf_iterator<char>(inputFile)),
                           istreambuf_iterator<char>());
    inputFile.close();

    // Calculate the results
    int totalSumAllMulOperations = sumMulOperations(corruptedMemory);
    int totalSumEnabledMulOperations = sumEnabledMulOperations(corruptedMemory);

    // Output the results
    cout << "The sum of all mul operations is: " << totalSumAllMulOperations << endl;
    cout << "The sum of enabled mul operations is: " << totalSumEnabledMulOperations << endl;

    return 0;
}
