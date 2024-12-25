#include <iostream>
#include <fstream>
#include <vector>
#include <unordered_map>
#include <cstdlib>  // for std::abs

// Function to calculate the total similarity score
void calculate_similarity_score(const std::string &file_path) {
    std::ifstream file(file_path);
    if (!file) {
        std::cerr << "Could not open the file: " << file_path << std::endl;
        return;
    }

    std::vector<int> left_list;
    std::vector<int> right_list;
    int left, right;

    // Read the file and load the location IDs into two vectors
    while (file >> left >> right) {
        left_list.push_back(left);
        right_list.push_back(right);
    }
    file.close();

    // Create a hash map to count occurrences of each number in the right list
    std::unordered_map<int, int> right_list_counts;
    for (int num : right_list) {
        right_list_counts[num]++;
    }

    // Calculate the similarity score
    int similarity_score = 0;
    for (int left : left_list) {
        similarity_score += left * right_list_counts[left];  // Multiply left by count in right list
    }

    // Output the similarity score
    std::cout << "Total Similarity Score: " << similarity_score << std::endl;
}

int main() {
    // File path for input data
    const std::string file_path = "input.txt";

    // Call the function to calculate the similarity score
    calculate_similarity_score(file_path);

    return 0;
}
