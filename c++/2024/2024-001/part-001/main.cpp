#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <cstdlib>  // for std::abs

// Function to calculate the total distance between two sorted lists
void calculate_total_distance(const std::string &file_path) {
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

    // Sort both lists
    std::sort(left_list.begin(), left_list.end());
    std::sort(right_list.begin(), right_list.end());

    // Calculate the total distance
    int total_distance = 0;
    for (size_t i = 0; i < left_list.size(); ++i) {
        total_distance += std::abs(left_list[i] - right_list[i]);
    }

    // Output the total distance
    std::cout << "Total Distance: " << total_distance << std::endl;
}

int main() {
    // File path for input data
    const std::string file_path = "input.txt";

    // Call the function to calculate the total distance
    calculate_total_distance(file_path);

    return 0;
}
