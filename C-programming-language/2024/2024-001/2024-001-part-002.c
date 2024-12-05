#include <stdio.h>
#include <stdlib.h>

#define MAX_SIZE 1000
#define MAX_VALUE 1000000  // Max value for location IDs, adjust as necessary

// Function to calculate the total similarity score
void calculate_similarity_score(const char *file_path) {
    FILE *file = fopen(file_path, "r");
    if (!file) {
        printf("Could not open file %s\n", file_path);
        return;
    }

    int left_list[MAX_SIZE];
    int right_list[MAX_SIZE];
    int right_list_counts[MAX_VALUE] = {0};  // Initialize an array to count occurrences of each number
    int i = 0;

    // Read the file and load the location IDs into two arrays
    while (fscanf(file, "%d %d", &left_list[i], &right_list[i]) == 2) {
        // Increment the count for the number in the right_list
        right_list_counts[right_list[i]]++;
        i++;
    }
    fclose(file);

    int size = i;
    int similarity_score = 0;

    // Calculate the similarity score by multiplying each number from the left list
    // with the number of times it appears in the right list
    for (int j = 0; j < size; j++) {
        similarity_score += left_list[j] * right_list_counts[left_list[j]];
    }

    // Output the similarity score
    printf("Total Similarity Score: %d\n", similarity_score);
}

int main() {
    // File path for input data
    const char *file_path = "/mnt/z/advent-of-code-002/c-programming-language/2024/2024-001/input.txt";

    // Call the function to calculate the similarity score
    calculate_similarity_score(file_path);

    return 0;
}
