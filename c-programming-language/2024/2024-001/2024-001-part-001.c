#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define MAX_SIZE 1000

// Function to compare two integers (used for sorting)
int compare(const void *a, const void *b) {
    return (*(int *)a - *(int *)b);
}

// Function to calculate the total distance between two sorted lists
void calculate_total_distance(const char *file_path) {
    FILE *file = fopen(file_path, "r");
    if (!file) {
        printf("Could not open file %s\n", file_path);
        return;
    }

    int left_list[MAX_SIZE];
    int right_list[MAX_SIZE];
    int i = 0;

    // Read the file and load the location IDs into two arrays
    while (fscanf(file, "%d %d", &left_list[i], &right_list[i]) == 2) {
        i++;
    }
    fclose(file);

    int size = i;

    // Sort both lists
    qsort(left_list, size, sizeof(int), compare);
    qsort(right_list, size, sizeof(int), compare);

    // Calculate the total distance
    int total_distance = 0;
    for (int j = 0; j < size; j++) {
        total_distance += abs(left_list[j] - right_list[j]);
    }

    // Output the total distance
    printf("Total Distance: %d\n", total_distance);
}

int main() {
    // File path for input data
    const char *file_path = "/mnt/z/advent-of-code-002/c-programming-language/2024/2024-001/input.txt";

    // Call the function to calculate the total distance
    calculate_total_distance(file_path);

    return 0;
}
