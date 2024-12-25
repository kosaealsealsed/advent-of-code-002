#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define MAX_REPORTS 1000
#define MAX_LEVELS 100

// Function to check if a report is safe
bool is_safe(const int* report, int size) {
    bool all_increasing = true, all_decreasing = true;

    for (int i = 0; i < size - 1; i++) {
        int diff = report[i + 1] - report[i];
        if (diff < 1 || diff > 3) {
            all_increasing = false;
        }
        if (diff > -1 || diff < -3) {
            all_decreasing = false;
        }
    }

    return all_increasing || all_decreasing;
}

// Function to check if a report is safe with the Problem Dampener
bool is_safe_with_dampener(const int* report, int size) {
    if (is_safe(report, size)) {
        return true;
    }

    for (int i = 0; i < size; i++) {
        int modified_report[MAX_LEVELS];
        int k = 0;
        for (int j = 0; j < size; j++) {
            if (j != i) {
                modified_report[k++] = report[j];
            }
        }
        if (is_safe(modified_report, size - 1)) {
            return true;
        }
    }

    return false;
}

int main() {
    const char* input_path = "input.txt";
    FILE* file = fopen(input_path, "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    int reports[MAX_REPORTS][MAX_LEVELS];
    int sizes[MAX_REPORTS];
    int report_count = 0;

    char line[1024];
    while (fgets(line, sizeof(line), file)) {
        int index = 0;
        sizes[report_count] = 0;

        char* token = strtok(line, " ");
        while (token) {
            reports[report_count][index++] = atoi(token);
            sizes[report_count]++;
            token = strtok(NULL, " ");
        }
        report_count++;
    }
    fclose(file);

    // Count safe reports
    int safe_count = 0;
    for (int i = 0; i < report_count; i++) {
        if (is_safe(reports[i], sizes[i])) {
            safe_count++;
        }
    }
    printf("Safe reports: %d\n", safe_count);

    // Count safe reports with the Problem Dampener
    int safe_with_dampener_count = 0;
    for (int i = 0; i < report_count; i++) {
        if (is_safe_with_dampener(reports[i], sizes[i])) {
            safe_with_dampener_count++;
        }
    }
    printf("Safe reports with dampener: %d\n", safe_with_dampener_count);

    return 0;
}
