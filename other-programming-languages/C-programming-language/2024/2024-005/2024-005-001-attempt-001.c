#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

// Define the maximum sizes for input
#define MAX_RULES 100
#define MAX_UPDATES 100
#define MAX_UPDATE_LENGTH 100
#define MAX_LINE_LENGTH 256

typedef struct {
    int x, y;
} Rule;

typedef struct {
    int pages[MAX_UPDATE_LENGTH];
    int length;
} Update;

// Helper function to parse an integer
int parse_int(const char *str) {
    return atoi(str);
}

// Function to check if an update follows the rules
bool is_update_ordered(Update update, Rule *rules, int rule_count) {
    int index_map[MAX_UPDATE_LENGTH] = {-1}; // Map to store positions of pages

    // Populate the index map with positions of each page
    for (int i = 0; i < update.length; i++) {
        index_map[update.pages[i]] = i;
    }

    // Check each rule
    for (int i = 0; i < rule_count; i++) {
        int x = rules[i].x;
        int y = rules[i].y;

        if (index_map[x] != -1 && index_map[y] != -1) {
            int x_index = index_map[x];
            int y_index = index_map[y];

            if (x_index > y_index) {
                return false; // Rule violated
            }
        }
    }

    return true;
}

int main() {
    char file_path[] = "input.txt";
    FILE *file = fopen(file_path, "r");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    char buffer[MAX_LINE_LENGTH];
    Rule rules[MAX_RULES];
    Update updates[MAX_UPDATES];
    int rule_count = 0, update_count = 0;

    // Step 2: Reading file content into rules and updates
    // Read rules section
    while (fgets(buffer, sizeof(buffer), file)) {
        if (buffer[0] == '\n') break; // Blank line marks end of rules section
        char *token = strtok(buffer, "|");
        if (token) {
            rules[rule_count].x = parse_int(token);
            token = strtok(NULL, "|");
            if (token) {
                rules[rule_count].y = parse_int(token);
                rule_count++;
            }
        }
    }

    // Read updates section
    while (fgets(buffer, sizeof(buffer), file)) {
        char *token = strtok(buffer, ",");
        Update update = {.length = 0};
        while (token) {
            update.pages[update.length++] = parse_int(token);
            token = strtok(NULL, ",");
        }
        updates[update_count++] = update;
    }

    fclose(file);

    // Step 6: Identify correctly ordered updates
    int sum_middle_pages = 0;

    for (int i = 0; i < update_count; i++) {
        if (is_update_ordered(updates[i], rules, rule_count)) {
            int middle_index = updates[i].length / 2;
            sum_middle_pages += updates[i].pages[middle_index];
        }
    }

    // Output the result
    printf("Sum of middle pages of correctly ordered updates: %d\n", sum_middle_pages);

    return 0;
}
