#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Use `size_t` for large constants
#define MAX_LINE_LENGTH ((size_t)100000)
#define MAX_RULES ((size_t)100000)
#define MAX_UPDATES ((size_t)100000)

typedef struct {
    char first[MAX_LINE_LENGTH];
    char second[MAX_LINE_LENGTH];
} Rule;

int main() {
    // Allocate memory dynamically for large buffers
    size_t rules_section_size = MAX_LINE_LENGTH * MAX_RULES;
    size_t updates_section_size = MAX_LINE_LENGTH * MAX_UPDATES;
    size_t content_size = MAX_LINE_LENGTH * (MAX_RULES + MAX_UPDATES);

    char *rules_section = malloc(rules_section_size);
    char *updates_section = malloc(updates_section_size);
    char *content = malloc(content_size);

    if (!rules_section || !updates_section || !content) {
        fprintf(stderr, "Error: Memory allocation failed.\n");
        free(rules_section);
        free(updates_section);
        free(content);
        return EXIT_FAILURE;
    }

    // Step 1: Reading the file content
    const char *file_path = "input.txt";  // File containing the rules and updates
    FILE *file = fopen(file_path, "r");
    if (!file) {
        perror("Error opening file");
        free(rules_section);
        free(updates_section);
        free(content);
        return EXIT_FAILURE;
    }

    size_t content_length = fread(content, 1, content_size - 1, file);
    fclose(file);
    content[content_length] = '\0';

    // Step 2: Splitting the content into rules and updates sections
    char *split_ptr = strstr(content, "\n\n");  // Find the blank line
    if (!split_ptr) {
        fprintf(stderr, "Error: File format invalid (missing blank line).\n");
        free(rules_section);
        free(updates_section);
        free(content);
        return EXIT_FAILURE;
    }

    *split_ptr = '\0';  // Split the string into two parts
    strncpy(rules_section, content, rules_section_size - 1);
    rules_section[rules_section_size - 1] = '\0';
    strncpy(updates_section, split_ptr + 2, updates_section_size - 1);
    updates_section[updates_section_size - 1] = '\0';

    // Step 3: Parsing the rules section into a list of tuples
    Rule rules[MAX_RULES];
    size_t rule_count = 0;

    char *line = strtok(rules_section, "\n");  // Split rules section by line
    while (line && rule_count < MAX_RULES) {
        char first[MAX_LINE_LENGTH], second[MAX_LINE_LENGTH];

        // Parse rule using pipe (|) as delimiter
        char *pipe_pos = strchr(line, '|');
        if (pipe_pos) {
            *pipe_pos = '\0';  // Replace '|' with null terminator
            strncpy(first, line, MAX_LINE_LENGTH - 1);
            strncpy(second, pipe_pos + 1, MAX_LINE_LENGTH - 1);
            first[MAX_LINE_LENGTH - 1] = '\0';
            second[MAX_LINE_LENGTH - 1] = '\0';

            // Store the rule in the rules array
            strncpy(rules[rule_count].first, first, MAX_LINE_LENGTH - 1);
            strncpy(rules[rule_count].second, second, MAX_LINE_LENGTH - 1);
            rules[rule_count].first[MAX_LINE_LENGTH - 1] = '\0';
            rules[rule_count].second[MAX_LINE_LENGTH - 1] = '\0';

            rule_count++;
        } else {
            fprintf(stderr, "Warning: Skipping invalid rule line: %s\n", line);
        }

        line = strtok(NULL, "\n");  // Move to the next line
    }

    // Output the parsed rules for verification
    printf("Parsed Rules:\n");
    for (size_t i = 0; i < rule_count; i++) {
        printf("Rule %zu: %s -> %s\n", i + 1, rules[i].first, rules[i].second);
    }

    // Step 4: Parse the updates section
    char *updates[MAX_UPDATES];
    size_t update_count = 0;

    line = strtok(updates_section, "\n");  // Split updates section by line
    while (line && update_count < MAX_UPDATES) {
        updates[update_count] = strdup(line);  // Store each update line
        update_count++;
        line = strtok(NULL, "\n");  // Move to the next line
    }

    // Output parsed updates for verification
    printf("Parsed Updates:\n");
    for (size_t i = 0; i < update_count; i++) {
        printf("Update %zu: %s\n", i + 1, updates[i]);
        free(updates[i]);  // Free dynamically allocated memory
    }

    // Free dynamically allocated memory
    free(rules_section);
    free(updates_section);
    free(content);

    return 0;
}
