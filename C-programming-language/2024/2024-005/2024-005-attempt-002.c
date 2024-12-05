#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

// Struct for rules
typedef struct {
    int x;
    int y;
} Rule;

// Function prototypes
bool is_update_ordered(int *update, int update_size, Rule *rules, int rule_count);
void topological_sort_update(int *update, int update_size, Rule *rules, int rule_count, int *sorted_update);

// Utility function to parse a line of integers separated by delimiters
void parse_line(const char *line, const char *delim, int **result, int *size) {
    char *line_copy = strdup(line);
    char *token = strtok(line_copy, delim);
    int capacity = 10;
    *result = malloc(capacity * sizeof(int));
    *size = 0;
    while (token) {
        if (*size >= capacity) {
            capacity *= 2;
            *result = realloc(*result, capacity * sizeof(int));
        }
        (*result)[(*size)++] = atoi(token);
        token = strtok(NULL, delim);
    }
    free(line_copy);
}

int main() {
    const char *file_path = "input.txt";
    FILE *file = fopen(file_path, "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    // Read file content
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    char *content = malloc(file_size + 1);
    fread(content, 1, file_size, file);
    content[file_size] = '\0';
    fclose(file);

    // Split content into rules and updates sections
    char *sections[2];
    char *split = strstr(content, "\n\n");
    if (split) {
        *split = '\0';
        sections[0] = content;
        sections[1] = split + 2;
    } else {
        fprintf(stderr, "Error: Invalid file format\n");
        free(content);
        return 1;
    }

    // Parse rules
    char *rule_line = strtok(sections[0], "\n");
    Rule *rules = malloc(10 * sizeof(Rule));
    int rule_count = 0, rule_capacity = 10;

    while (rule_line) {
        if (rule_count >= rule_capacity) {
            rule_capacity *= 2;
            rules = realloc(rules, rule_capacity * sizeof(Rule));
        }
        int *rule_parts;
        int rule_size;
        parse_line(rule_line, "|", &rule_parts, &rule_size);
        if (rule_size == 2) {
            rules[rule_count].x = rule_parts[0];
            rules[rule_count].y = rule_parts[1];
            rule_count++;
        }
        free(rule_parts);
        rule_line = strtok(NULL, "\n");
    }

    // Parse updates
    char *update_line = strtok(sections[1], "\n");
    int **updates = malloc(10 * sizeof(int *));
    int *update_sizes = malloc(10 * sizeof(int));
    int update_count = 0, update_capacity = 10;

    while (update_line) {
        if (update_count >= update_capacity) {
            update_capacity *= 2;
            updates = realloc(updates, update_capacity * sizeof(int *));
            update_sizes = realloc(update_sizes, update_capacity * sizeof(int));
        }
        parse_line(update_line, ",", &updates[update_count], &update_sizes[update_count]);
        update_count++;
        update_line = strtok(NULL, "\n");
    }

    // Identify correctly ordered updates and their middle pages
    int sum_middle_pages = 0;
    for (int i = 0; i < update_count; i++) {
        if (is_update_ordered(updates[i], update_sizes[i], rules, rule_count)) {
            sum_middle_pages += updates[i][update_sizes[i] / 2];
        }
    }

    printf("Sum of middle pages of correctly ordered updates: %d\n", sum_middle_pages);

    // Correct incorrectly ordered updates
    int sum_incorrect_middle_pages = 0;
    for (int i = 0; i < update_count; i++) {
        if (!is_update_ordered(updates[i], update_sizes[i], rules, rule_count)) {
            int *corrected_update = malloc(update_sizes[i] * sizeof(int));
            topological_sort_update(updates[i], update_sizes[i], rules, rule_count, corrected_update);
            sum_incorrect_middle_pages += corrected_update[update_sizes[i] / 2];
            free(corrected_update);
        }
    }

    printf("Sum of middle pages of corrected updates: %d\n", sum_incorrect_middle_pages);

    // Free allocated memory
    free(content);
    free(rules);
    for (int i = 0; i < update_count; i++) {
        free(updates[i]);
    }
    free(updates);
    free(update_sizes);

    return 0;
}

// Check if an update respects the rules
bool is_update_ordered(int *update, int update_size, Rule *rules, int rule_count) {
    int *index_map = calloc(1001, sizeof(int));  // Assuming page numbers <= 1000
    for (int i = 0; i < update_size; i++) {
        index_map[update[i]] = i + 1;  // Store 1-based index
    }

    for (int i = 0; i < rule_count; i++) {
        int x = rules[i].x, y = rules[i].y;
        if (index_map[x] && index_map[y] && index_map[x] > index_map[y]) {
            free(index_map);
            return false;
        }
    }

    free(index_map);
    return true;
}

// Topological sort to correct update order
void topological_sort_update(int *update, int update_size, Rule *rules, int rule_count, int *sorted_update) {
    int *in_degree = calloc(1001, sizeof(int));
    int **adj_list = malloc(1001 * sizeof(int *));
    int *adj_list_sizes = calloc(1001, sizeof(int));
    for (int i = 0; i < 1001; i++) {
        adj_list[i] = malloc(10 * sizeof(int));
    }

    // Build adjacency list and in-degree
    for (int i = 0; i < rule_count; i++) {
        int x = rules[i].x, y = rules[i].y;
        adj_list[x][adj_list_sizes[x]++] = y;
        in_degree[y]++;
    }

    // Topological sort using queue
    int *queue = malloc(update_size * sizeof(int));
    int front = 0, rear = 0;
    for (int i = 0; i < update_size; i++) {
        if (in_degree[update[i]] == 0) {
            queue[rear++] = update[i];
        }
    }

    int index = 0;
    while (front < rear) {
        int current = queue[front++];
        sorted_update[index++] = current;
        for (int i = 0; i < adj_list_sizes[current]; i++) {
            int neighbor = adj_list[current][i];
            in_degree[neighbor]--;
            if (in_degree[neighbor] == 0) {
                queue[rear++] = neighbor;
            }
        }
    }

    // Free memory
    free(queue);
    free(in_degree);
    for (int i = 0; i < 1001; i++) {
        free(adj_list[i]);
    }
    free(adj_list);
    free(adj_list_sizes);
}
