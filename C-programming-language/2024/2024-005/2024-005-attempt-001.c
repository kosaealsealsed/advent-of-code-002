#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_UPDATES 100
#define MAX_PAGES 1000

typedef struct {
    int x;
    int y;
} Rule;

typedef struct {
    int data[MAX_PAGES];
    int size;
} Update;

void read_file(const char *file_path, Rule **rules, int *rule_count, Update **updates, int *update_count) {
    FILE *file = fopen(file_path, "r");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }
    
    char line[256];
    *rules = malloc(MAX_UPDATES * sizeof(Rule));
    *updates = malloc(MAX_UPDATES * sizeof(Update));
    *rule_count = 0;
    *update_count = 0;
    
    // Read rules
    while (fgets(line, sizeof(line), file)) {
        if (strcmp(line, "\n") == 0) break;
        int x, y;
        sscanf(line, "%d|%d", &x, &y);
        (*rules)[(*rule_count)++] = (Rule){x, y};
    }
    
    // Read updates
    while (fgets(line, sizeof(line), file)) {
        (*updates)[*update_count].size = 0;
        char *token = strtok(line, ",");
        while (token) {
            (*updates)[*update_count].data[(*updates)[*update_count].size++] = atoi(token);
            token = strtok(NULL, ",");
        }
        (*update_count)++;
    }
    
    fclose(file);
}

bool is_update_ordered(const Update *update, Rule *rules, int rule_count) {
    int index_map[MAX_PAGES];
    for (int i = 0; i < MAX_PAGES; i++) index_map[i] = -1;
    
    for (int i = 0; i < update->size; i++) {
        index_map[update->data[i]] = i;
    }
    
    for (int i = 0; i < rule_count; i++) {
        int x = rules[i].x, y = rules[i].y;
        if (index_map[x] != -1 && index_map[y] != -1 && index_map[x] > index_map[y]) {
            return false;
        }
    }
    return true;
}

int find_middle_page(const Update *update) {
    return update->data[update->size / 2];
}

int calculate_middle_pages_sum(Update *updates, int update_count, Rule *rules, int rule_count) {
    int sum = 0;
    for (int i = 0; i < update_count; i++) {
        if (is_update_ordered(&updates[i], rules, rule_count)) {
            sum += find_middle_page(&updates[i]);
        }
    }
    return sum;
}

void topological_sort_update(const Update *update, Rule *rules, int rule_count, Update *sorted_update) {
    int in_degree[MAX_PAGES] = {0};
    int graph[MAX_PAGES][MAX_PAGES] = {0};
    bool visited[MAX_PAGES] = {false};
    int queue[MAX_PAGES];
    int queue_start = 0, queue_end = 0;
    
    for (int i = 0; i < rule_count; i++) {
        graph[rules[i].x][rules[i].y] = 1;
        in_degree[rules[i].y]++;
    }
    
    for (int i = 0; i < update->size; i++) {
        int node = update->data[i];
        if (in_degree[node] == 0) {
            queue[queue_end++] = node;
        }
    }
    
    sorted_update->size = 0;
    while (queue_start < queue_end) {
        int current = queue[queue_start++];
        sorted_update->data[sorted_update->size++] = current;
        for (int i = 0; i < MAX_PAGES; i++) {
            if (graph[current][i]) {
                in_degree[i]--;
                if (in_degree[i] == 0) {
                    queue[queue_end++] = i;
                }
            }
        }
    }
}

int process_updates(Update *updates, int update_count, Rule *rules, int rule_count) {
    int sum = 0;
    Update corrected_update;
    for (int i = 0; i < update_count; i++) {
        if (!is_update_ordered(&updates[i], rules, rule_count)) {
            topological_sort_update(&updates[i], rules, rule_count, &corrected_update);
            sum += find_middle_page(&corrected_update);
        }
    }
    return sum;
}

int main() {
    const char *file_path = "input.txt";
    Rule *rules;
    Update *updates;
    int rule_count, update_count;

    read_file(file_path, &rules, &rule_count, &updates, &update_count);
    
    int sum_correct_middle_pages = calculate_middle_pages_sum(updates, update_count, rules, rule_count);
    printf("Sum of middle pages for correctly ordered updates: %d\n", sum_correct_middle_pages);
    
    int sum_incorrect_middle_pages = process_updates(updates, update_count, rules, rule_count);
    printf("Sum of middle pages for corrected updates: %d\n", sum_incorrect_middle_pages);

    free(rules);
    free(updates);
    return 0;
}
