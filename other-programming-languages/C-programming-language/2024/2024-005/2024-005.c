#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

// Maximum line length
#define MAX_LINE_LENGTH 1024

// Structure to hold a rule (x, y)
typedef struct {
    int x;
    int y;
} Rule;

// Dynamic array for Rules
typedef struct {
    Rule *data;
    size_t size;
    size_t capacity;
} RuleArray;

// Initialize RuleArray
void initRuleArray(RuleArray *arr) {
    arr->size = 0;
    arr->capacity = 10;
    arr->data = (Rule *)malloc(arr->capacity * sizeof(Rule));
    if (!arr->data) {
        fprintf(stderr, "Memory allocation failed for RuleArray.\n");
        exit(EXIT_FAILURE);
    }
}

// Append to RuleArray
void appendRuleArray(RuleArray *arr, Rule rule) {
    if (arr->size == arr->capacity) {
        arr->capacity *= 2;
        arr->data = (Rule *)realloc(arr->data, arr->capacity * sizeof(Rule));
        if (!arr->data) {
            fprintf(stderr, "Memory reallocation failed for RuleArray.\n");
            exit(EXIT_FAILURE);
        }
    }
    arr->data[arr->size++] = rule;
}

// Free RuleArray
void freeRuleArray(RuleArray *arr) {
    free(arr->data);
    arr->data = NULL;
    arr->size = arr->capacity = 0;
}

// Structure to hold an update (array of pages)
typedef struct {
    int *pages;
    size_t size;
    size_t capacity;
} Update;

// Dynamic array for Updates
typedef struct {
    Update *data;
    size_t size;
    size_t capacity;
} UpdateArray;

// Initialize UpdateArray
void initUpdateArray(UpdateArray *arr) {
    arr->size = 0;
    arr->capacity = 10;
    arr->data = (Update *)malloc(arr->capacity * sizeof(Update));
    if (!arr->data) {
        fprintf(stderr, "Memory allocation failed for UpdateArray.\n");
        exit(EXIT_FAILURE);
    }
}

// Initialize an individual Update
void initUpdate(Update *update) {
    update->size = 0;
    update->capacity = 10;
    update->pages = (int *)malloc(update->capacity * sizeof(int));
    if (!update->pages) {
        fprintf(stderr, "Memory allocation failed for Update pages.\n");
        exit(EXIT_FAILURE);
    }
}

// Append a page to an Update
void appendUpdate(Update *update, int page) {
    if (update->size == update->capacity) {
        update->capacity *= 2;
        update->pages = (int *)realloc(update->pages, update->capacity * sizeof(int));
        if (!update->pages) {
            fprintf(stderr, "Memory reallocation failed for Update pages.\n");
            exit(EXIT_FAILURE);
        }
    }
    update->pages[update->size++] = page;
}

// Append an Update to UpdateArray
void appendUpdateArray(UpdateArray *arr, Update update) {
    if (arr->size == arr->capacity) {
        arr->capacity *= 2;
        arr->data = (Update *)realloc(arr->data, arr->capacity * sizeof(Update));
        if (!arr->data) {
            fprintf(stderr, "Memory reallocation failed for UpdateArray.\n");
            exit(EXIT_FAILURE);
        }
    }
    arr->data[arr->size++] = update;
}

// Free an individual Update
void freeUpdate(Update *update) {
    free(update->pages);
    update->pages = NULL;
    update->size = update->capacity = 0;
}

// Free UpdateArray
void freeUpdateArray(UpdateArray *arr) {
    for (size_t i = 0; i < arr->size; i++) {
        freeUpdate(&arr->data[i]);
    }
    free(arr->data);
    arr->data = NULL;
    arr->size = arr->capacity = 0;
}

// Function to split a string by a delimiter and return an array of tokens
char **split(const char *str, const char *delimiter, int *count) {
    char *s = strdup(str);
    if (!s) {
        fprintf(stderr, "Memory allocation failed for split.\n");
        exit(EXIT_FAILURE);
    }

    int capacity = 10;
    char **tokens = (char **)malloc(capacity * sizeof(char *));
    if (!tokens) {
        fprintf(stderr, "Memory allocation failed for tokens.\n");
        exit(EXIT_FAILURE);
    }
    *count = 0;

    char *token = strtok(s, delimiter);
    while (token != NULL) {
        if (*count == capacity) {
            capacity *= 2;
            tokens = (char **)realloc(tokens, capacity * sizeof(char *));
            if (!tokens) {
                fprintf(stderr, "Memory reallocation failed for tokens.\n");
                exit(EXIT_FAILURE);
            }
        }
        tokens[*count] = strdup(token);
        if (!tokens[*count]) {
            fprintf(stderr, "Memory allocation failed for token duplication.\n");
            exit(EXIT_FAILURE);
        }
        (*count)++;
        token = strtok(NULL, delimiter);
    }

    free(s);
    return tokens;
}

// Function to check if an update is ordered according to the rules
bool is_update_ordered(Update *update, RuleArray *rules) {
    // Create a map from page to index
    // For simplicity, assume page numbers are positive and not too large
    // Find the maximum page number
    int max_page = 0;
    for (size_t i = 0; i < update->size; i++) {
        if (update->pages[i] > max_page) {
            max_page = update->pages[i];
        }
    }

    // Create index_map
    int *index_map = (int *)malloc((max_page + 1) * sizeof(int));
    if (!index_map) {
        fprintf(stderr, "Memory allocation failed for index_map.\n");
        exit(EXIT_FAILURE);
    }
    for (int i = 0; i <= max_page; i++) {
        index_map[i] = -1; // Initialize to -1 indicating not present
    }
    for (size_t i = 0; i < update->size; i++) {
        index_map[update->pages[i]] = (int)i;
    }

    // Check rules
    for (size_t i = 0; i < rules->size; i++) {
        int x = rules->data[i].x;
        int y = rules->data[i].y;
        if (x <= max_page && y <= max_page && index_map[x] != -1 && index_map[y] != -1) {
            if (index_map[x] > index_map[y]) {
                free(index_map);
                return false;
            }
        }
    }

    free(index_map);
    return true;
}

// Function to perform topological sort on an update according to the rules
Update topological_sort_update(Update *update, RuleArray *rules) {
    // Build the graph and in-degree count based on the rules
    // For simplicity, use adjacency lists with dynamic arrays

    // Find unique nodes
    size_t num_nodes = update->size;
    int *nodes = update->pages;
    // Assign each node an index
    HashMap *node_map = create_hashmap(); // Implement a simple hashmap
    for (size_t i = 0; i < num_nodes; i++) {
        hashmap_put(node_map, nodes[i], (int)i);
    }

    // Create adjacency list
    int **graph = (int **)malloc(num_nodes * sizeof(int *));
    int *graph_sizes = (int *)calloc(num_nodes, sizeof(int));
    int *graph_capacities = (int *)calloc(num_nodes, sizeof(int));
    for (size_t i = 0; i < num_nodes; i++) {
        graph_capacities[i] = 2; // Initial capacity
        graph[i] = (int *)malloc(graph_capacities[i] * sizeof(int));
        if (!graph[i]) {
            fprintf(stderr, "Memory allocation failed for graph adjacency list.\n");
            exit(EXIT_FAILURE);
        }
    }

    // In-degree array
    int *in_degree = (int *)calloc(num_nodes, sizeof(int));
    if (!in_degree) {
        fprintf(stderr, "Memory allocation failed for in_degree.\n");
        exit(EXIT_FAILURE);
    }

    // Build the graph based on rules
    for (size_t i = 0; i < rules->size; i++) {
        int x = rules->data[i].x;
        int y = rules->data[i].y;
        int index_x, index_y;
        if (hashmap_get(node_map, x, &index_x) && hashmap_get(node_map, y, &index_y)) {
            // Add edge from x to y
            if (graph_sizes[index_x] == graph_capacities[index_x]) {
                graph_capacities[index_x] *= 2;
                graph[index_x] = (int *)realloc(graph[index_x], graph_capacities[index_x] * sizeof(int));
                if (!graph[index_x]) {
                    fprintf(stderr, "Memory reallocation failed for graph adjacency list.\n");
                    exit(EXIT_FAILURE);
                }
            }
            graph[index_x][graph_sizes[index_x]++] = index_y;
            in_degree[index_y]++;
        }
    }

    // Initialize the queue with nodes having in-degree 0
    int *queue_arr = (int *)malloc(num_nodes * sizeof(int));
    int front = 0, rear = 0;
    for (size_t i = 0; i < num_nodes; i++) {
        if (in_degree[i] == 0) {
            queue_arr[rear++] = i;
        }
    }

    // Perform BFS
    ListBuffer sorted;
    init_listbuffer(&sorted);
    while (front < rear) {
        int current = queue_arr[front++];
        listbuffer_append(&sorted, nodes[current]);

        for (int i = 0; i < graph_sizes[current]; i++) {
            int neighbor = graph[current][i];
            in_degree[neighbor]--;
            if (in_degree[neighbor] == 0) {
                queue_arr[rear++] = neighbor;
            }
        }
    }

    // Check if topological sort was possible
    if (sorted.size != num_nodes) {
        // Cycle detected or unable to sort
        free(queue_arr);
        for (size_t i = 0; i < num_nodes; i++) {
            free(graph[i]);
        }
        free(graph);
        free(graph_sizes);
        free(graph_capacities);
        free(in_degree);
        destroy_hashmap(node_map);
        destroy_listbuffer(&sorted);
        return (Update){ .pages = NULL, .size = 0, .capacity = 0 };
    }

    // Create the sorted update
    Update sorted_update;
    initUpdate(&sorted_update);
    for (size_t i = 0; i < sorted.size; i++) {
        appendUpdate(&sorted_update, sorted.data[i]);
    }

    // Cleanup
    free(queue_arr);
    for (size_t i = 0; i < num_nodes; i++) {
        free(graph[i]);
    }
    free(graph);
    free(graph_sizes);
    free(graph_capacities);
    free(in_degree);
    destroy_hashmap(node_map);
    destroy_listbuffer(&sorted);

    return sorted_update;
}

// Structure for ListBuffer (dynamic list of integers)
typedef struct {
    int *data;
    size_t size;
    size_t capacity;
} ListBuffer;

// Initialize ListBuffer
void init_listbuffer(ListBuffer *lb) {
    lb->size = 0;
    lb->capacity = 10;
    lb->data = (int *)malloc(lb->capacity * sizeof(int));
    if (!lb->data) {
        fprintf(stderr, "Memory allocation failed for ListBuffer.\n");
        exit(EXIT_FAILURE);
    }
}

// Append to ListBuffer
void listbuffer_append(ListBuffer *lb, int value) {
    if (lb->size == lb->capacity) {
        lb->capacity *= 2;
        lb->data = (int *)realloc(lb->data, lb->capacity * sizeof(int));
        if (!lb->data) {
            fprintf(stderr, "Memory reallocation failed for ListBuffer.\n");
            exit(EXIT_FAILURE);
        }
    }
    lb->data[lb->size++] = value;
}

// Free ListBuffer
void destroy_listbuffer(ListBuffer *lb) {
    free(lb->data);
    lb->data = NULL;
    lb->size = lb->capacity = 0;
}

// Simple hashmap implementation for integers
typedef struct hashmap_entry {
    int key;
    int value;
    struct hashmap_entry *next;
} hashmap_entry;

typedef struct {
    hashmap_entry **buckets;
    size_t size;
    size_t capacity;
} HashMap;

// Initialize HashMap
HashMap* create_hashmap() {
    HashMap *map = (HashMap *)malloc(sizeof(HashMap));
    if (!map) {
        fprintf(stderr, "Memory allocation failed for HashMap.\n");
        exit(EXIT_FAILURE);
    }
    map->capacity = 100;
    map->size = 0;
    map->buckets = (hashmap_entry **)calloc(map->capacity, sizeof(hashmap_entry *));
    if (!map->buckets) {
        fprintf(stderr, "Memory allocation failed for HashMap buckets.\n");
        exit(EXIT_FAILURE);
    }
    return map;
}

// Simple hash function
size_t hash_int(int key, size_t capacity) {
    return ((unsigned int)key) % capacity;
}

// Insert key-value pair into HashMap
void hashmap_put(HashMap *map, int key, int value) {
    size_t index = hash_int(key, map->capacity);
    hashmap_entry *entry = map->buckets[index];
    while (entry) {
        if (entry->key == key) {
            entry->value = value; // Update existing key
            return;
        }
        entry = entry->next;
    }
    // Insert new entry
    hashmap_entry *new_entry = (hashmap_entry *)malloc(sizeof(hashmap_entry));
    if (!new_entry) {
        fprintf(stderr, "Memory allocation failed for HashMap entry.\n");
        exit(EXIT_FAILURE);
    }
    new_entry->key = key;
    new_entry->value = value;
    new_entry->next = map->buckets[index];
    map->buckets[index] = new_entry;
    map->size++;
}

// Get value by key from HashMap
bool hashmap_get(HashMap *map, int key, int *value) {
    size_t index = hash_int(key, map->capacity);
    hashmap_entry *entry = map->buckets[index];
    while (entry) {
        if (entry->key == key) {
            *value = entry->value;
            return true;
        }
        entry = entry->next;
    }
    return false;
}

// Free HashMap
void destroy_hashmap(HashMap *map) {
    for (size_t i = 0; i < map->capacity; i++) {
        hashmap_entry *entry = map->buckets[i];
        while (entry) {
            hashmap_entry *temp = entry;
            entry = entry->next;
            free(temp);
        }
    }
    free(map->buckets);
    free(map);
}

// Function to get the middle page of an update
int get_middle_page(Update *update) {
    if (update->size == 0) {
        return -1; // Undefined
    }
    return update->pages[update->size / 2];
}

int main() {
    const char *file_path = "input.txt";
    FILE *file = fopen(file_path, "r");
    if (!file) {
        fprintf(stderr, "Error: Unable to open the file %s\n", file_path);
        return EXIT_FAILURE;
    }

    // Read the entire file content
    char *content = NULL;
    size_t len = 0;
    ssize_t read_len;
    size_t buffer_size = 0;
    size_t buffer_capacity = 1024;
    content = (char *)malloc(buffer_capacity * sizeof(char));
    if (!content) {
        fprintf(stderr, "Memory allocation failed for content.\n");
        fclose(file);
        return EXIT_FAILURE;
    }

    while ((read_len = getline(&content, &buffer_size, file)) != -1) {
        if ((size_t)(len + read_len) >= buffer_capacity) {
            buffer_capacity *= 2;
            content = (char *)realloc(content, buffer_capacity * sizeof(char));
            if (!content) {
                fprintf(stderr, "Memory reallocation failed for content.\n");
                fclose(file);
                return EXIT_FAILURE;
            }
        }
        len += read_len;
    }
    fclose(file);

    // Trim the content
    while (len > 0 && (content[len - 1] == '\n' || content[len - 1] == '\r' || content[len - 1] == ' ' || content[len - 1] == '\t')) {
        content[len - 1] = '\0';
        len--;
    }

    // Split content into rules and updates based on two consecutive newlines
    char *rules_section = NULL;
    char *updates_section = NULL;

    char *delimiter = "\n\n";
    char *split_pos = strstr(content, delimiter);
    if (!split_pos) {
        fprintf(stderr, "Invalid input format. Expected two sections separated by two newlines.\n");
        free(content);
        return EXIT_FAILURE;
    }

    // Separate the sections
    *split_pos = '\0';
    rules_section = content;
    updates_section = split_pos + strlen(delimiter);

    // Parse rules
    RuleArray rules;
    initRuleArray(&rules);
    int rule_count;
    char **rule_tokens = split(rules_section, "\n", &rule_count);
    for (int i = 0; i < rule_count; i++) {
        char *line = rule_tokens[i];
        if (strlen(line) == 0) {
            free(line);
            continue;
        }
        int delimiter_index = -1;
        for (int j = 0; j < strlen(line); j++) {
            if (line[j] == '|') {
                delimiter_index = j;
                break;
            }
        }
        if (delimiter_index == -1) {
            fprintf(stderr, "Invalid rule format: %s\n", line);
            free(line);
            continue;
        }
        line[delimiter_index] = '\0';
        char *x_str = line;
        char *y_str = line + delimiter_index + 1;
        int x = atoi(x_str);
        int y = atoi(y_str);
        Rule rule = {x, y};
        appendRuleArray(&rules, rule);
        free(line);
    }
    free(rule_tokens);

    // Parse updates
    UpdateArray updates;
    initUpdateArray(&updates);
    int update_count;
    char **update_tokens = split(updates_section, "\n", &update_count);
    for (int i = 0; i < update_count; i++) {
        char *line = update_tokens[i];
        if (strlen(line) == 0) {
            free(line);
            continue;
        }
        int comma_count;
        char **page_tokens = split(line, ",", &comma_count);
        Update update;
        initUpdate(&update);
        bool valid = true;
        for (int j = 0; j < comma_count; j++) {
            char *page_str = page_tokens[j];
            int page = atoi(page_str);
            appendUpdate(&update, page);
            free(page_str);
        }
        free(page_tokens);
        if (valid) {
            appendUpdateArray(&updates, update);
        } else {
            freeUpdate(&update);
        }
    }
    free(update_tokens);

    // Identify correctly ordered updates and their middle page numbers
    long long sum_middle_pages = 0;
    for (size_t i = 0; i < updates.size; i++) {
        if (is_update_ordered(&updates.data[i], &rules)) {
            int middle = get_middle_page(&updates.data[i]);
            sum_middle_pages += middle;
        }
    }
    printf("Sum of middle pages for correctly ordered updates: %lld\n", sum_middle_pages);

    // Correct the incorrectly ordered updates and find their middle page numbers
    long long sum_incorrect_middle_pages = 0;
    for (size_t i = 0; i < updates.size; i++) {
        if (!is_update_ordered(&updates.data[i], &rules)) {
            Update corrected_update = topological_sort_update(&updates.data[i], &rules);
            if (corrected_update.size == 0) {
                fprintf(stderr, "Cycle detected or unable to sort update: ");
                for (size_t j = 0; j < updates.data[i].size; j++) {
                    fprintf(stderr, "%d", updates.data[i].pages[j]);
                    if (j != updates.data[i].size - 1) {
                        fprintf(stderr, ", ");
                    }
                }
                fprintf(stderr, "\n");
                continue;
            }
            int middle = get_middle_page(&corrected_update);
            sum_incorrect_middle_pages += middle;
            free(corrected_update.pages);
        }
    }

    printf("Sum of middle pages for corrected updates: %lld\n", sum_incorrect_middle_pages);

    // Cleanup
    free(content);
    freeRuleArray(&rules);
    freeUpdateArray(&updates);

    return EXIT_SUCCESS;
}
