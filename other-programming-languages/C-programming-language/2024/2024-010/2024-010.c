#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define INPUT_FILE "input.txt"
#define MAX_ROWS 1000
#define MAX_COLS 1000

typedef struct {
    int row, col;
} Point;

typedef struct {
    Point data[MAX_ROWS * MAX_COLS];
    int front, back;
} Queue;

void enqueue(Queue* q, int row, int col) {
    q->data[q->back++] = (Point){row, col};
}

Point dequeue(Queue* q) {
    return q->data[q->front++];
}

int is_queue_empty(Queue* q) {
    return q->front == q->back;
}

void neighbors(int r, int c, int rows, int cols, Point* result, int* count) {
    *count = 0;
    int directions[4][2] = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};
    for (int i = 0; i < 4; i++) {
        int nr = r + directions[i][0];
        int nc = c + directions[i][1];
        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
            result[(*count)++] = (Point){nr, nc};
        }
    }
}

int read_map(const char* filename, int grid[MAX_ROWS][MAX_COLS], int* cols) {
    FILE* file = fopen(filename, "r");
    if (!file) {
        perror("Error opening file");
        exit(EXIT_FAILURE);
    }

    int rows = 0;
    char line[MAX_COLS + 2];
    while (fgets(line, sizeof(line), file)) {
        printf("Read line: %s", line); // Debug
        *cols = strlen(line) - 1; // Exclude newline
        for (int c = 0; c < *cols; c++) {
            grid[rows][c] = line[c] - '0';
        }
        rows++;
    }
    fclose(file);
    printf("Rows: %d, Cols: %d\n", rows, *cols); // Debug
    return rows;
}
int find_trailhead_scores(int grid[MAX_ROWS][MAX_COLS], int rows, int cols) {
    int total_score = 0;
    Queue queue = {.front = 0, .back = 0};
    int visited[MAX_ROWS][MAX_COLS] = {0};
    Point nbs[4];
    int nbs_count;

    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (grid[r][c] != 0) continue;

            memset(visited, 0, sizeof(visited));
            queue.front = queue.back = 0;

            enqueue(&queue, r, c);
            visited[r][c] = 1;

            int reachable_nines = 0;

            while (!is_queue_empty(&queue)) {
                Point p = dequeue(&queue);
                int current_height = grid[p.row][p.col];

                if (current_height == 9) {
                    reachable_nines++;
                } else {
                    neighbors(p.row, p.col, rows, cols, nbs, &nbs_count);
                    for (int i = 0; i < nbs_count; i++) {
                        int nr = nbs[i].row, nc = nbs[i].col;
                        if (!visited[nr][nc] && grid[nr][nc] == current_height + 1) {
                            visited[nr][nc] = 1;
                            enqueue(&queue, nr, nc);
                        }
                    }
                }
            }

            total_score += reachable_nines;
        }
    }

    return total_score;
}

int count_paths(int r, int c, int grid[MAX_ROWS][MAX_COLS], int dp[MAX_ROWS][MAX_COLS], int rows, int cols) {
    if (dp[r][c] != -1) return dp[r][c];

    if (grid[r][c] == 9) {
        dp[r][c] = 1;
        return 1;
    }

    int total_paths = 0;
    Point nbs[4];
    int nbs_count;
    neighbors(r, c, rows, cols, nbs, &nbs_count);

    for (int i = 0; i < nbs_count; i++) {
        int nr = nbs[i].row, nc = nbs[i].col;
        if (grid[nr][nc] == grid[r][c] + 1) {
            total_paths += count_paths(nr, nc, grid, dp, rows, cols);
        }
    }

    dp[r][c] = total_paths;
    return total_paths;
}

int calculate_total_rating(int grid[MAX_ROWS][MAX_COLS], int rows, int cols) {
    int dp[MAX_ROWS][MAX_COLS];
    memset(dp, -1, sizeof(dp));

    int total_rating = 0;
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (grid[r][c] == 0) {
                total_rating += count_paths(r, c, grid, dp, rows, cols);
            }
        }
    }
    return total_rating;
}

int main() {
    clock_t start, end;
    int grid[MAX_ROWS][MAX_COLS];
    int cols;
    int rows = read_map(INPUT_FILE, grid, &cols);

    start = clock();
    int total_score = find_trailhead_scores(grid, rows, cols);
    end = clock();
    printf("Part 1 Result: %d\n", total_score);
    printf("Time taken for Part 1: %.9f s\n", (double)(end - start) / CLOCKS_PER_SEC);

    start = clock();
    int total_rating = calculate_total_rating(grid, rows, cols);
    end = clock();
    printf("Part 2 Result: %d\n", total_rating);
    printf("Time taken for Part 2: %.9f s\n", (double)(end - start) / CLOCKS_PER_SEC);

    return 0;
}
