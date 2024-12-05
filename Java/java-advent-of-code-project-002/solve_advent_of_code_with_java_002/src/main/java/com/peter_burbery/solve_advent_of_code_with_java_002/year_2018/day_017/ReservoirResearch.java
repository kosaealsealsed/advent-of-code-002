package com.peter_burbery.solve_advent_of_code_with_java_002.year_2018.day_017;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ReservoirResearch {

    /**
     * Represents a point in the grid with x and y coordinates.
     */
    static class Point {
        int x;
        int y;

        Point(int x, int y) {
            this.x = x;
            this.y = y;
        }

        // Override equals and hashCode for correct behavior in hash-based collections
        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof Point)) return false;
            Point point = (Point) o;
            return x == point.x &&
                   y == point.y;
        }

        @Override
        public int hashCode() {
            return Objects.hash(x, y);
        }
    }

    /**
     * Parses the input lines to extract clay vein positions and determine grid boundaries.
     *
     * @param inputLines List of strings representing clay vein definitions.
     * @return An array containing:
     *         [0] - Set<Point> representing clay positions,
     *         [1] - Integer min_x,
     *         [2] - Integer max_x,
     *         [3] - Integer min_y,
     *         [4] - Integer max_y
     */
    public static Object[] parseInput(List<String> inputLines) {
        Set<Point> clay = new HashSet<>();
        int min_x = Integer.MAX_VALUE;
        int max_x = Integer.MIN_VALUE;
        int min_y = Integer.MAX_VALUE;
        int max_y = Integer.MIN_VALUE;

        Pattern pattern = Pattern.compile("(x|y)=(\\d+), (x|y)=(\\d+)..(\\d+)");

        for (String line : inputLines) {
            Matcher matcher = pattern.matcher(line);
            if (!matcher.matches()) {
                continue; // Skip lines that don't match the expected format
            }
            String fixedAxis = matcher.group(1);
            int fixedVal = Integer.parseInt(matcher.group(2));
            String varAxis = matcher.group(3);
            int varStart = Integer.parseInt(matcher.group(4));
            int varEnd = Integer.parseInt(matcher.group(5));

            for (int var = varStart; var <= varEnd; var++) {
                int x, y;
                if (fixedAxis.equals("x")) {
                    x = fixedVal;
                    y = var;
                } else {
                    y = fixedVal;
                    x = var;
                }
                clay.add(new Point(x, y));
                min_x = Math.min(min_x, x);
                max_x = Math.max(max_x, x);
                min_y = Math.min(min_y, y);
                max_y = Math.max(max_y, y);
            }
        }

        return new Object[]{clay, min_x, max_x, min_y, max_y};
    }

    /**
     * Prints the x-axis labels above the grid for reference.
     *
     * @param min_x Minimum x-coordinate in the grid.
     * @param max_x Maximum x-coordinate in the grid.
     */
    public static void printXLabels(int min_x, int max_x) {
        StringBuilder hundreds = new StringBuilder();
        StringBuilder tens = new StringBuilder();
        StringBuilder units = new StringBuilder();

        for (int x = min_x; x <= max_x; x++) {
            hundreds.append(x / 100);
        }
        for (int x = min_x; x <= max_x; x++) {
            tens.append((x % 100) / 10);
        }
        for (int x = min_x; x <= max_x; x++) {
            units.append(x % 10);
        }

        System.out.println("     " + hundreds.toString());
        System.out.println("     " + tens.toString());
        System.out.println("     " + units.toString());
    }

    /**
     * Renders the grid with clay, sand, flowing water, settled water, and the water spring.
     *
     * @param grid   Map<Point, Character> representing the grid state.
     * @param min_x  Minimum x-coordinate in the grid.
     * @param max_x  Maximum x-coordinate in the grid.
     * @param min_y  Minimum y-coordinate to display.
     * @param max_y  Maximum y-coordinate to display.
     */
    public static void visualize(Map<Point, Character> grid, int min_x, int max_x, int min_y, int max_y) {
        int padding = 1;
        int grid_min_x = min_x - padding;
        int grid_max_x = max_x + padding;
        int grid_min_y = min_y;
        int grid_max_y = max_y;

        // Print x-axis labels
        printXLabels(grid_min_x, grid_max_x);

        // Render each row of the grid
        for (int y = grid_min_y; y <= grid_max_y; y++) {
            StringBuilder line = new StringBuilder();
            for (int x = grid_min_x; x <= grid_max_x; x++) {
                Point current = new Point(x, y);
                if (x == 500 && y == 0) {
                    line.append('+'); // Water spring
                } else if (grid.containsKey(current)) {
                    line.append(grid.get(current));
                } else {
                    line.append('.'); // Sand
                }
            }
            // Right-align y for better formatting
            System.out.printf("%3d %s%n", y, line.toString());
        }
    }

    /**
     * Simulates water flow based on clay veins and the specified number of water units.
     *
     * @param clay        Set<Point> representing clay positions.
     * @param min_x       Minimum x-coordinate in the clay veins.
     * @param max_x       Maximum x-coordinate in the clay veins.
     * @param min_y       Minimum y-coordinate in the clay veins.
     * @param max_y       Maximum y-coordinate in the clay veins.
     * @param water_units Number of water units to simulate.
     * @return Map<Point, Character> representing the updated grid after simulation.
     */
    public static Map<Point, Character> simulateWaterFlowCore(Set<Point> clay, int min_x, int max_x, int min_y, int max_y, int water_units) {
        Map<Point, Character> grid = new HashMap<>();
        // Initialize the grid with clay positions
        for (Point p : clay) {
            grid.put(p, '#');
        }

        // Define the spring position
        Point spring = new Point(500, 0);
        grid.put(spring, '+');

        // Directions for spreading: left (-1) and right (+1)
        int[] directions = {-1, 1};

        // Simulate each water unit
        for (int unit = 0; unit < water_units; unit++) {
            Deque<Point> stack = new ArrayDeque<>();
            stack.push(spring);

            while (!stack.isEmpty()) {
                Point current = stack.pop();
                int x = current.x;
                int y = current.y;

                // Move downward as far as possible
                while (true) {
                    Point below = new Point(x, y + 1);
                    if (y + 1 > max_y) {
                        // Water flows out of the grid
                        break;
                    }
                    if (!grid.containsKey(below)) {
                        grid.put(below, '|'); // Mark as flowing
                        y += 1;
                    } else if (grid.get(below) == '|') {
                        // Water is flowing below; cannot settle
                        break;
                    } else if (grid.get(below) == '#' || grid.get(below) == '~') {
                        // Blocked; need to spread
                        break;
                    }
                }

                // After moving down, attempt to spread left and right
                if (y + 1 > max_y) {
                    // Water has flowed out; do not mark current position
                    continue;
                }

                // Attempt to spread left and right
                Map<Integer, Integer> spreadResults = new HashMap<>(); // Direction -> x position

                for (int direction : directions) {
                    int spread_x = x;
                    int spread_y = y;
                    while (true) {
                        spread_x += direction;
                        Point spreadPoint = new Point(spread_x, spread_y);
                        Point belowSpread = new Point(spread_x, spread_y + 1);

                        if (clay.contains(spreadPoint)) {
                            // Bounded by clay on this side
                            spreadResults.put(direction, spread_x);
                            break;
                        }

                        if (!grid.containsKey(belowSpread) || grid.get(belowSpread) == '|') {
                            // Water can fall down here; mark as flowing and stop spreading
                            grid.put(spreadPoint, '|');
                            stack.push(spreadPoint);
                            spreadResults.put(direction, spread_x);
                            break;
                        } else {
                            // Mark as flowing
                            grid.put(spreadPoint, '|');
                        }
                    }
                }

                // Determine if water is bounded on both sides
                boolean boundedLeft = spreadResults.containsKey(-1) && spreadResults.get(-1) != x;
                boolean boundedRight = spreadResults.containsKey(1) && spreadResults.get(1) != x;

                if (boundedLeft && boundedRight) {
                    // Both sides are bounded by clay; settle the water
                    for (int fill_x = spreadResults.get(-1) + 1; fill_x < spreadResults.get(1); fill_x++) {
                        Point fillPoint = new Point(fill_x, y);
                        grid.put(fillPoint, '~');
                    }
                    // After settling, the water above might also settle
                    Point above = new Point(x, y - 1);
                    stack.push(above);
                } else {
                    // Water flows out; do not settle
                    // No action needed as flowing water is already marked
                }
            }
        }

        return grid;
    }

    /**
     * Counts the number of tiles that water can reach within the specified y-range.
     *
     * @param grid   Map<Point, Character> representing the grid state.
     * @param min_y Minimum y-coordinate to consider.
     * @param max_y Maximum y-coordinate to consider.
     * @return Number of reachable tiles.
     */
    public static int countReachableTiles(Map<Point, Character> grid, int min_y, int max_y) {
        int count = 0;
        for (Map.Entry<Point, Character> entry : grid.entrySet()) {
            Point p = entry.getKey();
            char val = entry.getValue();
            if (p.y >= min_y && p.y <= max_y && (val == '|' || val == '~')) {
                count++;
            }
        }
        return count;
    }

    /**
     * Orchestrates the simulation and visualization.
     *
     * @param inputString Multi-line string representing clay vein definitions.
     * @param waterUnits  Number of water units to simulate.
     */
    public static void main(String[] args) {
        // Example input representing clay veins
        String inputString = """
        x=495, y=2..7
        y=7, x=495..501
        x=501, y=3..7
        x=498, y=2..4
        x=506, y=1..2
        x=498, y=10..13
        x=504, y=10..13
        y=13, x=498..504
        """;

        // Define different water units for testing
        List<Integer> testUnits = Arrays.asList(1, 4, 5, 9, 10);

        // Split the input string into lines
        List<String> inputLines = Arrays.asList(inputString.strip().split("\\n"));

        // Parse the input to extract clay positions and grid boundaries
        Object[] parsedInput = parseInput(inputLines);
        @SuppressWarnings("unchecked")
        Set<Point> clay = (Set<Point>) parsedInput[0];
        int min_x = (int) parsedInput[1];
        int max_x = (int) parsedInput[2];
        int min_y = (int) parsedInput[3];
        int max_y = (int) parsedInput[4];

        // Initialize the grid with clay positions
        // (Optional: If you want to reset the grid for each simulation)
        // However, for cumulative simulations, keep the grid updated.

        // Simulate and visualize for each specified number of water units
        for (int units : testUnits) {
            System.out.printf("\n--- Simulation after injecting %d water unit(s) ---\n\n", units);
            // Clone the clay set to avoid mutating the original during simulation
            Set<Point> clayClone = new HashSet<>();
            for (Point p : clay) {
                clayClone.add(new Point(p.x, p.y));
            }

            // Simulate water flow
            Map<Point, Character> grid = simulateWaterFlowCore(clayClone, min_x, max_x, min_y, max_y, units);

            // Count reachable tiles (flowing and settled water) within y-range
            int reachable = countReachableTiles(grid, min_y, max_y);

            // Print the number of reachable tiles
            System.out.printf("Number of tiles the water can reach: %d%n%n", reachable);

            // Visualize the final state
            visualize(grid, min_x, max_x, min_y, max_y);
        }
    }
}

