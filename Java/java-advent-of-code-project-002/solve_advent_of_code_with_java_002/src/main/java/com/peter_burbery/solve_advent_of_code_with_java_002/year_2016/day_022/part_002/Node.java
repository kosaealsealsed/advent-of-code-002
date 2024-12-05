package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_022.part_002;

public class Node {
    int x, y, size, used, avail;

    public Node(int x, int y, int size, int used) {
        this.x = x;
        this.y = y;
        this.size = size;
        this.used = used;
        this.avail = size - used;
    }

    public boolean isEmpty() {
        return this.used == 0;
    }

    @Override
    public String toString() {
        return String.format("Node(%d,%d) - Size: %dT, Used: %dT, Avail: %dT", x, y, size, used, avail);
    }
}

