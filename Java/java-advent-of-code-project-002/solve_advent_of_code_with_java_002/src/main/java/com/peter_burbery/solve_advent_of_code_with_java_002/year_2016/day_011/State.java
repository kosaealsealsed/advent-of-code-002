package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_011;

import java.util.*;

public class State {
    public int elevatorFloor;
    public Map<String, Integer> rtgFloors;
    public Map<String, Integer> chipFloors;

    // Constructor
    public State(int elevatorFloor, Map<String, Integer> rtgFloors, Map<String, Integer> chipFloors) {
        this.elevatorFloor = elevatorFloor;
        this.rtgFloors = rtgFloors;
        this.chipFloors = chipFloors;
    }

    // Generate a canonical key for the state
    public String getCanonicalKey() {
        // Pair generator and chip positions
        List<String> pairs = new ArrayList<>();
        for (String element : rtgFloors.keySet()) {
            int rtgFloor = rtgFloors.get(element);
            int chipFloor = chipFloors.get(element);
            pairs.add(rtgFloor + "," + chipFloor);
        }

        // Sort pairs for canonical representation
        Collections.sort(pairs);

        // Include elevator position
        return elevatorFloor + "|" + String.join("|", pairs);
    }

    // Override equals and hashCode to use canonical key
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!(obj instanceof State)) return false;
        State other = (State) obj;
        return this.getCanonicalKey().equals(other.getCanonicalKey());
    }

    @Override
    public int hashCode() {
        return this.getCanonicalKey().hashCode();
    }

    // Check if the state is a goal state
    public boolean isGoal(int targetFloor) {
        for (int floor : rtgFloors.values()) {
            if (floor != targetFloor) return false;
        }
        for (int floor : chipFloors.values()) {
            if (floor != targetFloor) return false;
        }
        return elevatorFloor == targetFloor;
    }

    // Validate the state (already existing logic)
    public boolean isValid() {
        // Ensure no chip is fried
        for (String element : chipFloors.keySet()) {
            int chipFloor = chipFloors.get(element);
            int rtgFloor = rtgFloors.get(element);

            // If a chip is not with its generator, it must not share a floor with any other generator
            if (chipFloor != rtgFloor) {
                for (int otherRtgFloor : rtgFloors.values()) {
                    if (otherRtgFloor == chipFloor) return false;
                }
            }
        }
        return true;
    }
}
