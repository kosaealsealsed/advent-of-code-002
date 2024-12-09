package com.peter_burbery.solve_advent_of_code_with_java_002.year_2019.day_018;

import java.util.Objects;

//Helper class to represent the state for A*
public class State {
 Position position;
 int keysMask;  // Bitmask representing collected keys
 int steps;     // Cost to reach this state
 int f;         // Estimated total cost (g + h)

 State(Position position, int keysMask, int steps, int f) {
     this.position = position;
     this.keysMask = keysMask;
     this.steps = steps;
     this.f = f;
 }

 @Override
 public boolean equals(Object o) {
     if (this == o) return true;
     if (!(o instanceof State)) return false;
     State state = (State) o;
     return keysMask == state.keysMask &&
            Objects.equals(position, state.position);
 }

 @Override
 public int hashCode() {
     return Objects.hash(position, keysMask);
 }
}
