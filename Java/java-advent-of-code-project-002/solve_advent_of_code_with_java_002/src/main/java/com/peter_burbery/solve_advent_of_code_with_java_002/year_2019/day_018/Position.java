package com.peter_burbery.solve_advent_of_code_with_java_002.year_2019.day_018;

import java.util.Objects;

//Helper class to represent positions
public class Position {
 int x, y;

 Position(int x, int y) {
     this.x = x;
     this.y = y;
 }

 @Override
 public boolean equals(Object o) {
     if (this == o) return true;
     if (!(o instanceof Position)) return false;
     Position position = (Position) o;
     return x == position.x && y == position.y;
 }

 @Override
 public int hashCode() {
     return Objects.hash(x, y);
 }
}
