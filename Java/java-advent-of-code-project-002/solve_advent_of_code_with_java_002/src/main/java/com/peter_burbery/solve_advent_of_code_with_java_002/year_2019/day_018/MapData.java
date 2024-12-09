package com.peter_burbery.solve_advent_of_code_with_java_002.year_2019.day_018;

import java.util.Map;

//Helper class to store the map data
public class MapData {
 char[][] grid;
 Map<Character, Position> keys;
 Map<Character, Position> doors;
 Position start;

 MapData(char[][] grid, Map<Character, Position> keys, Map<Character, Position> doors, Position start) {
     this.grid = grid;
     this.keys = keys;
     this.doors = doors;
     this.start = start;
 }
}