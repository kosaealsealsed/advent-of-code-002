#!/usr/bin/env raku

use v6;
use Time::HiRes 'now';    # Corrected import of 'now'

# Helper class to represent a position in the grid
class Position does Equatable {
    has Int $.row;
    has Int $.col;
}

# Helper class to represent a state (position and direction) of the guard
class State does Equatable {
    has Int $.row;
    has Int $.col;
    has Int $.direction;
}

# Simple Pair class to hold two related objects
class Pair does Equatable {
    has $.first;
    has $.second;
}

# Main class for detecting guard patrol loops
class GuardPatrolLoopDetector {
    # Direction mappings
    constant %DIRECTION_MAP = { '^' => 0, '>' => 1, 'v' => 2, '<' => 3 };

    # Direction offsets: Up, Right, Down, Left
    constant @DIRECTION_OFFSETS = [
        [-1, 0], # Up
        [0, 1],  # Right
        [1, 0],  # Down
        [0, -1], # Left
    ];

    # Entry point of the program
    method main(@args) is export {
        # Specify the path to your input file here
        my $file-path = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-006\\input.txt";

        try {
            # Part 1: Count distinct positions visited without obstructions
            my $distinct-positions = GuardPatrolLoopDetector.countDistinctPositionsVisited($file-path);
            say "Number of distinct positions visited: $distinct-positions";

            # Part 2: Detect loops with obstructions and measure execution times
            GuardPatrolLoopDetector.countObstructionPositions($file-path);
        }
        CATCH {
            default {
                warn "Error: $_";
            }
        }
    }

    # Part 1: Counts the number of distinct positions visited by the guard without any obstructions
    method countDistinctPositionsVisited(Str $file-path --> Int) is class {
        # Parse the grid
        my @grid = GuardPatrolLoopDetector.parseGrid($file-path);

        # Find the guard's starting position and direction
        my $guard-info = GuardPatrolLoopDetector.findGuard(@grid);
        my $guard-pos  = $guard-info.first;
        my $guard-dir  = $guard-info.second;

        # Initialize visited positions set using a hash for uniqueness
        my %visited-positions;
        %visited-positions{"$guard-pos.row,$guard-pos.col"} = True;

        # Simulate the guard's movement
        loop {
            my $dr = @DIRECTION_OFFSETS[$guard-dir][0];
            my $dc = @DIRECTION_OFFSETS[$guard-dir][1];
            my $new-r = $guard-pos.row + $dr;
            my $new-c = $guard-pos.col + $dc;

            # Check boundaries
            if $new-r < 0 || $new-r >= @grid.elems || $new-c < 0 || $new-c >= @grid[0].elems {
                last; # Guard exits the mapped area
            }

            if @grid[$new-r][$new-c] eq '#' {
                # Turn right if obstacle ahead
                $guard-dir = ($guard-dir + 1) % 4;
            }
            else {
                # Move forward
                $guard-pos = Position.new(row => $new-r, col => $new-c);
                %visited-positions{"$guard-pos.row,$guard-pos.col"} = True;
            }
        }

        # Return the number of distinct positions visited
        return %visited-positions.keys.elems;
    }

    # Part 2: Counts the number of obstruction positions that cause the guard to loop indefinitely
    # Also measures and prints execution times
    method countObstructionPositions(Str $file-path) is class {
        # Start total timing
        my $total-start-time = now;

        # Parse the grid
        my @grid = GuardPatrolLoopDetector.parseGrid($file-path);

        # Find the guard's starting position and direction
        my $guard-info = GuardPatrolLoopDetector.findGuard(@grid);
        my $guard-pos  = $guard-info.first;
        my $guard-dir  = $guard-info.second;

        # Time to find obstruction positions
        my $obstruction-start-time = now;
        my @possible-obstructions    = GuardPatrolLoopDetector.getPossibleObstructions(@grid, $guard-pos);
        my $obstruction-end-time   = now;
        my $obstruction-time       = $obstruction-end-time - $obstruction-start-time; # seconds

        # Print the first header and line: [time_obstruction_positions] [total_obstruction_positions]
        say "time, denominator";
        printf "%.9f %d\n", $obstruction-time, @possible-obstructions.elems;

        # Print header for batches
        say "batch, batch time, cumulative time";

        # Initialize loop counter
        my $loop-count = 0;
        my $total      = @possible-obstructions.elems;

        # Initialize timing for batches
        my $batch-size      = 1000;
        my $batch-start     = now;
        my $cumulative-time = $obstruction-time; # cumulative_time includes obstruction_time

        for @possible-obstructions.kv -> $idx, $obstruction {
            # Place obstruction
            @grid[$obstruction.row][$obstruction.col] = '#';

            if GuardPatrolLoopDetector.simulateMovement(@grid, $guard-pos, $guard-dir) {
                $loop-count++;
            }

            # Remove obstruction
            @grid[$obstruction.row][$obstruction.col] = '.';

            # Check if batch size is reached or it's the last position
            if (($idx + 1) % $batch-size == 0 || ($idx + 1) == $total) {
                my $batch-end-time = now;
                my $batch-time     = $batch-end-time - $batch-start; # seconds
                $cumulative-time += $batch-time;
                printf "%d %.9f %.9f\n", $idx + 1, $batch-time, $cumulative-time;
                $batch-start = now; # Reset batch start time
            }
        }

        # End total timing
        my $total-end-time = now;
        my $total-time     = $total-end-time - $total-start-time; # Total time from start to end

        # Print final answer header and line: [answer] [answer_time]
        say "answer, answer time";
        printf "%d %.9f\n", $loop-count, $total-time;
    }

    # Parses the grid from the given file
    method parseGrid(Str $file-path --> @grid) is class {
        my @grid-list;
        for $file-path.IO.lines -> $line {
            @grid-list.push: $line.trim.comb;
        }

        if @grid-list.is-empty {
            die "The grid is empty.";
        }

        # Ensure all rows have the same length
        my $cols = @grid-list[0].elems;
        for @grid-list -> $row {
            if $row.elems != $cols {
                die "Inconsistent row lengths in the grid.";
            }
        }

        return @grid-list;
    }

    # Finds the guard's starting position and direction
    method findGuard(@grid --> Pair) is class {
        for @grid.kv -> $r, $row {
            for $row.kv -> $c, $cell {
                if %DIRECTION_MAP{$cell}:exists {
                    my $guard-pos = Position.new(row => $r, col => $c);
                    my $guard-dir = %DIRECTION_MAP{$cell};
                    @grid[$r][$c] = '.'; # Clear the starting position
                    return Pair.new(first => $guard-pos, second => $guard-dir);
                }
            }
        }
        die "Guard not found in the grid.";
    }

    # Retrieves all possible obstruction positions excluding the guard's starting position and already obstructed cells
    method getPossibleObstructions(@grid, Position $guard-pos --> @possible) is class {
        my @possible;
        for @grid.kv -> $r, $row {
            for $row.kv -> $c, $cell {
                if (($r != $guard-pos.row || $c != $guard-pos.col) && $cell eq '.') {
                    @possible.push: Position.new(row => $r, col => $c);
                }
            }
        }
        return @possible;
    }

    # Simulates the guard's movement on the grid
    method simulateMovement(@grid, Position $start-pos, Int $start-dir --> Bool) is class {
        my %visited-states;

        my $r         = $start-pos.row;
        my $c         = $start-pos.col;
        my $direction = $start-dir;

        loop {
            my $current-state-key = "$r,$c,$direction";
            if %visited-states{$current-state-key}:exists {
                return True; # Loop detected
            }
            %visited-states{$current-state-key} = True;

            my $dr = @DIRECTION_OFFSETS[$direction][0];
            my $dc = @DIRECTION_OFFSETS[$direction][1];
            my $new-r = $r + $dr;
            my $new-c = $c + $dc;

            # Check boundaries
            if $new-r < 0 || $new-r >= @grid.elems || $new-c < 0 || $new-c >= @grid[0].elems {
                return False; # Guard exits the grid
            }

            if @grid[$new-r][$new-c] eq '#' {
                # Turn right if obstacle ahead
                $direction = ($direction + 1) % 4;
            }
            else {
                # Move forward
                $r = $new-r;
                $c = $new-c;
            }
        }
    }
}

# Instantiate and run the GuardPatrolLoopDetector
GuardPatrolLoopDetector.main(@*ARGS);
