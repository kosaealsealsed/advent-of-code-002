use v6;

class Part001 {
    method main() {
        # Start timing
        my $start-time = now;

        # Load the input data from the file
        my $file-path = "C:\\advent-of-code-002\\input-files\\2024\\2024-006\\input.txt";

        my @maze;

        try {
            if $file-path.IO.e {
                my @lines = $file-path.IO.lines.list;  # Cache the lines as a list
                for @lines -> $line {
                    @maze.push: $line.comb.Array;  # Convert each line to a mutable Array
                }
            } else {
                say "File not found: $file-path";
                exit;
            }
        }
        CATCH {
            default {
                say "Failed to load input file: $_";
                exit;
            }
        }

        # Constants for directions and turning
        my %DIRECTIONS = '^' => [-1, 0],
                         '>' => [ 0, 1],
                         'v' => [ 1, 0],
                         '<' => [ 0, -1];

        my %TURN_RIGHT = '^' => '>',
                          '>' => 'v',
                          'v' => '<',
                          '<' => '^';

        # Find the initial position and direction of the guard
        my $rows = @maze.elems;
        my $cols = @maze[0].elems;
        my $guard-pos;
        my $guard-dir;

        outer: for ^$rows -> $r {
            for ^$cols -> $c {
                if %DIRECTIONS{@maze[$r][$c]}:exists {
                    $guard-pos = [$r, $c];
                    $guard-dir = @maze[$r][$c];
                    @maze[$r][$c] = '.'; # Clear the starting position
                    last outer;
                }
            }
        }

        unless $guard-pos {
            say "No guard found in the maze.";
            exit;
        }

        # Track the positions visited
        my %visited-positions;
        %visited-positions{$guard-pos} = True;

        # Simulate the guard's movement
        loop {
            my $next-r = $guard-pos[0] + %DIRECTIONS{$guard-dir}[0];
            my $next-c = $guard-pos[1] + %DIRECTIONS{$guard-dir}[1];

            # Guard leaves the mapped area
            if $next-r < 0 || $next-r >= $rows || $next-c < 0 || $next-c >= $cols {
                last;
            }

            if @maze[$next-r][$next-c] eq '#' { # Obstacle ahead, turn right
                $guard-dir = %TURN_RIGHT{$guard-dir};
            } else { # Move forward
                $guard-pos = [$next-r, $next-c];
                %visited-positions{$guard-pos} = True;
            }
        }

        # End timing
        my $end-time = now;

        # Calculate distinct positions visited
        my $distinct-positions = %visited-positions.keys.elems;

        # Calculate elapsed time in seconds with precision
        my $elapsed-time = $end-time - $start-time;
        my $formatted-time = sprintf("%.9f", $elapsed-time).subst(',', ' ');

        # Print the header row
        say "answer, time";

        # Print the result in the desired format
        say "$distinct-positions, $formatted-time";
    }
}

Part001.main();
