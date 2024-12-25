use v6;

class Part002 {

    # Parses the grid from the given file and returns it as a list of arrays.
    method parse-grid(Str $file-path) {
        my @grid;
        try {
            for $file-path.IO.lines -> $line {
                if $line.trim.chars > 0 {
                    @grid.push: $line.comb.Array; # Ensure non-empty lines
                }
            }
        }
        CATCH {
            default {
                die "Failed to read the grid: $_";
            }
        }
        if @grid.elems == 0 || @grid[0].elems == 0 {
            die "Grid is empty or malformed.";
        }
        return @grid;
    }

    # Finds the guard's starting position and initial direction.
    method find-guard(@grid) {
        my %direction-map = '^' => 0, '>' => 1, 'v' => 2, '<' => 3;
        for @grid.kv -> $r, @row {
            for @row.kv -> $c, $cell {
                if %direction-map{$cell}:exists {
                    say "Guard found at ($r, $c) facing {$cell}";
                    return { position => [$r, $c], direction => %direction-map{$cell} };
                }
            }
        }
        die "Guard not found in the grid.";
    }

    # Returns all positions where an obstruction can be placed.
    method get-possible-obstructions(@grid, @guard-pos) {
        my @possible;
        for @grid.kv -> $r, @row {
            for @row.kv -> $c, $cell {
                if [$r, $c] ne @guard-pos && $cell eq '.' {
                    @possible.push: [$r, $c];
                }
            }
        }
        return @possible;
    }

    # Simulates the guard's movement on the grid.
    method simulate-movement(@grid, @start-pos, $start-dir) {
        my %direction-offsets = 0 => [-1, 0], # Up
                                1 => [0, 1],  # Right
                                2 => [1, 0],  # Down
                                3 => [0, -1]; # Left

        my %visited-states;
        my ($r, $c) = @start-pos;
        my $direction = $start-dir;

        loop {
            my $state = "$r,$c,$direction";
            if %visited-states{$state}:exists {
                return True; # Loop detected
            }
            %visited-states{$state} = True;

            my ($dr, $dc) = %direction-offsets{$direction}.list;
            my $new-r = $r + $dr;
            my $new-c = $c + $dc;

            # Validate new indices
            if $new-r < 0 || $new-r >= @grid.elems || $new-c < 0 || $new-c >= @grid[0].elems {
                return False; # Guard exits the grid
            }

            my $next-cell = @grid[$new-r][$new-c] // Nil;
            if $next-cell ~~ Nil || $next-cell eq '#' {
                $direction = ($direction + 1) % 4; # Turn right
            } else {
                $r = $new-r;
                $c = $new-c; # Move forward
            }
        }
    }

    # Counts positions where an obstruction causes the guard to loop indefinitely.
    method count-obstruction-positions(Str $file-path) {
        my $total-start-time = now;

        # Parse the grid
        my @grid = self.parse-grid($file-path);
        say "Grid loaded successfully.";

        # Find the guard's starting position and direction
        my %guard = self.find-guard(@grid);
        my @guard-pos = %guard<position>;
        my $guard-dir = %guard<direction>;

        # Find possible obstruction positions
        my $obstruction-start-time = now;
        my @possible-obstructions = self.get-possible-obstructions(@grid, @guard-pos);
        my $obstruction-end-time = now;

        my $obstruction-time = $obstruction-end-time - $obstruction-start-time;
        say "time, denominator";
        say sprintf("%.9f %d", $obstruction-time, @possible-obstructions.elems);

        say "batch, batch time, cumulative time";

        # Batch processing
        my $loop-count = 0;
        my $total = @possible-obstructions.elems;
        my $batch-size = 1000;
        my $batch-start-time = now;
        my $cumulative-time = $obstruction-time;

        for @possible-obstructions.kv -> $idx, @obstruction {
            my ($r, $c) = @obstruction;

            if defined @grid[$r][$c] {
                @grid[$r][$c] = '#'; # Place obstruction

                if self.simulate-movement(@grid, @guard-pos, $guard-dir) {
                    $loop-count++;
                }

                @grid[$r][$c] = '.'; # Remove obstruction
            } else {
                warn "Accessing undefined cell at ($r, $c)";
            }

            # Print batch results
            if (($idx + 1) % $batch-size == 0 || $idx + 1 == $total) {
                my $batch-end-time = now;
                my $batch-time = $batch-end-time - $batch-start-time;
                $cumulative-time += $batch-time;
                say sprintf("%d %.9f %.9f", $idx + 1, $batch-time, $cumulative-time);
                $batch-start-time = now;
            }
        }

        my $total-end-time = now;
        my $total-time = $total-end-time - $total-start-time;

        say "answer, answer time";
        say sprintf("%d %.9f", $loop-count, $total-time);
    }

    # Entry point
    method main() {
        my $file-path = "C:\\advent-of-code-002\\input-files\\2024\\2024-006\\input.txt";
        try {
            self.count-obstruction-positions($file-path);
        }
        CATCH {
            default {
                die "Error: $_";
            }
        }
    }
}

# Run the program
Part002.main();
