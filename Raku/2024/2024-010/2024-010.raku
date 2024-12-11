use Timer;

my $INPUT_FILE = "input.txt";

# Reads the input file and returns a grid as a 2D array
sub read-map($filename) {
    my @grid;
    for $filename.IO.lines -> $line {
        @grid.push: $line.comb.map(*.Int);
    }
    if @grid.elems == 0 || @grid[0].elems == 0 {
        die "Input grid is empty or improperly formatted";
    }
    return @grid;
}

# Returns a list of valid neighbors for a given cell
sub neighbors($r, $c, $rows, $cols) {
    my @directions = ([ -1, 0 ], [ 1, 0 ], [ 0, -1 ], [ 0, 1 ]);
    @directions.grep({
        my ($dr, $dc) = @($_);
        ($r + $dr) >= 0 && ($r + $dr) < $rows &&
        ($c + $dc) >= 0 && ($c + $dc) < $cols
    }).map({ $r + .[0], $c + .[1] });
}

# Calculates the trailhead scores
sub find-trailhead-scores(@grid) {
    my $rows = @grid.elems;
    my $cols = @grid[0].elems;

    my @trailheads = gather for ^$rows -> $r {
        for ^$cols -> $c {
            take [$r, $c] if @grid[$r][$c] == 0;
        }
    };

    if @trailheads.elems == 0 {
        die "No trailheads (height 0 cells) found in the grid";
    }

    my $total-score = 0;

    for @trailheads -> [$start-r, $start-c] {
        my %visited;
        my @queue = ([ $start-r, $start-c ]);
        %visited{"$start-r,$start-c"} = True;

        my %reachable-nines;

        while @queue {
            my ($r, $c) = @queue.shift;
            my $current-height = @grid[$r][$c];

            if $current-height == 9 {
                %reachable-nines{"$r,$c"} = True;
            } else {
                my $next-height = $current-height + 1;
                for neighbors($r, $c, $rows, $cols) -> ($nr, $nc) {
                    if !%visited{"$nr,$nc"} && @grid[$nr][$nc] == $next-height {
                        %visited{"$nr,$nc"} = True;
                        @queue.push: [ $nr, $nc ];
                    }
                }
            }
        }

        $total-score += %reachable-nines.keys.elems;
    }

    return $total-score;
}

# Recursive function to count paths from a cell to a height-9 cell
sub count-paths($r, $c, @grid, %dp, $rows, $cols) {
    return %dp{"$r,$c"} if %dp{"$r,$c"}:exists;

    my $current-height = @grid[$r][$c];

    if $current-height == 9 {
        %dp{"$r,$c"} = 1;
        return 1;
    }

    my $total-paths = 0;
    my $next-height = $current-height + 1;

    for neighbors($r, $c, $rows, $cols) -> ($nr, $nc) {
        if @grid[$nr][$nc] == $next-height {
            $total-paths += count-paths($nr, $nc, @grid, %dp, $rows, $cols);
        }
    }

    %dp{"$r,$c"} = $total-paths;
    return $total-paths;
}

# Calculates the total rating of all paths starting from trailheads
sub calculate-total-rating(@grid) {
    my $rows = @grid.elems;
    my $cols = @grid[0].elems;

    my @trailheads = gather for ^$rows -> $r {
        for ^$cols -> $c {
            take [$r, $c] if @grid[$r][$c] == 0;
        }
    };

    my %dp;
    my $total-rating = 0;

    for @trailheads -> [$tr, $tc] {
        $total-rating += count-paths($tr, $tc, @grid, %dp, $rows, $cols);
    }

    return $total-rating;
}

# Main program
sub MAIN() {
    my $start-part1 = now;
    my @grid = read-map($INPUT_FILE);
    my $total-score = find-trailhead-scores(@grid);
    my $elapsed-part1 = now - $start-part1;

    say "Part 1 Result: $total-score";
    say sprintf("Time taken for Part 1: %.9f s", $elapsed-part1);

    my $start-part2 = now;
    my $total-rating = calculate-total-rating(@grid);
    my $elapsed-part2 = now - $start-part2;

    say "Part 2 Result: $total-rating";
    say sprintf("Time taken for Part 2: %.9f s", $elapsed-part2);
}

MAIN();
