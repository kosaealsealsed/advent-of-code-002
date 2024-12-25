use v6;

class AntinodeCalculator {
    # Reads the grid map from a file
    method read-map(Str $filename) {
        my @lines = $filename.IO.lines;
        return @lines;
    }

    # Computes unique antinode positions using the pairwise method
    method compute-antinodes-pairwise(@grid) {
        my $rows = @grid.elems;
        my $cols = $rows > 0 ?? @grid[0].chars !! 0;

        my %antennas-by-freq;
        for ^$rows -> $r {
            for ^$cols -> $c {
                my $ch = @grid[$r].substr($c, 1);
                next if $ch eq '.';
                %antennas-by-freq{$ch} //= [];
                %antennas-by-freq{$ch}.push([$r, $c]);
            }
        }

        my %antinodes;
        for %antennas-by-freq.kv -> $freq, @coords {
            if @coords.elems < 2 { next; }
            for @coords.kv -> $i, $A {
                for @coords[$i+1 .. *] -> $B {
                    my ($rA, $cA) = @$A;
                    my ($rB, $cB) = @$B;

                    # Compute P1 = 2B - A
                    my $p1-r = 2 * $rB - $rA;
                    my $p1-c = 2 * $cB - $cA;
                    if $p1-r >= 0 && $p1-r < $rows && $p1-c >= 0 && $p1-c < $cols {
                        %antinodes{"$p1-r,$p1-c"}++;
                    }

                    # Compute P2 = 2A - B
                    my $p2-r = 2 * $rA - $rB;
                    my $p2-c = 2 * $cA - $cB;
                    if $p2-r >= 0 && $p2-r < $rows && $p2-c >= 0 && $p2-c < $cols {
                        %antinodes{"$p2-r,$p2-c"}++;
                    }
                }
            }
        }

        return %antinodes.keys;
    }

    # Computes unique antinode positions using the line-drawing method
    method compute-antinodes-lines(@grid) {
        my $rows = @grid.elems;
        my $cols = $rows > 0 ?? @grid[0].chars !! 0;

        my %antennas-by-freq;
        for ^$rows -> $r {
            for ^$cols -> $c {
                my $ch = @grid[$r].substr($c, 1);
                next if $ch eq '.';
                %antennas-by-freq{$ch} //= [];
                %antennas-by-freq{$ch}.push([$r, $c]);
            }
        }

        my %antinodes;
        for %antennas-by-freq.kv -> $freq, @coords {
            if @coords.elems < 2 { next; }
            for @coords.kv -> $i, $A {
                for @coords[$i+1 .. *] -> $B {
                    my ($rA, $cA) = @$A;
                    my ($rB, $cB) = @$B;
                    self.add-line-points($rA, $cA, $rB, $cB, $rows, $cols, %antinodes);
                }
            }
        }

        return %antinodes.keys;
    }

    # Adds all points along a line between two antennas
    method add-line-points(Int $rA, Int $cA, Int $rB, Int $cB, Int $rows, Int $cols, %antinodes) {
        my $dr = $rB - $rA;
        my $dc = $cB - $cA;
        my $g = self.gcd($dr.abs, $dc.abs);
        $dr div= $g;
        $dc div= $g;

        my ($rP, $cP) = ($rA, $cA);
        while $rP >= 0 && $rP < $rows && $cP >= 0 && $cP < $cols {
            %antinodes{"$rP,$cP"}++;
            $rP += $dr;
            $cP += $dc;
        }

        ($rP, $cP) = ($rA - $dr, $cA - $dc);
        while $rP >= 0 && $rP < $rows && $cP >= 0 && $cP < $cols {
            %antinodes{"$rP,$cP"}++;
            $rP -= $dr;
            $cP -= $dc;
        }
    }

    # Computes the greatest common divisor of two integers
    method gcd(Int $a, Int $b) {
        return $b == 0 ?? $a !! self.gcd($b, $a % $b);
    }
}

# Main function
sub MAIN(Str $filename = "input.txt") {
    my $calculator = AntinodeCalculator.new();

    my @grid = $calculator.read-map($filename);

    my $start-time = now;
    my @pairwise-antinodes = $calculator.compute-antinodes-pairwise(@grid);
    my $pairwise-time = now - $start-time;
    say "Part 001 finished in {$pairwise-time}";
    say "Number of unique antinodes (Pairwise method): {@pairwise-antinodes.elems}";

    $start-time = now;
    my @line-antinodes = $calculator.compute-antinodes-lines(@grid);
    my $line-time = now - $start-time;
    say "Part 002 finished in {$line-time}";
    say "Number of unique antinodes (Line-drawing method): {@line-antinodes.elems}";

    my $overall-time = $pairwise-time + $line-time;
    say "Total computation time: {$overall-time}";
}
