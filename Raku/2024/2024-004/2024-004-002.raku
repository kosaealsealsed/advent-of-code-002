# Read the file
my $file-path = "/uploads/input.txt";
my @grid = $file-path.IO.lines.map: *.trim;

# Define the grid dimensions
my $rows = @grid.elems;
my $cols = @grid[0].chars;

# Function to count all X-MAS patterns
sub count-all-xmas-patterns {
    my $count = 0;

    # Traverse the grid, ensuring bounds for a 3x3 X-MAS pattern
    for 1..^($rows - 1) -> $r {
        for 1..^($cols - 1) -> $c {
            my $center       = @grid[$r].substr($c, 1);
            my $top-left     = @grid[$r - 1].substr($c - 1, 1);
            my $top-right    = @grid[$r - 1].substr($c + 1, 1);
            my $bottom-left  = @grid[$r + 1].substr($c - 1, 1);
            my $bottom-right = @grid[$r + 1].substr($c + 1, 1);

            # Check all valid X-MAS configurations
            if $center eq "A" {
                # Pattern 1: M.S
                if $top-left eq "M" && $top-right eq "S" && $bottom-left eq "M" && $bottom-right eq "S" {
                    $count++;
                }
                # Pattern 2: S.M
                elsif $top-left eq "S" && $top-right eq "M" && $bottom-left eq "S" && $bottom-right eq "M" {
                    $count++;
                }
                # Pattern 3: M.M
                elsif $top-left eq "M" && $top-right eq "M" && $bottom-left eq "S" && $bottom-right eq "S" {
                    $count++;
                }
                # Pattern 4: S.S
                elsif $top-left eq "S" && $top-right eq "S" && $bottom-left eq "M" && $bottom-right eq "M" {
                    $count++;
                }
            }
        }
    }

    return $count;
}

# Count the X-MAS patterns in the grid
my $total-xmas-patterns = count-all-xmas-patterns();
say $total-xmas-patterns;
