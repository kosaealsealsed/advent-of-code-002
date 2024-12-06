# Read the file
my $file-path = "/uploads/input.txt";
my @grid = $file-path.IO.lines.map: *.trim;

# Define the target word
my $target-word = "XMAS";

# Define the grid dimensions
my $rows = @grid.elems;
my $cols = @grid[0].chars;

# Define the directions for moving in the grid
my @directions = (
    [ 0,  1],  # Right
    [ 1,  0],  # Down
    [ 1,  1],  # Diagonal-right-down
    [ 1, -1],  # Diagonal-left-down
    [ 0, -1],  # Left
    [-1,  0],  # Up
    [-1, -1],  # Diagonal-left-up
    [-1,  1],  # Diagonal-right-up
);

# Function to check if a word exists in a given direction
sub check-word($x, $y, $dx, $dy) {
    for 0..^$target-word.chars -> $i {
        my $nx = $x + $i * $dx;
        my $ny = $y + $i * $dy;
        return False if $nx < 0 || $ny < 0 || $nx >= $rows || $ny >= $cols;
        return False if @grid[$nx].substr($ny, 1) ne $target-word.substr($i, 1);
    }
    return True;
}

# Count all occurrences of the target word
my $count = 0;
for 0..^$rows -> $r {
    for 0..^$cols -> $c {
        for @directions -> $dir {
            $count++ if check-word($r, $c, |$dir);
        }
    }
}

say $count;
