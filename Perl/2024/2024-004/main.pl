#!/usr/bin/perl
use strict;
use warnings;

# Function to read the grid from the input file
sub read_grid {
    my ($file_path) = @_;
    open my $fh, '<', $file_path or die "Error opening file: $!";
    my @grid = grep { /\S/ } map { chomp; $_ } <$fh>;
    close $fh;
    return @grid;
}

# Function to check if the word exists in a given direction
sub check_word {
    my ($grid, $x, $y, $dx, $dy, $word, $rows, $cols) = @_;
    my $word_len = length($word);

    for my $i (0 .. $word_len - 1) {
        my $nx = $x + $i * $dx;
        my $ny = $y + $i * $dy;

        return 0 if $nx < 0 || $ny < 0 || $nx >= $rows || $ny >= $cols;
        return 0 if substr($grid->[$nx], $ny, 1) ne substr($word, $i, 1);
    }
    return 1;
}

# Count occurrences of the word "XMAS" in all directions
sub count_xmas {
    my ($grid) = @_;
    my $word = "XMAS";
    my @directions = (
        [0, 1],   # Right
        [1, 0],   # Down
        [1, 1],   # Diagonal-right-down
        [1, -1],  # Diagonal-left-down
        [0, -1],  # Left
        [-1, 0],  # Up
        [-1, -1], # Diagonal-left-up
        [-1, 1]   # Diagonal-right-up
    );

    my $rows = scalar(@$grid);
    my $cols = length($grid->[0]);
    my $count = 0;

    for my $r (0 .. $rows - 1) {
        for my $c (0 .. $cols - 1) {
            for my $dir (@directions) {
                my ($dx, $dy) = @$dir;
                $count++ if check_word($grid, $r, $c, $dx, $dy, $word, $rows, $cols);
            }
        }
    }
    return $count;
}

# Count all X-MAS patterns
sub count_all_xmas_patterns {
    my ($grid) = @_;
    my $rows = scalar(@$grid);
    my $cols = length($grid->[0]);
    my $count = 0;

    for my $r (1 .. $rows - 2) {
        for my $c (1 .. $cols - 2) {
            my $center = substr($grid->[$r], $c, 1);
            my $top_left = substr($grid->[$r - 1], $c - 1, 1);
            my $top_right = substr($grid->[$r - 1], $c + 1, 1);
            my $bottom_left = substr($grid->[$r + 1], $c - 1, 1);
            my $bottom_right = substr($grid->[$r + 1], $c + 1, 1);

            if ($center eq 'A') {
                # Pattern 1: M.S
                $count++ if $top_left eq 'M' && $top_right eq 'S' && $bottom_left eq 'M' && $bottom_right eq 'S';
                # Pattern 2: S.M
                $count++ if $top_left eq 'S' && $top_right eq 'M' && $bottom_left eq 'S' && $bottom_right eq 'M';
                # Pattern 3: M.M
                $count++ if $top_left eq 'M' && $top_right eq 'M' && $bottom_left eq 'S' && $bottom_right eq 'S';
                # Pattern 4: S.S
                $count++ if $top_left eq 'S' && $top_right eq 'S' && $bottom_left eq 'M' && $bottom_right eq 'M';
            }
        }
    }
    return $count;
}

# Main function
sub main {
    my $file_path = "input.txt";
    my @grid = read_grid($file_path);

    if (!@grid) {
        print "Error: Grid is empty or invalid.\n";
        return;
    }

    my $xmas_count = count_xmas(\@grid);
    print "Count of XMAS: $xmas_count\n";

    my $xmas_patterns = count_all_xmas_patterns(\@grid);
    print "Total X-MAS patterns: $xmas_patterns\n";
}

# Run the main function
main();
