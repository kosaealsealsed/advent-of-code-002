#!/usr/bin/perl

use strict;
use warnings;
use Time::HiRes qw(gettimeofday tv_interval);
use List::Util qw(any);

# Direction mappings
my %DIRECTION_MAP = (
    '^' => 0,
    '>' => 1,
    'v' => 2,
    '<' => 3,
);

my @DIRECTION_OFFSETS = (
    [-1,  0],  # Up
    [ 0,  1],  # Right
    [ 1,  0],  # Down
    [ 0, -1],  # Left
);

# Entry point
sub main {
    my $file_path = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-006\\input.txt"; # Specify the path to your input file here
    eval {
        # Part 1: Count distinct positions visited without obstructions
        my $distinct_positions = count_distinct_positions_visited($file_path);
        print "Number of distinct positions visited: $distinct_positions\n";

        # Part 2: Detect loops with obstructions and measure execution times
        count_obstruction_positions($file_path);
    };
    if ($@) {
        print STDERR "Error: $@\n";
    }
}

# Part 1: Counts the number of distinct positions visited by the guard without any obstructions.
sub count_distinct_positions_visited {
    my ($file_path) = @_;

    # Parse the grid
    my $grid_ref = parse_grid($file_path);

    # Find the guard's starting position and direction
    my ($guard_pos, $guard_dir) = find_guard($grid_ref);

    # Initialize visited positions set
    my %visited_positions;
    $visited_positions{position_key($guard_pos)} = 1;

    # Simulate the guard's movement
    while (1) {
        my $dr = $DIRECTION_OFFSETS[$guard_dir][0];
        my $dc = $DIRECTION_OFFSETS[$guard_dir][1];
        my $new_r = $guard_pos->{row} + $dr;
        my $new_c = $guard_pos->{col} + $dc;

        # Check boundaries
        if ($new_r < 0 || $new_r >= @$grid_ref || $new_c < 0 || $new_c >= @{ $grid_ref->[0] }) {
            last; # Guard exits the mapped area
        }

        if ($grid_ref->[$new_r][$new_c] eq '#') {
            # Turn right if obstacle ahead
            $guard_dir = ($guard_dir + 1) % 4;
        } else {
            # Move forward
            $guard_pos = { row => $new_r, col => $new_c };
            $visited_positions{position_key($guard_pos)} = 1;
        }
    }

    # Number of distinct positions visited
    return scalar keys %visited_positions;
}

# Part 2: Counts the number of obstruction positions that cause the guard to loop indefinitely.
sub count_obstruction_positions {
    my ($file_path) = @_;

    # Start total timing
    my $total_start = [gettimeofday];

    # Parse the grid
    my $grid_ref = parse_grid($file_path);

    # Find the guard's starting position and direction
    my ($guard_pos, $guard_dir) = find_guard($grid_ref);

    # Time to find obstruction positions
    my $obstruction_start = [gettimeofday];
    my @possible_obstructions = get_possible_obstructions($grid_ref, $guard_pos);
    my $obstruction_end = [gettimeofday];
    my $obstruction_time = tv_interval($obstruction_start, $obstruction_end);

    # Print the first header and line: [time_obstruction_positions] [total_obstruction_positions]
    print "time, denominator\n";
    printf "%.9f %d\n", $obstruction_time, scalar @possible_obstructions;

    # Print header for batches
    print "batch, batch time, cumulative time\n";

    # Initialize loop counter
    my $loop_count = 0;
    my $total = scalar @possible_obstructions;

    # Initialize timing for batches
    my $batch_size = 1000;
    my $batch_start = [gettimeofday];
    my $cumulative_time = $obstruction_time; # cumulative_time includes obstruction_time

    for my $idx (0 .. $#possible_obstructions) {
        my $obstruction = $possible_obstructions[$idx];

        # Place obstruction
        $grid_ref->[$obstruction->{row}][$obstruction->{col}] = '#';

        # Simulate movement
        if (simulate_movement($grid_ref, $guard_pos, $guard_dir)) {
            $loop_count++; # Found a position that causes a loop
        }

        # Remove obstruction
        $grid_ref->[$obstruction->{row}][$obstruction->{col}] = '.';

        # Check if batch size is reached or it's the last position
        if ( (($idx + 1) % $batch_size == 0) || (($idx + 1) == $total) ) {
            my $batch_end = [gettimeofday];
            my $batch_time = tv_interval($batch_start, $batch_end);
            $cumulative_time += $batch_time;
            printf "%d %.9f %.9f\n", ($idx + 1), $batch_time, $cumulative_time;
            $batch_start = [gettimeofday]; # Reset batch start time
        }
    }

    # End total timing
    my $total_end = [gettimeofday];
    my $total_time = tv_interval($total_start, $total_end); # Total time from start to end

    # Print final answer header and line: [answer] [answer_time]
    print "answer, answer time\n";
    printf "%d %.9f\n", $loop_count, $total_time;
}

# Parses the grid from the given file.
sub parse_grid {
    my ($file_path) = @_;
    my @grid_list;

    open my $fh, '<', $file_path or die "Cannot open file '$file_path': $!";
    while (my $line = <$fh>) {
        chomp $line;
        $line =~ s/\s+//g; # Trim whitespace
        my @chars = split //, $line;
        push @grid_list, \@chars;
    }
    close $fh;

    if (!@grid_list) {
        die "The grid is empty.\n";
    }

    # Ensure all rows have the same length
    my $cols = scalar @{ $grid_list[0] };
    for my $row_ref (@grid_list) {
        if (scalar @$row_ref != $cols) {
            die "Inconsistent row lengths in the grid.\n";
        }
    }

    return \@grid_list;
}

# Finds the guard's starting position and direction.
sub find_guard {
    my ($grid_ref) = @_;

    for my $r (0 .. $#$grid_ref) {
        for my $c (0 .. $#{ $grid_ref->[0] }) {
            my $cell = $grid_ref->[$r][$c];
            if (exists $DIRECTION_MAP{$cell}) {
                my $guard_pos = { row => $r, col => $c };
                my $guard_dir = $DIRECTION_MAP{$cell};
                $grid_ref->[$r][$c] = '.'; # Clear the starting position
                return ($guard_pos, $guard_dir);
            }
        }
    }

    die "Guard not found in the grid.\n";
}

# Retrieves all possible obstruction positions excluding the guard's starting position and already obstructed cells.
sub get_possible_obstructions {
    my ($grid_ref, $guard_pos) = @_;
    my @possible;

    for my $r (0 .. $#$grid_ref) {
        for my $c (0 .. $#{ $grid_ref->[0] }) {
            if (!( ($r == $guard_pos->{row}) && ($c == $guard_pos->{col}) ) && ($grid_ref->[$r][$c] eq '.') ) {
                push @possible, { row => $r, col => $c };
            }
        }
    }

    return @possible;
}

# Simulates the guard's movement on the grid.
# Returns 1 if a loop is detected, 0 if the guard exits the grid.
sub simulate_movement {
    my ($grid_ref, $start_pos, $start_dir) = @_;
    my %visited_states;

    my $r = $start_pos->{row};
    my $c = $start_pos->{col};
    my $direction = $start_dir;

    while (1) {
        my $state_key = state_key($r, $c, $direction);
        if (exists $visited_states{$state_key}) {
            return 1; # Loop detected
        }
        $visited_states{$state_key} = 1;

        my $dr = $DIRECTION_OFFSETS[$direction][0];
        my $dc = $DIRECTION_OFFSETS[$direction][1];
        my $new_r = $r + $dr;
        my $new_c = $c + $dc;

        # Check boundaries
        if ($new_r < 0 || $new_r >= @$grid_ref || $new_c < 0 || $new_c >= @{ $grid_ref->[0] }) {
            return 0; # Guard exits the grid
        }

        if ($grid_ref->[$new_r][$new_c] eq '#') {
            # Turn right if obstacle ahead
            $direction = ($direction + 1) % 4;
        } else {
            # Move forward
            $r = $new_r;
            $c = $new_c;
        }
    }
}

# Helper function to create a unique key for a position
sub position_key {
    my ($pos) = @_;
    return "$pos->{row},$pos->{col}";
}

# Helper function to create a unique key for a state
sub state_key {
    my ($r, $c, $dir) = @_;
    return "$r,$c,$dir";
}

# Execute the main subroutine
main();
