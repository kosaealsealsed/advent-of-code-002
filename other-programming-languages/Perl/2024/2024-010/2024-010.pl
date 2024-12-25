use strict;
use warnings;
use Time::HiRes qw(gettimeofday tv_interval);

my $INPUT_FILE = "input.txt";

sub read_map {
    my ($filename) = @_;
    open my $fh, '<', $filename or die "Cannot open file: $!";
    my @grid;
    while (my $line = <$fh>) {
        chomp $line;
        push @grid, [map { int($_) } split //, $line];
    }
    close $fh;
    return \@grid;
}

sub neighbors {
    my ($r, $c, $rows, $cols) = @_;
    my @directions = ([-1, 0], [1, 0], [0, -1], [0, 1]);
    my @result;
    for my $dir (@directions) {
        my ($dr, $dc) = @$dir;
        my $nr = $r + $dr;
        my $nc = $c + $dc;
        if ($nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols) {
            push @result, [$nr, $nc];
        }
    }
    return @result;
}

sub find_trailhead_scores {
    my ($grid) = @_;
    my $rows = @$grid;
    my $cols = @{$grid->[0]};

    my @trailheads;
    for my $r (0 .. $rows - 1) {
        for my $c (0 .. $cols - 1) {
            push @trailheads, [$r, $c] if $grid->[$r][$c] == 0;
        }
    }

    my $total_score = 0;

    for my $start (@trailheads) {
        my ($start_r, $start_c) = @$start;
        my %visited;
        my @queue = ([$start_r, $start_c]);
        $visited{"$start_r,$start_c"} = 1;

        my %reachable_nines;

        while (@queue) {
            my ($r, $c) = @{shift @queue};
            my $current_height = $grid->[$r][$c];

            if ($current_height == 9) {
                $reachable_nines{"$r,$c"} = 1;
            } else {
                my $next_height = $current_height + 1;
                for my $neighbor (neighbors($r, $c, $rows, $cols)) {
                    my ($nr, $nc) = @$neighbor;
                    if (!$visited{"$nr,$nc"} && $grid->[$nr][$nc] == $next_height) {
                        $visited{"$nr,$nc"} = 1;
                        push @queue, [$nr, $nc];
                    }
                }
            }
        }

        $total_score += keys %reachable_nines;
    }

    return $total_score;
}

sub count_paths {
    my ($r, $c, $grid, $dp, $rows, $cols) = @_;
    return $dp->[$r][$c] if defined $dp->[$r][$c];

    my $current_height = $grid->[$r][$c];

    if ($current_height == 9) {
        $dp->[$r][$c] = 1;
        return 1;
    }

    my $total_paths = 0;
    my $next_height = $current_height + 1;
    for my $neighbor (neighbors($r, $c, $rows, $cols)) {
        my ($nr, $nc) = @$neighbor;
        if ($grid->[$nr][$nc] == $next_height) {
            $total_paths += count_paths($nr, $nc, $grid, $dp, $rows, $cols);
        }
    }

    $dp->[$r][$c] = $total_paths;
    return $total_paths;
}

sub calculate_total_rating {
    my ($grid) = @_;
    my $rows = @$grid;
    my $cols = @{$grid->[0]};

    my @trailheads;
    for my $r (0 .. $rows - 1) {
        for my $c (0 .. $cols - 1) {
            push @trailheads, [$r, $c] if $grid->[$r][$c] == 0;
        }
    }

    my @dp = map { [(undef) x $cols] } 0 .. $rows - 1;

    my $total_rating = 0;
    for my $start (@trailheads) {
        my ($tr, $tc) = @$start;
        $total_rating += count_paths($tr, $tc, $grid, \@dp, $rows, $cols);
    }

    return $total_rating;
}

sub main {
    my $start_time_part1 = [gettimeofday];
    my $grid = read_map($INPUT_FILE);
    my $total_score = find_trailhead_scores($grid);
    my $elapsed_time_part1 = tv_interval($start_time_part1);

    print "Part 1 Result: $total_score\n";
    printf "Time taken for Part 1: %.9f s\n", $elapsed_time_part1;

    my $start_time_part2 = [gettimeofday];
    my $total_rating = calculate_total_rating($grid);
    my $elapsed_time_part2 = tv_interval($start_time_part2);

    print "Part 2 Result: $total_rating\n";
    printf "Time taken for Part 2: %.9f s\n", $elapsed_time_part2;
}

main();
