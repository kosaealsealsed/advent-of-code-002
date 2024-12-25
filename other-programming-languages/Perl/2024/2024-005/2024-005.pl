use strict;
use warnings;
use List::Util qw(sum);

# Reading the file content
my $file_path = 'input.txt';
open(my $fh, '<', $file_path) or die "Cannot open file: $!";
my $content = do { local $/; <$fh> };
close($fh);

# Splitting content into rules and updates
my ($rules_section, $updates_section) = split(/\n\n/, $content);

my @rules = map { [split(/\|/, $_)] } split(/\n/, $rules_section);
@rules = map { [int($_->[0]), int($_->[1])] } @rules;

my @updates = map { [split(/,/, $_)] } split(/\n/, $updates_section);
@updates = map { [map { int($_) } @$_] } @updates;

# Helper function to check if an update follows the rules
sub is_update_ordered {
    my ($update, $rules) = @_;
    my %index_map = map { $update->[$_] => $_ } 0..$#$update;

    foreach my $rule (@$rules) {
        my ($x, $y) = @$rule;
        if (exists $index_map{$x} && exists $index_map{$y} && $index_map{$x} > $index_map{$y}) {
            return 0;
        }
    }
    return 1;
}

# Identify correctly ordered updates and their middle page numbers
my @correct_updates;
my @middle_pages;

foreach my $update (@updates) {
    if (is_update_ordered($update, \@rules)) {
        push @correct_updates, $update;
        push @middle_pages, $update->[int(@$update / 2)];
    }
}

# Calculate the sum of middle pages
my $sum_middle_pages = sum(@middle_pages);
print "Sum of middle pages for correctly ordered updates: $sum_middle_pages\n";

# Helper function to sort an update according to the rules
sub topological_sort {
    my ($update, $rules) = @_;
    my %graph;
    my %in_degree;
    my %nodes = map { $_ => 1 } @$update;

    foreach my $rule (@$rules) {
        my ($x, $y) = @$rule;
        if (exists $nodes{$x} && exists $nodes{$y}) {
            push @{$graph{$x}}, $y;
            $in_degree{$y}++;
            $in_degree{$x} //= 0;
        }
    }

    my @queue = grep { $in_degree{$_} == 0 } keys %nodes;
    my @sorted_update;

    while (@queue) {
        my $current = shift @queue;
        push @sorted_update, $current;

        foreach my $neighbor (@{$graph{$current} || []}) {
            $in_degree{$neighbor}--;
            if ($in_degree{$neighbor} == 0) {
                push @queue, $neighbor;
            }
        }
    }

    return \@sorted_update;
}

# Correct the incorrectly ordered updates and find their middle page numbers
my @incorrect_updates;
my @incorrect_middle_pages;

foreach my $update (@updates) {
    unless (is_update_ordered($update, \@rules)) {
        my $corrected_update = topological_sort($update, \@rules);
        push @incorrect_updates, $corrected_update;
        push @incorrect_middle_pages, $corrected_update->[int(@$corrected_update / 2)];
    }
}

# Calculate the sum of middle pages for corrected updates
my $sum_incorrect_middle_pages = sum(@incorrect_middle_pages);
print "Sum of middle pages for corrected updates: $sum_incorrect_middle_pages\n";
