use strict;
use warnings;

# Define the input file path
my $file_path = "input.txt";

# Initialize arrays for the two lists
my (@left_list, @right_list);

# Read the file and extract the two columns
open my $fh, '<', $file_path or die "Could not open file '$file_path': $!";
while (my $line = <$fh>) {
    chomp $line;
    my @parts = split /\s+/, $line;
    if (@parts == 2) {
        push @left_list,  $parts[0];
        push @right_list, $parts[1];
    }
}
close $fh;

# Sort both lists
@left_list  = sort { $a <=> $b } @left_list;
@right_list = sort { $a <=> $b } @right_list;

# Calculate the total distance
my $total_distance = 0;
for my $i (0 .. $#left_list) {
    $total_distance += abs($left_list[$i] - $right_list[$i]);
}

# Print the result
print "Total Distance: $total_distance\n";
