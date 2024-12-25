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

# Create a frequency hash (dictionary) for the right list
my %right_list_counts;
foreach my $num (@right_list) {
    $right_list_counts{$num}++;
}

# Calculate the similarity score
my $similarity_score = 0;
foreach my $left (@left_list) {
    my $count = $right_list_counts{$left} // 0;  # Default to 0 if not found
    $similarity_score += $left * $count;
}

# Print the result
print "Similarity Score: $similarity_score\n";
