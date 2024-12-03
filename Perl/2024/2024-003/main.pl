use strict;
use warnings;

# Function to calculate the sum of all mul(X, Y) operations
sub sum_mul_operations {
    my ($corrupted_memory) = @_;
    my $total = 0;

    # Regex pattern for mul(X, Y)
    while ($corrupted_memory =~ /mul\((\d{1,3}),(\d{1,3})\)/g) {
        my ($num1, $num2) = ($1, $2);
        $total += $num1 * $num2;
    }

    return $total;
}

# Function to calculate the sum of enabled mul(X, Y) operations
sub sum_enabled_mul_operations {
    my ($corrupted_memory) = @_;
    my $total_sum = 0;
    my $mul_enabled = 1;  # mul instructions are enabled at the start

    # Regex pattern for do(), don't(), and mul(X, Y)
    while ($corrupted_memory =~ /(do\(\)|don't\(\)|mul\((\d{1,3}),(\d{1,3})\))/g) {
        my $full_match = $1;

        if ($full_match eq "do()") {
            $mul_enabled = 1;
        } elsif ($full_match eq "don't()") {
            $mul_enabled = 0;
        } elsif (defined $2 && defined $3 && $mul_enabled) {
            my ($num1, $num2) = ($2, $3);
            $total_sum += $num1 * $num2;
        }
    }

    return $total_sum;
}

# Main execution
my $file_path = "input.txt";

# Read the corrupted memory from the file
open my $fh, '<', $file_path or die "Could not open file '$file_path': $!";
my $corrupted_memory = do { local $/; <$fh> };
close $fh;

# Calculate the results
my $total_sum_all_mul_operations = sum_mul_operations($corrupted_memory);
my $total_sum_enabled_mul_operations = sum_enabled_mul_operations($corrupted_memory);

# Output the results
print "The sum of all mul operations is: $total_sum_all_mul_operations\n";
print "The sum of enabled mul operations is: $total_sum_enabled_mul_operations\n";
