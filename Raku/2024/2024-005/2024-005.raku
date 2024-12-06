use v6;

# Read the file content
my $file-path = 'input.txt';
my $content = slurp $file-path;

# Split content into rules and updates
my ($rules-section, $updates-section) = $content.split("\n\n");

# Parse rules and updates
my @rules = $rules-section.lines.map({ .split('|').map(*.Int) });
my @updates = $updates-section.lines.map({ .split(',').map(*.Int) });

# Helper function to check if an update follows the rules
sub is-update-ordered(@update, @rules) {
    my %index-map = @update.kv.cache; # Cache the mapping to prevent re-consumption
    for @rules -> ($x, $y) {
        if (%index-map{$x}:exists and %index-map{$y}:exists and (%index-map{$x} > %index-map{$y})) {
            return False;
        }
    }
    return True;
}

# Identify correctly ordered updates and their middle page numbers
my @correct-updates;
my @middle-pages;

for @updates -> @update {
    if is-update-ordered(@update, @rules) {
        @correct-updates.push(@update);
        @middle-pages.push(@update[@update.elems div 2]);
    }
}

# Calculate the sum of middle pages
my $sum-middle-pages = @middle-pages.sum;
say "Sum of middle pages for correctly ordered updates: $sum-middle-pages";

# Helper function to sort an update according to the rules
sub topological-sort(@update, @rules) {
    my %graph;
    my %in-degree;
    my %nodes = @update.kv.cache.map({ $_ => True }); # Cache the mapping

    for @rules -> ($x, $y) {
        if %nodes{$x}:exists and %nodes{$y}:exists {
            %graph{$x}.push: $y;
            %in-degree{$y}++;
            %in-degree{$x} //= 0;
        }
    }

    my @queue = %nodes.keys.grep({ %in-degree{$_} == 0 });
    my @sorted-update;

    while @queue {
        my $current = @queue.shift;
        @sorted-update.push($current);
        for %graph{$current}.list -> $neighbor {
            %in-degree{$neighbor}--;
            if %in-degree{$neighbor} == 0 {
                @queue.push($neighbor);
            }
        }
    }

    return @sorted-update;
}

# Correct the incorrectly ordered updates and find their middle page numbers
my @incorrect-updates;
my @incorrect-middle-pages;

for @updates -> @update {
    unless is-update-ordered(@update, @rules) {
        my @corrected-update = topological-sort(@update, @rules);
        @incorrect-updates.push(@corrected-update);
        @incorrect-middle-pages.push(@corrected-update[@corrected-update.elems div 2]);
    }
}

# Calculate the sum of middle pages for corrected updates
my $sum-incorrect-middle-pages = @incorrect-middle-pages.sum;
say "Sum of middle pages for corrected updates: $sum-incorrect-middle-pages";
