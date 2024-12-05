# Read the input file content
my $file-path = 'input.txt';  
my $content = slurp $file-path;

# Split content into rules and updates sections
my @sections = $content.trim.split("\n\n");
die "Invalid file format: missing sections" if @sections.elems < 2;

my $rules-section = @sections[0];
my $updates-section = @sections[1];

# Parse rules and updates
my @rules = $rules-section.split("\n").map: {
    my ($x, $y) = .split("|").map(*.Int);
    ($x, $y)
};

my @updates = $updates-section.split("\n").map: {
    .split(",").map(*.Int)
};

# Functions
sub is-update-ordered(@update, @rules) {
    my %index-map = @update.kv.map: -> $index, $value { $value => $index };
    for @rules -> ($x, $y) {
        if %index-map{$x}:exists and %index-map{$y}:exists {
            return False if %index-map{$x} > %index-map{$y};
        }
    }
    return True;
}

sub topological-sort-update(@update, @rules) {
    my %graph;
    my %in-degree;
    for @update -> $node { %in-degree{$node} //= 0; }

    for @rules -> ($x, $y) {
        # Check if both $x and $y exist in @update
        if $x ∈ @update and $y ∈ @update {
            %graph{$x} //= [];
            %graph{$x}.push($y);
            %in-degree{$y}++;
        }
    }

    my @queue = %in-degree.keys.grep: { %in-degree{$_} == 0 };
    my @sorted-update;

    while @queue {
        my $current = @queue.shift;
        @sorted-update.push($current);

        my @neighbors = %graph{$current} // [];
        for @neighbors -> $neighbor {
            %in-degree{$neighbor}--;
            @queue.push($neighbor) if %in-degree{$neighbor} == 0;
        }
    }
    return @sorted-update;
}


# Process updates
my @correct-middle-pages;
my @incorrect-middle-pages;

for @updates -> @update {
    next unless @update;  # Skip empty updates
    if is-update-ordered(@update, @rules) {
        my $middle-index = @update.elems div 2;
        @correct-middle-pages.push(@update[$middle-index]);
    }
    else {
        my @corrected-update = topological-sort-update(@update, @rules);
        my $middle-index = @corrected-update.elems div 2;
        @incorrect-middle-pages.push(@corrected-update[$middle-index] // 0);
    }
}

# Calculate sums
my $sum-correct-middle-pages = [+] @correct-middle-pages;
my $sum-incorrect-middle-pages = [+] @incorrect-middle-pages;

# Output
say "Sum of middle pages of correctly ordered updates: $sum-correct-middle-pages";
say "Sum of middle pages of corrected updates: $sum-incorrect-middle-pages";
