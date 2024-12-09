class CalibrationSolver {
    # TestCase class
    class TestCase {
        has Int $.target;
        has @.numbers;

        submethod BUILD(:$!target, :@!numbers) {}
    }

    # Evaluate expression left to right with + and *
    method evaluate-left-to-right(@numbers, @ops) {
        my $result = @numbers[0];
        for @ops.kv -> $i, $op {
            my $next-number = @numbers[$i + 1];
            given $op {
                when '+' { $result += $next-number }
                when '*' { $result *= $next-number }
                default { die "Unsupported operator: $op" }
            }
        }
        return $result;
    }

    # Evaluate expression left to right with +, *, and ||
    method evaluate-with-concat(@numbers, @ops) {
        my $result = @numbers[0];
        for @ops.kv -> $i, $op {
            my $next-number = @numbers[$i + 1];
            given $op {
                when '+' { $result += $next-number }
                when '*' { $result *= $next-number }
                when '||' { $result = ($result ~ $next-number).Int }
                default { die "Unsupported operator: $op" }
            }
        }
        return $result;
    }

    # Parse input file
    method parse-input(Str $file-path) {
        my @test-cases;
        for $file-path.IO.lines -> $line {
            next unless $line.contains(':');
            my ($target, $numbers-str) = $line.split(':');
            my $parsed-target = $target.trim.Int;
            my @numbers = $numbers-str.trim.split(/\s+/).map(*.Int);
            @test-cases.push: TestCase.new(target => $parsed-target, numbers => @numbers);
        }
        return @test-cases;
    }

    # Generate all possible operator combinations
    method generate-operator-combinations(@operators, Int $length) {
        return [] if $length == 0;
        my @sub-combinations = self.generate-operator-combinations(@operators, $length - 1);
        my @combinations;
        for @sub-combinations -> @sub {
            for @operators -> $op {
                @combinations.push: (@sub, $op);
            }
        }
        return @combinations;
    }

    # Solve Part One using only + and *
    method solve-part-one(@test-cases) {
        my @operators = <+ *>;
        my $valid-test-values-sum = 0;
        for @test-cases -> $test-case {
            my $possible = False;
            my $ops-length = $test-case.numbers.elems - 1;
            my @all-ops = self.generate-operator-combinations(@operators, $ops-length);
            for @all-ops -> @ops {
                my $result = self.evaluate-left-to-right($test-case.numbers, @ops);
                if $result == $test-case.target {
                    $possible = True;
                    last;
                }
            }
            $valid-test-values-sum += $test-case.target if $possible;
        }
        return $valid-test-values-sum;
    }

    # Solve Part Two using +, *, and || with progress reporting
    method solve-part-two(@test-cases, Num $part-one-time) {
        my @operators = <+ * ||>;
        my $valid-test-values-sum = 0;
        my $total-cases = @test-cases.elems;
        my $progress-interval = $total-cases div 10 || 1; # Handle small numbers

        my $cumulative1 = 0;
        my $cumulative2 = $part-one-time;

        say "\nProgress    Interval(s)         Cumulative1(s)    Cumulative2(s)";
        say "-" x 70;

        my $start-time = now;
        for @test-cases.kv -> $index, $test-case {
            my $possible = False;
            my $ops-length = $test-case.numbers.elems - 1;
            my @all-ops = self.generate-operator-combinations(@operators, $ops-length);
            for @all-ops -> @ops {
                my $result = self.evaluate-with-concat($test-case.numbers, @ops);
                if $result == $test-case.target {
                    $possible = True;
                    last;
                }
            }
            $valid-test-values-sum += $test-case.target if $possible;

            # Progress reporting
            if ($index + 1) % $progress-interval == 0 || $index + 1 == $total-cases {
                my $current-time = now;
                my $interval-elapsed = $current-time - $start-time;
                $cumulative1 += $interval-elapsed;
                $cumulative2 = $part-one-time + $cumulative1;

                printf "%-10s%-20.9f%-20.9f%-20.9f\n",
                    "{$index + 1}/{$total-cases}",
                    $interval-elapsed,
                    $cumulative1,
                    $cumulative2;

                $start-time = now; # Reset timer for next interval
            }
        }
        return ($valid-test-values-sum, $cumulative2);
    }
}

# Main
sub MAIN() {
    my $file-path = "input.txt";
    my $solver = CalibrationSolver.new();

    my @test-cases = $solver.parse-input($file-path);

    say "Starting part 001...";
    my $part-one-start-time = now;
    my $part-one-result = $solver.solve-part-one(@test-cases);
    my $part-one-duration = now - $part-one-start-time;
    my $part-one-time = $part-one-duration.seconds;  # Explicitly convert to Num
    printf "Part 1 finished in %.9f s\n", $part-one-time;
    say "Part 1 Total Calibration Result: $part-one-result";

    say "Starting part 002...";
    my ($part-two-result, $cumulative-time) = $solver.solve-part-two(@test-cases, $part-one-time);
    my $part-two-time = $cumulative-time - $part-one-time;
    printf "\nPart 2 finished in %.9f s, cumulative time %.9f s\n", $part-two-time, $cumulative-time;
    say "Part 2 Total Calibration Result: $part-two-result";
}

