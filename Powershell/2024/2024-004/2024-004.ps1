# Define the file path
$filePath = "input.txt"

# Define the target word
$targetWord = "XMAS"

# Read the file and store the grid
$grid = Get-Content -Path $filePath

# Define the grid dimensions
$rows = $grid.Count
$cols = $grid[0].Length

# Define directions for moving in the grid
$directions = @(
    @{dx=0; dy=1},   # Right
    @{dx=1; dy=0},   # Down
    @{dx=1; dy=1},   # Diagonal-right-down
    @{dx=1; dy=-1},  # Diagonal-left-down
    @{dx=0; dy=-1},  # Left
    @{dx=-1; dy=0},  # Up
    @{dx=-1; dy=-1}, # Diagonal-left-up
    @{dx=-1; dy=1}   # Diagonal-right-up
)

# Function to check if a word exists in a given direction
function Check-Word {
    param (
        [int]$x,
        [int]$y,
        [int]$dx,
        [int]$dy
    )
    for ($i = 0; $i -lt $targetWord.Length; $i++) {
        $nx = $x + $i * $dx
        $ny = $y + $i * $dy
        if ($nx -lt 0 -or $ny -lt 0 -or $nx -ge $rows -or $ny -ge $cols -or $grid[$nx][$ny] -ne $targetWord[$i]) {
            return $false
        }
    }
    return $true
}

# Count all occurrences of the target word
$count = 0
for ($r = 0; $r -lt $rows; $r++) {
    for ($c = 0; $c -lt $cols; $c++) {
        foreach ($direction in $directions) {
            if (Check-Word -x $r -y $c -dx $direction.dx -dy $direction.dy) {
                $count++
            }
        }
    }
}

Write-Host "Count is $count."

# Function to count all X-MAS patterns
function Count-All-XMAS-Patterns {
    $count = 0
    for ($r = 1; $r -lt $rows - 1; $r++) {
        for ($c = 1; $c -lt $cols - 1; $c++) {
            $center = $grid[$r][$c]
            $topLeft = $grid[$r - 1][$c - 1]
            $topRight = $grid[$r - 1][$c + 1]
            $bottomLeft = $grid[$r + 1][$c - 1]
            $bottomRight = $grid[$r + 1][$c + 1]

            if ($center -eq "A") {
                # Check all valid X-MAS configurations
                if (
                    ($topLeft -eq "M" -and $topRight -eq "S" -and $bottomLeft -eq "M" -and $bottomRight -eq "S") -or
                    ($topLeft -eq "S" -and $topRight -eq "M" -and $bottomLeft -eq "S" -and $bottomRight -eq "M") -or
                    ($topLeft -eq "M" -and $topRight -eq "M" -and $bottomLeft -eq "S" -and $bottomRight -eq "S") -or
                    ($topLeft -eq "S" -and $topRight -eq "S" -and $bottomLeft -eq "M" -and $bottomRight -eq "M")
                ) {
                    $count++
                }
            }
        }
    }
    return $count
}

# Count the X-MAS patterns in the grid
$totalXmasPatterns = Count-All-XMAS-Patterns
Write-Host "Total X-MAS patterns is $totalXmasPatterns"
