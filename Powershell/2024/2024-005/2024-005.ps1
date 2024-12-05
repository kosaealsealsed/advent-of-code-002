# Define the file path
$file_path = 'input.txt'

# Read the file content
$content = Get-Content $file_path

# Split content into rules and updates
$content_str = $content -join "`n"
$sections = $content_str -split "`n`n"
$rules_section = $sections[0]
$updates_section = $sections[1]

# Process rules
$rules = $rules_section -split "`n"
$rules = $rules | ForEach-Object {
    $pages = $_ -split "\|"
    
    # Ensure that the rule splits correctly into two values
    if ($pages.Length -eq 2) {
        [int]$pages[0], [int]$pages[1]
    } else {
        Write-Warning "Skipping invalid rule: $_"
        $null # Skip invalid rule
    }
} | Where-Object { $_ -ne $null } # Remove any null (invalid) rules

# Process updates
$updates = $updates_section -split "`n"
$updates = $updates | ForEach-Object {
    $_ -split "," | ForEach-Object { [int]$_ }
}

# Helper function to check if an update follows the rules
function Is-UpdateOrdered {
    param (
        [array]$update,
        [array]$rules
    )
    
    $index_map = @{}
    for ($i = 0; $i -lt $update.Length; $i++) {
        $index_map[$update[$i]] = $i
    }

    foreach ($rule in $rules) {
        $x = $rule[0]
        $y = $rule[1]
        
        # Ensure $x and $y are valid before calling ContainsKey
        if ($x -ne $null -and $y -ne $null) {
            if ($index_map.ContainsKey($x) -and $index_map.ContainsKey($y)) {
                if ($index_map[$x] -gt $index_map[$y]) {
                    return $false
                }
            }
        } else {
            Write-Warning "Skipping invalid rule: x = $x, y = $y"
        }
    }

    return $true
}

# Identify correctly ordered updates and their middle page numbers
$correct_updates = @()
$middle_pages = @()

foreach ($update in $updates) {
    if (Is-UpdateOrdered -update $update -rules $rules) {
        $correct_updates += ,$update
        $middle_pages += $update[([math]::Ceiling($update.Length / 2)) - 1]
    }
}

# Calculate the sum of middle pages
$sum_middle_pages = ($middle_pages | Measure-Object -Sum).Sum
$sum_middle_pages
