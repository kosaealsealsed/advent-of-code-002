# This program processes an input file to identify rules and updates.
# It determines which updates are correctly ordered based on the rules
# and calculates the sum of the middle page numbers of correctly ordered updates.

# Step 1: Reading the file content
$filePath = "input.txt"  # File containing the rules and updates
$content = Get-Content -Path $filePath -Raw

# Step 2: Splitting the content into rules and updates sections
$sections = $content.Trim() -split "`r`n`r`n"  # Split by blank line
$rulesSection = $sections[0]  # The first part contains the rules
$updatesSection = $sections[1]  # The second part contains the updates

# Step 3: Parsing the rules section into a list of tuples
# Initialize an empty array to store rules
$rules = @()

# Split the rules section into individual lines
$ruleLines = $rulesSection -split "`r`n"

# Process each rule line
foreach ($line in $ruleLines) {
    # Split the line into two parts using a space delimiter
    $ruleParts = $line -split " "
    
    # Convert the parts into integers
    $x = [int]$ruleParts[0]
    $y = [int]$ruleParts[1]
    
    # Add the tuple of integers to the rules array
    $rules += ,@($x, $y)
}

# Step 4: Parsing the updates section into a list of lists
# Initialize an empty array to store updates
$updates = @()

# Split the updates section into individual lines
$updateLines = $updatesSection -split "`r`n"

# Process each update line
foreach ($line in $updateLines) {
    # Split the line into individual numbers using the "," delimiter
    $updateParts = $line -split ","
    
    # Convert each part into an integer and add it to the updates array
    $update = $updateParts | ForEach-Object { [int]$_ }
    $updates += ,$update
}

# Step 5: Define a helper function to check if an update follows the rules
function Is-UpdateOrdered {
    param (
        [array]$update,
        [array]$rules
    )
    
    # Create a mapping of page numbers to their indices
    $indexMap = @{}
    
    # Populate the index map with the positions of each page in the update
    for ($i = 0; $i -lt $update.Count; $i++) {
        $page = $update[$i]
        $indexMap[$page] = $i
    }

    # Check each rule to ensure it is respected
    foreach ($rule in $rules) {
        $x = $rule[0]
        $y = $rule[1]
        
        if ($indexMap.ContainsKey($x) -and $indexMap.ContainsKey($y)) {
            $xIndex = $indexMap[$x]
            $yIndex = $indexMap[$y]
            
            # If x comes after y, the rule is violated
            if ($xIndex -gt $yIndex) {
                return $false
            }
        }
    }
    
    return $true
}

# Step 6: Identify correctly ordered updates and calculate their middle page numbers
# Initialize arrays for storing results
$correctUpdates = @()
$middlePages = @()

# Process each update
foreach ($update in $updates) {
    # Check if the update is correctly ordered
    if (Is-UpdateOrdered -update $update -rules $rules) {
        # Add the update to the list of correct updates
        $correctUpdates += ,$update
        
        # Find the middle page of the update
        $middleIndex = [math]::Floor($update.Count / 2)
        $middlePage = $update[$middleIndex]
        
        # Add the middle page to the list of middle pages
        $middlePages += $middlePage
    }
}

# Step 7: Calculate the sum of middle page numbers
$sumMiddlePages = ($middlePages | Measure-Object -Sum).Sum

# Step 8: Output the result
Write-Output "Sum of middle pages of correctly ordered updates: $sumMiddlePages"
