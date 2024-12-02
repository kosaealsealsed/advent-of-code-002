# Function to check if a report is safe
def is_safe(report)
  differences = report.each_cons(2).map { |a, b| b - a }
  all_increasing = differences.all? { |diff| diff >= 1 && diff <= 3 }
  all_decreasing = differences.all? { |diff| diff <= -1 && diff >= -3 }
  all_increasing || all_decreasing
end

# Function to check if a report is safe with the Problem Dampener
def is_safe_with_dampener(report)
  return true if is_safe(report)

  report.each_index do |i|
    modified_report = report[0...i] + report[i+1..-1]
    return true if is_safe(modified_report)
  end

  false
end

# Main function to process the input and count safe reports
def main
  input_path = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-002\\input.txt"
  
  # Read and process the input file
  reports = File.readlines(input_path).map do |line|
    line.split.map(&:to_i)
  end

  # Count the safe reports
  safe_count = reports.count { |report| is_safe(report) }
  puts "Safe reports: #{safe_count}"

  # Count the safe reports with the Problem Dampener
  safe_with_dampener_count = reports.count { |report| is_safe_with_dampener(report) }
  puts "Safe reports with dampener: #{safe_with_dampener_count}"
end

# Run the main function
main
