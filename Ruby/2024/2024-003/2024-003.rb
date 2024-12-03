def sum_mul_operations(corrupted_memory)
  # Regex pattern for mul(X,Y)
  pattern = /mul\((\d{1,3}),(\d{1,3})\)/
  total = 0

  # Find all matches and calculate the sum of products
  corrupted_memory.scan(pattern).each do |match|
    num1, num2 = match.map(&:to_i)
    total += num1 * num2
  end

  total
end

def sum_enabled_mul_operations(corrupted_memory)
  # Regex pattern for do(), don't(), and mul(X,Y)
  pattern = /(do\(\)|don't\(\)|mul\((\d{1,3}),(\d{1,3})\))/
  total_sum = 0
  mul_enabled = true # mul instructions are enabled at the start

  # Find all matches in the corrupted memory
  corrupted_memory.scan(pattern).each do |full_match, num1, num2|
    case full_match
    when "do()"
      mul_enabled = true
    when "don't()"
      mul_enabled = false
    else
      if mul_enabled && num1 && num2
        total_sum += num1.to_i * num2.to_i
      end
    end
  end

  total_sum
end

# Read the corrupted memory from 'input.txt'
file_path = '\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-003\\input.txt'
corrupted_memory = File.read(file_path)

# Calculate the results
total_sum_all_mul_operations = sum_mul_operations(corrupted_memory)
total_sum_enabled_mul_operations = sum_enabled_mul_operations(corrupted_memory)

# Output the results
puts "The sum of all mul operations is: #{total_sum_all_mul_operations}"
puts "The sum of enabled mul operations is: #{total_sum_enabled_mul_operations}"
