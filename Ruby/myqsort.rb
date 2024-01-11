
=begin

Directions: 
	Run the file with the command `ruby myqsort.rb` to run a quicksort with a
	default array.
	Optionally, run the file with `ruby myqsort.rb <array>`, where <array> contains
	a list of comma-separated values with no spaces. EXAMPLE: `ruby myqsort.rb 1,4,2`

	To run the code in an interactive ruby environment, enter the following:
	
		irb
		load 'myqsort <array>'

	with <array> being the same as above.

Description:
	The program first checks if an arg was given in the CLI. If so, the `input` 
	array is assigned the entries in the arg, with each element delimited by a comma.

	The program divides `input` around a pivot point at the 0th element. All 
	elements <= the pivot are grouped into the array `low`, and elements > pivot
	grouped into the array `high`. The program then recurses on low and high, 
	inserting the pivot between the returns from the recursive calls.  
=end

def myqsort(arr)
	# If arr is < 2 elements, it is sorted
	return arr if arr.length <= 1

	# Set the pivot for the sort
	pivot = arr[0]

	# Split the list into two subarrays, one less than pivot and one greater
	low = arr[1..-1].select {|x| x <= pivot}
	high = arr[1..-1].select {|x| x > pivot}

	# Recursively sort around the pivot, LHS is sorted as less than
	# the pivot and RHS is greater than the pivot.
	# Subarrays will be sorted on return by virtue of the pivot being inserted
	# between the result of both recursive calls.
	myqsort(low) + [pivot] + myqsort(high)
end

# Initialize an array to accept command line args
input = Array.new

## Begin command line argument check for sorting			
if ARGV.length > 1
	puts "\nFound #{ARGV.length} args - please use only one arg!\n"
	puts "Arrays must be comma separated with no spaces as such: \"1,4,2\"\n\n"

# If only one arg passed, split the array elements
elsif ARGV.length == 1
		input = ARGV[0]
		input = input.split(',')
		puts "\nUsing input: #{input.join(', ')}\n" 
else		
	puts "\nNo argument given!\n\n"
end

# If input was not set by an arg, initialize it with default values
if input.length == 0
	input = [5,1,78,6,3,-1,8,1,-2,-5,1,2,82,-98,0,1,3]
	puts "Using default array: #{input.join(', ')}\n"
end
## End check for command line args

# Begin quicksort function call
sorted = myqsort(input)

# Print the sorted array, comma-separated
puts "Sorted Array: #{sorted.join(', ')}\n\n"
