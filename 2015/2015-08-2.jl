file = open("2015-08.txt", "r")
input = read(file, String)
close(file)

function solve(input)
	total_addition = 0
	for str in split(input, "\n")
		if length(str) > 1
			num_backslashes = count("\\", str)
			num_quotes = count("\"", str)
			total_addition += 2 + num_backslashes + num_quotes
		end
	end
	return total_addition
end

output = solve(input)

println(output)
