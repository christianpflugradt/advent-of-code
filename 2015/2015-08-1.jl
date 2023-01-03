file = open("2015-08.txt", "r")
input = read(file, String)
close(file)

function solve(input)
	total_reduction = 0
	for str in split(input, "\n")
		if length(str) > 1
			str_without_backslashes = replace(str, "\\\\" => "")
			num_backslashes = count("\\\\", str)
			num_quotes = count("\\\"", str_without_backslashes)
			num_unicodes = count("\\x", str_without_backslashes)
			total_reduction += 2 + num_backslashes + num_quotes + 3 * num_unicodes
		end
	end
	return total_reduction
end

output = solve(input)

println(output)
