file = open("2015-12.txt", "r")
input = read(file, String)
close(file)

function solve(input)
	aggregation = 0
	buffer = ""
	for chr in input
		if is_number(chr)
			buffer = "$buffer$chr"
		elseif length(buffer) > 0
			aggregation += parse(Int, buffer)
			buffer = ""
		end
	end
	if length(buffer) > 0
		aggregation += parse(Int, buffer)
	end
	return aggregation
end

is_number(chr) = in(chr, "0123456789-")

output = solve(input)

println(output)
