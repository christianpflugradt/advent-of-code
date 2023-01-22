import Pkg
Pkg.add("JSON")

using JSON

input = JSON.parsefile("2015-12.txt")

function solve(input)
	return total_of(input)
end

function total_of(node)::Int
	if typeof(node) == Int
		return node
	elseif typeof(node) == String || ( string(typeof(node))[1:4] == "Dict" && in("red", values(node)) )
		return 0
	else
		total = 0
		for e in node
			total += total_of(e)
		end
		return total
	end
end

output = solve(input)

println(output)
