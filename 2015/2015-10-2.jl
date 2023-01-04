input = raw"3113322113"

function solve(input)
	next_code = input
	for i in 1:50
		prv = ""
		repetition = 1
		code = next_code
		next_code = ""
		for chr in code
			if prv != ""
				if chr == prv
					repetition += 1
				else
					next_code = "$next_code$repetition$prv"
					repetition = 1
				end
			end
			prv = chr
		end
		next_code = "$next_code$repetition$prv"
	end
	return length(next_code)
end

output = solve(input)

println(output)
