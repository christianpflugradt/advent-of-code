input = raw"3113322113"

function solve(input)
	next_code = input
	buffer = IOBuffer()
	for i in 1:40
		prv = ""
		repetition = 1
		code = next_code
		truncate(buffer, 0)
		for chr in code
			if prv != ""
				if chr == prv
					repetition += 1
				else
					print(buffer, repetition, prv)
					repetition = 1
				end
			end
			prv = chr
		end
		print(buffer, repetition, prv)
		next_code = String(take!(buffer))
	end
	return length(next_code)
end

output = solve(input)

println(output)
