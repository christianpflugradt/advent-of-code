input = raw"Faerun to Tristram = 65
Faerun to Tambi = 129
Faerun to Norrath = 144
Faerun to Snowdin = 71
Faerun to Straylight = 137
Faerun to AlphaCentauri = 3
Faerun to Arbre = 149
Tristram to Tambi = 63
Tristram to Norrath = 4
Tristram to Snowdin = 105
Tristram to Straylight = 125
Tristram to AlphaCentauri = 55
Tristram to Arbre = 14
Tambi to Norrath = 68
Tambi to Snowdin = 52
Tambi to Straylight = 65
Tambi to AlphaCentauri = 22
Tambi to Arbre = 143
Norrath to Snowdin = 8
Norrath to Straylight = 23
Norrath to AlphaCentauri = 136
Norrath to Arbre = 115
Snowdin to Straylight = 101
Snowdin to AlphaCentauri = 84
Snowdin to Arbre = 96
Straylight to AlphaCentauri = 107
Straylight to Arbre = 14
AlphaCentauri to Arbre = 46"

function solve(input)
	distance_match = r"^([A-Za-z]+) to ([A-Za-z]+) = ([\d]+)"
	towns_set = Set{String}()
	distances = Dict{String, Int16}()
	for distance in split(input, "\n")
		m = match(distance_match, distance)
		if m != nothing
			for from_to in [(m[1], m[2]), (m[2], m[1])]
				push!(towns_set, from_to[1])
				distances["$(from_to[1]) $(from_to[2])"] = parse(Int16, m[3])
			end
		else
			println("does not meet expectation: $distance")
		end
	end
	towns = collect(towns_set)
	max_distance = 0
	for num in 12345678:87654321
		perm = string(num)
		if has0(0, perm) && has0(9, perm)
			if has1(1, perm) && has1(2, perm) && has1(3, perm) && has1(4, perm)
				if has1(5, perm) && has1(6, perm) && has1(7, perm) && has1(8, perm)
					current_distance = 0
					prv = ""
					for chr in perm
						if prv != ""
							current_distance += distances["$(towns[parse(Int8, prv)]) $(towns[parse(Int8, chr)])"]
						end
						prv = chr
					end
					if current_distance > max_distance
						max_distance = current_distance
					end
				end
			end
		end
	end
	return max_distance
end

has1(num, str) = count(string(num), str) == 1
has0(num, str) = count(string(num), str) == 0

output = solve(input)

println(output)
