input = raw"Frosting: capacity 4, durability -2, flavor 0, texture 0, calories 5
Candy: capacity 0, durability 5, flavor -1, texture 0, calories 8
Butterscotch: capacity -1, durability 0, flavor 5, texture 0, calories 6
Sugar: capacity 0, durability 0, flavor -2, texture 2, calories 1"

function solve(input)
	regex = r"[A-Za-z]+: capacity ([-\d]+), durability ([-\d]+), flavor ([-\d]+), texture ([-\d]+), calories ([\d]+)"
	stuff = []
	for line in split(input, "\n")
		m = match(regex, line)
		capacity = parse(Int, m[1])
		durability = parse(Int, m[2])
		flavor = parse(Int, m[3])
		texture = parse(Int, m[4])
		calories = parse(Int, m[5])
		push!(stuff, (capacity, durability, flavor, texture, calories))
	end
	max_value = 0
	for a in 1:97, b in 1:97, c in 1:97
		d = 100 - (a + b + c)
		if d < 1
			continue
		end
		total_calories = stuff[1][5] * a + stuff[2][5] * b + stuff[3][5] * c + stuff[4][5] * d
		if total_calories != 500
			continue
		end
		total_capacity = max(0, stuff[1][1] * a + stuff[2][1] * b + stuff[3][1] * c + stuff[4][1] * d)
		total_durability = max(0, stuff[1][2] * a + stuff[2][2] * b + stuff[3][2] * c + stuff[4][2] * d)
		total_flavor = max(0, stuff[1][3] * a + stuff[2][3] * b + stuff[3][3] * c + stuff[4][3] * d)
		total_texture = max(0, stuff[1][4] * a + stuff[2][4] * b + stuff[3][4] * c + stuff[4][4] * d)
		cookie_value = total_capacity * total_durability * total_flavor * total_texture
		max_value = max(max_value, cookie_value)
	end
	return max_value
end

output = solve(input)

println(output)
