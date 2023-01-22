input = raw"Rudolph can fly 22 km/s for 8 seconds, but then must rest for 165 seconds.
Cupid can fly 8 km/s for 17 seconds, but then must rest for 114 seconds.
Prancer can fly 18 km/s for 6 seconds, but then must rest for 103 seconds.
Donner can fly 25 km/s for 6 seconds, but then must rest for 145 seconds.
Dasher can fly 11 km/s for 12 seconds, but then must rest for 125 seconds.
Comet can fly 21 km/s for 6 seconds, but then must rest for 121 seconds.
Blitzen can fly 18 km/s for 3 seconds, but then must rest for 50 seconds.
Vixen can fly 20 km/s for 4 seconds, but then must rest for 75 seconds.
Dancer can fly 7 km/s for 20 seconds, but then must rest for 119 seconds."

function solve(input)
	total_time = 2503
	regex = r"[A-Za-z]+ can fly ([\d]+) km/s for ([\d]+) seconds, but then must rest for ([\d]+) seconds."
	steps_per_reindeer = []
	for line in split(input, "\n")
		m = match(regex, line)
		speed = parse(Int, m[1])
		travel_time = parse(Int, m[2])
		rest_time = parse(Int, m[3])
		steps = []
		for second in 1:total_time
			push!(steps, distance_at(second, speed, travel_time, rest_time))
		end
		push!(steps_per_reindeer, steps)
	end
	points = []
	for reindeer in 1:length(steps_per_reindeer)
		push!(points, 0)
	end
	for second in 1:total_time
		highest_value = 0
		highest_indices = []
		for (index, reindeer) in enumerate(steps_per_reindeer)
			if reindeer[second] > highest_value
				highest_value = reindeer[second]
				highest_indices = []
				push!(highest_indices, index)
			elseif reindeer[second] == highest_value
				push!(highest_indices, index)
			end
		end
		for index in highest_indices
			points[index] += 1
		end

	end
	return sort(points, rev=true)[1]
end

function distance_at(second, speed, travel_time, rest_time)
	cycle_time = travel_time + rest_time
	multiplicator = floor(second / cycle_time)
	remaining = second - (multiplicator * cycle_time)
	partial = 0
	if (remaining >= travel_time)
		multiplicator += 1
	else
		partial = remaining
	end
	distance = multiplicator * travel_time * speed
	distance += partial * speed
end

output = solve(input)

println(output)
