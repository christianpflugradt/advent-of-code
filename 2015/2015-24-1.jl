input = [
1
3
5
11
13
17
19
23
29
31
41
43
47
53
59
61
67
71
73
79
83
89
97
101
103
107
109
113
]

max_packages = 6
available = reverse(input)
weight = sum(input)/3

function get_packages(added)
	new = []
	for package in available
		for group in added
			if length(group) < max_packages && (sum(group) + package) <= weight && package < minimum(group)
				for subgroup in get_packages([[package, group...]])
					push!(new, subgroup)
				end
			end
		end
	end
	for list in new
		push!(added, list)
	end
	return added
end

function solve(input)
	candidates = filter(x -> sum(x) == weight, get_packages(map(x -> [x], available)))
	min_size = minimum(map(x -> length(x), candidates))
	return minimum(map(x -> reduce(*, x, init=1), filter(y -> length(y) == min_size, candidates)))
end

output = solve(input)

println(output)
