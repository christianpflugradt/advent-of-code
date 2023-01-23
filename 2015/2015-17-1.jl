input = [
11
30
47
31
32
36
3
1
5
3
32
36
15
11
46
26
28
1
19
3
]

function solve(input)
	sort!(input)
	minimum = 0
	maximum = 0
	for (i,v) in enumerate(input)
		maximum += v
		if maximum >= 150
			maximum = i
			break
		end
	end
	for (i,v) in enumerate(sort(input, rev=true))
		minimum += v
		if minimum >= 150
			minimum = i
			break
		end
	end
	count = 0
	for i in minimum:maximum
		for c in combinations(i, length(input))
			exactly = true
			total = 0
			for v in c
				total += input[v]
				if total > 150
					exactly = false
					break
				end
			end
			if exactly && total == 150
				count += 1
			end
		end
	end
	return count
end

function combinations(p, t)
	n = []
	m = []
	for i in 1:p
		push!(n, i)
		push!(m, t - p + i)
	end
	c = [copy(n)]
	while n != m
		for (i,j) in enumerate(p:-1:1)
			if n[j] < t + 1 - i
				n[j] += 1
				for (k,l) in enumerate(j+1:p)
					n[l] = n[j] + k
				end
				break
			end
		end 
		push!(c, copy(n))
	end
	return c
end


output = solve(input)

println(output)
