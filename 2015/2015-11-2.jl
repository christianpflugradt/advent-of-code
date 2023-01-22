input = "vzbxxyzz"

function solve(input)
	next = next_str(input)
	while !has_two_pairs(next) || !has_sequence(next) || has_invalid_chars(next)
	       next = next_str(next)
	end
	return next
end

function next_str(str)
	chars = collect(str)
	pos = length(chars)
	while chars[pos] == 'z'
		pos -= 1
		if pos < 1
			return "$(repeat('a',length(str) + 1))"
		end
	end
	chars[pos] = Char(Int(chars[pos]) + 1)
	while is_invalid_char(chars[pos])
		chars[pos] = Char(Int(chars[pos]) + 1)
	end
	for post in pos+1:length(chars)
		chars[post] = 'a'
	end
	return join(chars)
end

function has_two_pairs(str)
	prv = ""
	pair = false
	just_found_a_pair = false
	for chr in str
		if just_found_a_pair
			just_found_a_pair = false
			prv = chr
			continue
		elseif chr == prv
			if pair == true
				return true
			else
				pair = true
				just_found_a_pair = true
			end
		end
		prv = chr
	end
	return false
end

function has_sequence(str)
	seq_count = 0
	prv_val = 0
	for chr in str
		cur_val = Int(chr)
		if cur_val == prv_val + 1
			seq_count += 1
		else
			seq_count = 0
		end
		if seq_count == 2
			return true
		end
		prv_val = cur_val
	end
	return false
end

has_invalid_chars(str) = in('i', str) || in('l', str) || in('o', str)

is_invalid_char(chr) = in(chr, "ilo")

output = solve(input)

println(output)
