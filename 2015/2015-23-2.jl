input = "jio a, +19
inc a
tpl a
inc a
tpl a
inc a
tpl a
tpl a
inc a
inc a
tpl a
tpl a
inc a
inc a
tpl a
inc a
inc a
tpl a
jmp +23
tpl a
tpl a
inc a
inc a
tpl a
inc a
inc a
tpl a
inc a
tpl a
inc a
tpl a
inc a
tpl a
inc a
inc a
tpl a
inc a
inc a
tpl a
tpl a
inc a
jio a, +8
inc b
jie a, +4
tpl a
inc a
jmp +2
hlf a
jmp -7"

even(num) = num % 2 == 0

function solve(input)
    inp = split(input, "\n")
    a = 1
    b = 0
    pointer = 1
    while pointer > 0 && pointer <= length(inp)
        regex = r"(hlf|tpl|inc|jmp|jie|jio) ([ab])?(, )?([+-])?([\d]+)?"
		m = match(regex, inp[pointer])
        if m[1] == "hlf"
            if m[2] == "a"
                a = floor(Int, a/2)
            else
                b = floor(Int, b/2)
            end
            pointer += 1
        elseif m[1] == "tpl"
            if m[2] == "a"
                a *= 3
            else
                b *= 3
            end
            pointer += 1
        elseif m[1] == "inc"
            if m[2] == "a"
                a += 1
            else
                b += 1
            end
            pointer += 1
        elseif m[1] == "jmp"
            if m[4] == "+"
                pointer += parse(Int, m[5])
            else
                pointer -= parse(Int, m[5])
            end
        elseif m[1] == "jie"
            if m[2] == "a"
                if even(a)
                    if m[4] == "+"
                        pointer += parse(Int, m[5])
                    else
                        pointer -= parse(Int, m[5])
                    end
                else
                    pointer += 1
                end
            else
                if even(b)
                    if m[4] == "+"
                        pointer += parse(Int, m[5])
                    else
                        pointer -= parse(Int, m[5])
                    end
                else
                    pointer += 1
                end
            end
        elseif m[1] == "jio"
            if m[2] == "a"
                if a == 1
                    if m[4] == "+"
                        pointer += parse(Int, m[5])
                    else
                        pointer -= parse(Int, m[5])
                    end
                else
                    pointer += 1
                end
            else
                if b == 1
                    if m[4] == "+"
                        pointer += parse(Int, m[5])
                    else
                        pointer -= parse(Int, m[5])
                    end
                else
                    pointer += 1
                end
            end
		end
    end
    return b
end

output = solve(input)

println(output)
