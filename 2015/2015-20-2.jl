input = floor(Int, 34_000_000 / 11)

function max_triangle_below_input()
    candidate = 1
    total = 1
    while total < input
        candidate += 1
        total += candidate
    end
    return candidate
end

function solve(input)
    number = max_triangle_below_input()
    presents = 0
    while presents < input
        number += 1
        presents = 1 + number
        for elv in 2:number-1
            if number % elv == 0 && number / elv <= 50
                presents += elv
            end
        end
        if number % 10000 == 0
            println(number, " ", presents)
        end
    end
    return number
end

output = solve(input)

println(output)
