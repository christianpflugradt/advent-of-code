input = (2947, 3029)

function solve(input)
    calculations = reduce((a, x) -> x + a, input[2]:(input[2] + input[1] - 2), init = sum(1:input[2]))
    return reduce((a, x) -> (a * 252533) % 33554393, 2:calculations, init=20151125)
end

output = solve(input)

println(output)
