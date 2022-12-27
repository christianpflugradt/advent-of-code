import Pkg
Pkg.add("MD5")

using MD5

input = raw""

function solve(input)
    prefix = "iwrupvqb"
    number = 1
    while true
        hash = bytes2hex(md5("$prefix$number"))
        if startswith(hash, "00000")
            return number
        end
        if number % 10000 == 0
            println("$number hashes evaluated...")
        end
        number += 1
    end
end

output = solve(input)

println(output)
