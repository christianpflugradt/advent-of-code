import Pkg
Pkg.add("MD5")

using MD5

input = raw""

function solve(input)
    prefix = "iwrupvqb"
    number = 1
    while true
        hash = bytes2hex(md5("$prefix$number"))
        if startswith(hash, "000000")
            return number
        end
        number += 1
    end
end

output = solve(input)

println(output)
