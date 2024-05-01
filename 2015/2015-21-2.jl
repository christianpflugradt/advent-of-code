mutable struct Player
    health::Int
    damage::Int
    armor::Int
end

input = Player(104, 8, 1)

struct Weapon
    name::String
    cost::Int
    damage::Int
end

struct Armor
    name::String
    cost::Int
    armor::Int
end

struct Ring
    name::String
    cost::Int
    damage::Int
    armor::Int
end

weapons = [
    Weapon("Dagger", 8, 4),
    Weapon("Shortsword", 10, 5),
    Weapon("Warhammer", 25, 6),
    Weapon("Longsword", 40, 7),
    Weapon("Greataxe", 74, 8),
]

armors = [
    Armor("(none)", 0, 0),
    Armor("Leather", 13, 1),
    Armor("Chainmail", 31, 2),
    Armor("Splintmail", 53, 3),
    Armor("Bandedmail", 75, 4),
    Armor("Platemail", 102, 5),
]

rings = [
    Ring("(none 1)", 0, 0, 0),
    Ring("(none 2)", 0, 0, 0),
    Ring("Damage +1", 25, 1, 0),
    Ring("Damage +2", 50, 2, 0),
    Ring("Damage +3", 100, 3, 0),
    Ring("Defense +1", 20, 0, 1),
    Ring("Defense +2", 40, 0, 2),
    Ring("Defense +3", 80, 0, 3),
]

function wins(you, boss)
    while boss.health > 0 && you.health > 0
        boss.health -= max(you.damage - boss.armor, 1)
        if boss.health < 1
            return true
        end
        you.health -= max(boss.damage - you.armor, 1)
    end
    return false
end

function solve(input)
    costs = Set{Int}()
    for weapon in weapons, armor in armors
        for ring1 in rings
            for ring2 in rings
                if ring1 == ring2
                    continue
                end
                cost = weapon.cost + armor.cost + ring1.cost + ring2.cost
                you = Player(
                    100,
                    weapon.damage + ring1.damage + ring2.damage,
                    armor.armor + ring1.armor + ring2.armor
                )
                if !wins(you, Player(input.health, input.damage, input.armor))
                    push!(costs, cost)
                end
            end
        end
    end
    return maximum(costs)
end

output = solve(input)

println(output)
