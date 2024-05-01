mutable struct Player
    health::Int
    damage::Int
    armor::Int
    mana::Int
end

input = Player(51, 9, 0, 0)

function create_paths(spells, len)
    if len == 1
        return [[s] for s in spells]
    end
    paths = []
    for i in eachindex(spells)
        for path in create_paths(spells, len-1)
            spell = spells[i]
            if !(spell > 2 && (spell == path[1] || (length(path) > 1 && spell == path[2])))
                push!(paths, [spells[i], path...])
            end
        end
    end
    return paths
end

shield_turns = 0
poison_turns = 0
recharge_turns = 0

function apply_effects(you, boss)
    global shield_turns
    global poison_turns
    global recharge_turns
    if shield_turns > 0
        you.armor = 7
        shield_turns -= 1
    else
        you.armor = 0
    end
    if poison_turns > 0
        boss.health -= 3
        poison_turns -= 1
    end
    if recharge_turns > 0
        you.mana += 101
        recharge_turns -= 1
    end
end

function solve(input)
    global shield_turns
    global poison_turns
    global recharge_turns
    mana_costs = Set()
    max_turns = 1
    while isempty(mana_costs)
        max_turns += 1
        paths = create_paths([1, 2, 3, 4, 5], max_turns)
        for path in paths
            shield_turns = 0
            poison_turns = 0
            recharge_turns = 0
            boss = Player(input.health, input.damage, input.armor, input.mana)
            you = Player(50, 0, 0, 500)
            won = false
            total_mana_spent = 0
            for turn in path
                apply_effects(you, boss)
                if you.mana >= 0 && you.health >= 0 && boss.health < 1
                    won = true
                    break
                end
                mana_spent = 0
                if turn == 1
                    mana_spent = 53
                    boss.health -= 4
                elseif turn == 2
                    mana_spent = 73
                    you.health += 2
                    boss.health -= 2
                elseif turn == 3
                    if shield_turns > 0
                        break
                    else
                        shield_turns = 6
                    end
                    mana_spent = 113
                elseif turn == 4
                    if poison_turns > 0
                        break
                    else
                        poison_turns = 6
                    end
                    mana_spent = 173
                elseif turn == 5
                    if recharge_turns > 0
                        break
                    else
                        recharge_turns = 5
                    end
                    mana_spent = 229
                end
                you.mana -= mana_spent
                total_mana_spent += mana_spent
                if you.mana >= 0 && you.health >= 0 && boss.health < 1
                    won = true
                    break
                end
                apply_effects(you, boss)
                if you.mana >= 0 && you.health >= 0 && boss.health < 1
                    won = true
                    break
                end
                you.health -= max(boss.damage - you.armor, 1)
                if you.mana >= 0 && you.health >= 0 && boss.health < 1
                    won = true
                    break
                elseif you.health < 1 || you.mana < 1
                    break
                end
            end
            if won
                push!(mana_costs, total_mana_spent)
            end
        end
    end
    return minimum(mana_costs)
end

output = solve(input)

println(output)
