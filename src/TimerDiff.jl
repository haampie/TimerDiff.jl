module TimerDiff

# TODO: make section variable, configure widths, etc.

import JSON
import DataStructures: OrderedDict
import Printf: @sprintf
import StatsBase: mean, median

const KEEP = 0x00
const DELETE = 0x01
const ADD = 0x02

"""
Get the edit distance between a and b

    diff_table("hello", "hallo") -> steps, costs

costs[i + 1, j + 1] is the cost to transform a[1:i] into b[1:j]
where insertion and deletion have a cost of one.

Further, if steps[i + 1, j + 1] == KEEP, a[i] and b[j] are equal
and this item should be retained. If steps[i + 1, j + 1] == DELETE,
a[i] has to be deleted. If steps[i + 1, j + 1] == ADD, then b[j] should
be added after to a. The set of actions to transform a into b can be
obtained from `backtrack`.
"""
function diff_table(a, b)
    na, nb = length(a), length(b)

    # for keeping track of the edit distance
    costs = zeros(Int, na + 1, nb + 1)
    costs[begin, :] .= 0:nb
    costs[:, begin] .= 0:na

    # for keeping track of where we came from
    steps = zeros(UInt8, na + 1, nb + 1)
    steps[begin, :] .= ADD
    steps[:, begin] .= DELETE

    for (j, bj) = enumerate(b), (i, ai) = enumerate(a)
        if ai == bj
            costs[i + 1, j + 1] = costs[i, j]
            steps[i + 1, j + 1] = KEEP
        elseif costs[i, j + 1] < costs[i + 1, j]
            costs[i + 1, j + 1] = costs[i, j + 1] + 1
            steps[i + 1, j + 1] = DELETE
        else
            costs[i + 1, j + 1] = costs[i + 1, j] + 1
            steps[i + 1, j + 1] = ADD
        end
    end

    return steps, costs
end

function backtrack(steps)
    path = UInt8[]

    i, j = size(steps)

    while i != 1 || j != 1
        if steps[i, j] == KEEP
            push!(path, KEEP)
            i -= 1
            j -= 1
        elseif steps[i, j] == DELETE
            push!(path, DELETE)
            i -= 1
        else
            push!(path, ADD)
            j -= 1
        end
    end

    reverse!(path)
end

"""
Get a vector of actions (KEEP, ADD, DELETE) to
transform a into b where the number of additions and
deletions is minimal.

    backtrack("hello", "whallo") -> [ADD, KEEP, DELETE, ADD, KEEP, KEEP, KEEP]

To work with this, keep two indices for a and b starting at 1,
whenever you hit KEEP: increment both, whenever you hit ADD,
you need to add b[j] and increment j. Whenever you hit DELETE,
you have to delete a[i] and increment i.
"""
backtrack(a, b) = backtrack(first(diff_table(a, b)))

function get_indent(done::Vector{Bool})
    indent = ""

    for idx = 1:length(done) - 1
        indent *= done[idx] ? "   " : "│  "
    end
    
    if !isempty(done)
        indent *= done[end] ? "└─ " : "├─ "
    end

    return indent
end

struct Stat
    count::Int
    total::Float64
    mean::Float64
    median::Float64
    min::Float64
    max::Float64
end

struct StatDiff
    count::Float64
    total::Float64
    mean::Float64
    median::Float64
    min::Float64
    max::Float64
end

Stat(item::OrderedDict) = Stat(item["timings"])

Stat(a::Vector) = Stat(
    length(a),
    sum(a),
    mean(a),
    median(a),
    minimum(a),
    maximum(a)
)

compare(a::Stat, b::Stat) = StatDiff(
    b.count / a.count,
    b.total / a.total,
    b.mean / a.mean,
    b.median / a.median,
    b.min / a.min,
    b.max / a.max
)

prettypercent(p) = string(@sprintf("%.2f", p * 100), "%")

function prettydiff(p)
    p == 1 && return ""
    diff = p - 1.0
    return string(diff >= 0.0 ? "(+" : "(", @sprintf("%.2f", diff * 100), "%)")
end

function prettytime(t)
    t *= 1e9

    if t < 1e3
        value, units = t, "ns"
    elseif t < 1e6
        value, units = t / 1e3, "μs"
    elseif t < 1e9
        value, units = t / 1e6, "ms"
    else
        value, units = t / 1e9, "s "
    end
    return string(@sprintf("%.2f", value), " ", units)
end

function prettycount(b)
    if b < 1000
        return string(b) * "  "
    elseif b < 1000^2
        value, units = b / 1000^1, "K"
    elseif b < 1000^3
        value, units = b / 1000^2, "M"
    else
        value, units = b / 1000^3, "G"
    end
    return string(@sprintf("%.2f", value), " ", units)
end

function print_indented(indent::Vector{Bool}, key; io = stdout)
    if length(indent) > 1
        for is_done in @view(indent[2:end-1])
            print(io, is_done ? "  " : "│ ")
        end

        print(io, indent[end] ? "└ " : "├ ")
    end

    print(io, rpad(key, 70 - 2 * length(indent)))
end

is_significant(ratio) = abs(ratio - 1) > 0.10

function default_on_keep(key, lhs, rhs, indent; io = stdout)
    stat_lhs, stat_rhs = Stat(lhs), Stat(rhs)
    ratio = compare(stat_lhs, stat_rhs)

    # add warnings if there's some significant change
    significant = is_significant(ratio.mean) || is_significant(ratio.count)
    print(io, significant ? "! " : "  ")
    print_indented(indent, key, io = io)
    print(io, "    ")
    print(io, lpad(lpad(prettytime(stat_lhs.mean), 9) * " → " * lpad(prettytime(stat_rhs.mean), 9), 21), " ", lpad(prettydiff(ratio.mean), 12))
    print(io, "    ")
    print(io, lpad(lpad(prettycount(stat_lhs.count), 8) * " → " * lpad(prettycount(stat_rhs.count), 8), 21), " ", lpad(prettydiff(ratio.count), 12))
    println(io)
end

function default_on_add_or_delete(key, item, mode, indent; io = stdout)
    stat = Stat(item)
    print(io, mode == ADD ? "+ " : "- ")
    print_indented(indent, key, io = io)
    print(io, "    ")
    print(io, rpad(lpad(prettytime(stat.mean), mode == DELETE ? 9 : 21), 34))
    print(io, "    ")
    print(io, rpad(lpad(prettycount(stat.count), mode == DELETE ? 10 : 21), 34))
    println(io)
end

function just_walk(a::OrderedDict{String}, mode::UInt8, indent::Vector{Bool}, on_add_or_delete)
    push!(indent, false)
    for (idx, (key, val)) in enumerate(a)
        if idx == length(a)
            indent[end] = true
        end

        on_add_or_delete(key, val, mode, indent)
        just_walk(val["sub-timings"], mode, indent, on_add_or_delete)
    end
    pop!(indent)
end

function print_header(; io = stdout)
    print(io,
"""
  ================================================================================================================================================
                                                                                      Mean                                    #
  ------------------------------------------------------------------------------------------------------------------------------------------------
""")
end

function walk(a::OrderedDict{String}, b::Union{Nothing,OrderedDict{String}}, indent::Vector{Bool} = Bool[], on_keep = default_on_keep, on_add_or_delete = default_on_add_or_delete)
    
    # ordered set of keys -- just create a copy for ease
    keys_a, keys_b = collect(keys(a)), collect(keys(b))

    # actions for diffing: keep / add / delete
    actions = backtrack(keys_a, keys_b)

    # indices for keys
    i, j = 1, 1

    push!(indent, false)

    for (idx, action) in enumerate(actions)
        if idx == length(actions)
            indent[end] = true
        end

        if action == KEEP
            key = keys_a[i]; lhs, rhs = a[key], b[key]
            on_keep(key, lhs, rhs, indent)
            walk(lhs["sub-timings"], rhs["sub-timings"], indent, on_keep, on_add_or_delete)
            i += 1; j += 1
        elseif action == DELETE
            key = keys_a[i]; lhs = a[key]
            on_add_or_delete(key, lhs, DELETE, indent)
            just_walk(lhs["sub-timings"], DELETE, indent, on_add_or_delete)
            i += 1
        else
            key = keys_b[j]; rhs = b[key]
            on_add_or_delete(key, rhs, ADD, indent)
            just_walk(rhs["sub-timings"], ADD, indent, on_add_or_delete)
            j += 1
        end
    end

    pop!(indent)

    return nothing
end

to_json(a::String) = JSON.parsefile(a, dicttype=OrderedDict)

to_table(a::String, b::String; io = stdout) = to_table(to_json(a), to_json(b), io = io)

function to_table(a, b; io = stdout)
    print_header(io = io)
    on_keep = (key, lhs, rhs, indent) -> default_on_keep(key, lhs, rhs, indent, io = io)
    on_add_or_delete = (key, item, mode, indent) -> default_on_add_or_delete(key, item, mode, indent, io = io)
    walk(a, b, Bool[], on_keep, on_add_or_delete)
end


end # module
