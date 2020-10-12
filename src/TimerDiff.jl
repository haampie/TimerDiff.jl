module TimerDiff

# TODO: make section variable, configure widths, etc.

import JSON
import DataStructures: OrderedDict
import Printf: @sprintf
import StatsBase: mean, median, median!, quantile!

const KEEP = 0x00
const DELETE = 0x01
const ADD = 0x02

const separator = " | "

abstract type Node end

struct TimingNode <: Node
    identifier::String
    timings::Vector{Float64}
    subNodes::Vector{TimingNode}
    totalTime::Float64

    TimingNode(identifier::String, timings::Vector{Float64}, subNodes::Vector{TimingNode}) = new(identifier, timings, subNodes, sum(timings))
end

struct RootNode <: Node
    subNodes::Vector{TimingNode}
end

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
            i -= 1; j -= 1
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

function describe_change(ratio; factor = 0.1)
    diff = ratio - 1

    if diff > factor
        :increased
    elseif diff < -factor
        :decreased
    else
        :no
    end
end

const diff_size = length("(+xxx.xx%)")

# column_size(x)

struct StatFormat{diff}
  stat::Symbol
  header::String
  space::Int

  function StatFormat{diff}(s::Symbol) where diff
    s === :count            && return new{diff}(s, "#", 9)
    s === :total            && return new{diff}(s, "Total", 14)
    s === :self             && return new{diff}(s, "Self", 14)
    s === :mean             && return new{diff}(s, "Mean", 14)
    s === :median           && return new{diff}(s, "Median", 14)
    s === :quartilehigh     && return new{diff}(s, "Quartile High", 14)
    s === :quartilelow      && return new{diff}(s, "Quartile Low", 14)
    s === :min              && return new{diff}(s, "Min", 14)
    s === :max              && return new{diff}(s, "Max", 14)
    s === :percentage       && return new{diff}(s, "%", 11)
    s === :parentpercentage && return new{diff}(s, "Parent %", 11)
    s === :selfpercentage   && return new{diff}(s, "Self %", 11)
    throw(ArgumentError("Unknown stat $s requested"))
  end
end

function max_node_identifier_length(node::Node, recursionDepth = 0, addPerLevel = 2, len = 0)
    if node isa TimingNode
        currentLength = length(node.identifier) + recursionDepth * addPerLevel
        len = max(currentLength, len)
    end

    for child in node.subNodes
        len = max_node_identifier_length(child, recursionDepth + 1, addPerLevel, len)
    end

    return len
end

struct StatWrapper
    node::TimingNode
    rootTime::Float64
    parentTime::Float64
    subTime::Float64
end

safe_ratio(a, b) = iszero(b) || a > b ? 1.0 : a / b
safe_self(total, sub) = max(0.0, total - sub)

compute(::Val{:count}, wrap::StatWrapper) = length(wrap.node.timings)
compute(::Val{:total}, wrap::StatWrapper) = wrap.node.totalTime
compute(::Val{:self}, wrap::StatWrapper) = safe_self(wrap.node.totalTime, wrap.subTime)
compute(::Val{:mean}, wrap::StatWrapper) = wrap.node.totalTime / length(wrap.node.timings)
compute(::Val{:median}, wrap::StatWrapper) = median!(wrap.node.timings)
compute(::Val{:quartilehigh}, wrap::StatWrapper) = quantile!(wrap.node.timings, 0.75)
compute(::Val{:quartilelow}, wrap::StatWrapper) = quantile!(wrap.node.timings, 0.25)
compute(::Val{:min}, wrap::StatWrapper) = minimum(wrap.node.timings)
compute(::Val{:max}, wrap::StatWrapper) = maximum(wrap.node.timings)
compute(::Val{:percentage}, wrap::StatWrapper) = safe_ratio(wrap.node.totalTime, wrap.rootTime)
compute(::Val{:parentpercentage}, wrap::StatWrapper) = safe_ratio(wrap.node.totalTime, wrap.parentTime)
compute(::Val{:selfpercentage}, wrap::StatWrapper) = safe_ratio(safe_self(wrap.node.totalTime, wrap.subTime), wrap.node.totalTime)

format(::Val{:count}, val) = prettycount(val)
format(::Val{:total}, val) = prettytime(val)
format(::Val{:self}, val) = prettytime(val)
format(::Val{:mean}, val) = prettytime(val)
format(::Val{:median}, val) = prettytime(val)
format(::Val{:quartilehigh}, val) = prettytime(val)
format(::Val{:quartilelow}, val) = prettytime(val)
format(::Val{:min}, val) = prettytime(val)
format(::Val{:max}, val) = prettytime(val)
format(::Val{:percentage}, val) = prettypercent(val)
format(::Val{:parentpercentage}, val) = prettypercent(val)
format(::Val{:selfpercentage}, val) = prettypercent(val)

abstract type Mode end
struct Normal <: Mode end
struct Delete <: Mode end
struct Add <: Mode end;

function print_stat(io, f, lhs::StatWrapper, rhs::StatWrapper)
    from = compute(Val(f.stat), lhs)
    to = compute(Val(f.stat), rhs)
    diff = prettydiff(to / from)
    result = lpad(lpad(format(Val(f.stat), from), f.space) * " → " * lpad(format(Val(f.stat), to), f.space) * " " * lpad(diff, diff_size), column_space(f))
    print(io, result, separator)
end

function print_stat(io, f, x::StatWrapper, mode::Mode = Normal())
    result = format(Val(f.stat), compute(Val(f.stat), x))
    if mode isa Delete
        result *= " " ^ (3 + f.space + 1 + diff_size)
    elseif mode isa Add
        result *= " " ^ (1 + diff_size)
    end

    print(io, lpad(result, column_space(f)), separator)
end

function print_node_diff(io, a::Node, b::Node, formats, identifierSpace, nodePrefix = "", isLastSubnode = false, parentTimeLhs = 0.0, parentTimeRhs = 0.0, rootTimeLhs = 0.0, rootTimeRhs = 0.0, depth = 0)
    subTimeLhs = mapreduce(child -> child.totalTime, +, a.subNodes, init=0.0)
    subTimeRhs = mapreduce(child -> child.totalTime, +, b.subNodes, init=0.0)

    if a isa TimingNode && b isa TimingNode
        stats_a = StatWrapper(a, rootTimeLhs, parentTimeLhs, subTimeLhs)
        stats_b = StatWrapper(b, rootTimeRhs, parentTimeRhs, subTimeRhs)

        total_runtime_change = describe_change(compute(Val(:total), stats_b) / compute(Val(:total), stats_a))
        mean_runtime_change = describe_change(compute(Val(:mean), stats_b) / compute(Val(:mean), stats_a))
        count_change = describe_change(compute(Val(:count), stats_b) / compute(Val(:count), stats_a))
        
        print(io, if total_runtime_change === :increased || mean_runtime_change === :increased
            "- "
        elseif total_runtime_change === :decreased || mean_runtime_change === :decreased
            "+ "
        elseif count_change !== :no
            "! "
        else
            "  "
        end)

        identifier = rpad(nodePrefix * (depth > 1 ? (isLastSubnode ? "└ " : "├ ") : (depth <= 1 ? "" : "  ")) * a.identifier, identifierSpace)
        print(io, identifier, separator)

        for format in formats
            print_stat(io, format, stats_a, stats_b)
        end

        println(io)

        parentTimeLhs, parentTimeRhs = a.totalTime, b.totalTime 
    end

    if depth > 1
        nodePrefix *= isLastSubnode ? "  " : "│ "
    end
    
    # ordered set of keys -- just create a copy for ease
    keys_a, keys_b = [child.identifier for child in a.subNodes], [child.identifier for child in b.subNodes]

    # actions for diffing: keep / add / delete
    actions = backtrack(keys_a, keys_b)

    # indices for keys
    i, j = 1, 1

    for (idx, action) in enumerate(actions)
        is_last = idx == length(actions)

        if a isa RootNode && (action == KEEP || action == DELETE)
            rootTimeLhs = a.subNodes[i].totalTime
        end

        if b isa RootNode && (action == KEEP || action == ADD)
            rootTimeRhs = b.subNodes[j].totalTime
        end
        
        if action == KEEP
            print_node_diff(io, a.subNodes[i], b.subNodes[j], formats, identifierSpace, nodePrefix, is_last, parentTimeLhs, parentTimeRhs, rootTimeLhs, rootTimeRhs, depth + 1)
            i += 1; j += 1
        elseif action == DELETE
            print_node(io, a.subNodes[i], formats, identifierSpace, nodePrefix, is_last, parentTimeLhs, rootTimeLhs, depth + 1, Delete())
            i += 1
        else
            print_node(io, b.subNodes[j], formats, identifierSpace, nodePrefix, is_last, parentTimeRhs, rootTimeRhs, depth + 1, Add())
            j += 1
        end
    end

    return nothing
end

function print_node(io, x::Node, formats, identifierSpace, nodePrefix = "", isLastSubnode = false, parentTime = 0.0, rootTime = 0.0, depth = 0, mode::Mode = Normal())
    subTime = mapreduce(child -> child.totalTime, +, x.subNodes, init=0.0)

    if x isa TimingNode
        if mode isa Add || mode isa Delete
            print(io, "  ")
        end

        identifier = rpad(nodePrefix * (depth > 1 ? (isLastSubnode ? "└ " : "├ ") : (depth <= 1 ? "" : "  ")) * x.identifier, identifierSpace)
        print(io, identifier, separator)

        for format in formats
            print_stat(io, format, StatWrapper(x, rootTime, parentTime, subTime), mode)
        end

        println(io)

        parentTime = x.totalTime
    end

    if depth > 1
        nodePrefix *= isLastSubnode ? "  " : "│ "
    end

    for (idx, child) in enumerate(x.subNodes)
        if x isa RootNode
            rootTime = child.totalTime
        end
        print_node(io, child, formats, identifierSpace, nodePrefix, idx == length(x.subNodes), parentTime, rootTime, depth + 1, mode)
    end
    
    return nothing
end

# just printing the value
column_space(x::StatFormat{false}) = x.space

# printing a diff requires 2 times normal space + space for diff percentage.
column_space(x::StatFormat{true}) = 2 * x.space + 4 + diff_size

function print_things(lhs::RootNode, rhs::RootNode, statistics::Vector{Symbol}; io = stdout)
    # calculate space for printing identifiers
    identifierSpace = max(max_node_identifier_length(lhs), max_node_identifier_length(rhs))
    totalSpace = identifierSpace

    formats = [StatFormat{true}(stat) for stat in statistics]
    totalSpace += mapreduce(column_space, +, formats, init = 0)
    totalSpace += length(separator) * (length(formats) + 1) - 1

    # Table start
    println(io, "  ", '=' ^ totalSpace)

    # Header
    print(io, "  ", " " ^ identifierSpace, separator)
    for format in formats
        print(io, lpad(format.header, column_space(format)), separator)
    end
    println(io)

    # Header separation line
    println(io, "  ", '-' ^ totalSpace)

    print_node_diff(io, lhs, rhs, formats, identifierSpace)

    # End table
    println(io, "  ", '=' ^ totalSpace)

    return nothing
end

function print_things(x::RootNode, statistics::Vector{Symbol}; io = stdout)
    # calculate space for printing identifiers
    identifierSpace = max_node_identifier_length(x)
    totalSpace = identifierSpace

    formats = [StatFormat{false}(stat) for stat in statistics]
    totalSpace += mapreduce(column_space, +, formats, init = 0)
    totalSpace += length(separator) * (length(formats) + 1) - 1

    # Table start
    println(io, '=' ^ totalSpace)

    # Header
    print(io, " " ^ identifierSpace, separator)
    for format in formats
        print(io, lpad(format.header, column_space(format)), separator)
    end
    println(io)

    # Header separtion line
    println(io, '-'^totalSpace)

    print_node(io, x, formats, identifierSpace)

    # End table
    println(io, '='^totalSpace)

    return nothing
end

function parse_json(x::OrderedDict)
    children = Vector{TimingNode}()
    for (key, value) in x
        push!(children, TimingNode(
            key,
            convert(Vector{Float64}, value["timings"]),
            parse_json(value["sub-timings"])
        ))
    end
    return children
end


to_json(a::String) = RootNode(parse_json(JSON.parsefile(a, dicttype=OrderedDict)))

to_table(a::String, b::String; stats = [:mean, :count, :total], io = stdout) = print_things(to_json(a), to_json(b), stats, io = io)
to_table(a::String; stats = [:mean, :count, :total], io = stdout) = print_things(to_json(a), stats, io = io)

end # module
