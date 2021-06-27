######################
# De Brujin Indexing #
######################

struct DeBrujinIndex <: LambdaTerm
    i::Int
    type::LambdaType
end

idx(idx::DeBrujinIndex) = idx.i
type(idx::DeBrujinIndex) = idx.type
incr(idx::DeBrujinIndex) = DeBrujinIndex(idx.i + 1, idx.type)

struct DeBrujinAbstraction <: Abstraction
    source_type::LambdaType
    body::LambdaTerm
end

source_type(abs::DeBrujinAbstraction) = abs.source_type
body(abs::DeBrujinAbstraction) = abs.body
type(abs::DeBrujinAbstraction) = ArrowType(source_type(abs), type(body(abs)))

# XXX refactor?

const IdxSubsType = Dict{Int,Variable}
const VARORDER = [Symbol(c) for c in "xyzwuvpqrstabcdefghijklmno"]
function debrujin_to_named(abs::DeBrujinAbstraction)
    substitute_body(i::DeBrujinIndex, subs::IdxSubsType) = subs[idx(i)]
    substitute_body(i::Identifier, subs::IdxSubsType) = i
    substitute_body(app::Application, subs::IdxSubsType) =
        Application(substitute_body(operator(app), subs),
                    substitute_body(operand(app), subs))
    function substitute_body(abs::DeBrujinAbstraction, subs::IdxSubsType)
        new_subs = Dict((k + 1) => v for (k, v) in subs)
        new_subs[1] = Variable(VARORDER[length(subs)+1], source_type(abs))
        NamedAbstraction(new_subs[1],
                         substitute_body(body(abs), new_subs))
    end

    init_subs = Dict(1 => Variable(VARORDER[1], source_type(abs)))
    NamedAbstraction(init_subs[1],
                     substitute_body(body(abs), init_subs))
end

const NamedSubsType = Dict{Symbol,DeBrujinIndex}
function named_to_debrujin(abs::NamedAbstraction)
    substitute_body(i::Identifier, subs::NamedSubsType) = get(subs, name(i), i)
    substitute_body(app::Application, subs::NamedSubsType) =
        Application(substitute_body(operator(app), subs),
                    substitute_body(operand(app), subs))
    function substitute_body(abs::NamedAbstraction, subs::NamedSubsType)
        new_subs = Dict(zip(keys(subs), map(incr, values(subs))))
        new_subs[name(var(abs))] = DeBrujinIndex(1, source(type(abs)))
        DeBrujinAbstraction(source(type(abs)),
                            substitute_body(body(abs), new_subs))
    end

    init_subs = Dict(name(var(abs)) => DeBrujinIndex(1, source(type(abs))))
    DeBrujinAbstraction(source(type(abs)),
                        substitute_body(body(abs), init_subs))
               
end
