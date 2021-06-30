######################
# De Brujin Indexing #
######################

abstract type DeBrujinLambdaTerm end

struct DeBrujinIndex <: DeBrujinLambdaTerm
    i::Int
    type::LambdaType
end

idx(idx::DeBrujinIndex) = idx.i
type(idx::DeBrujinIndex) = idx.type
incr(idx::DeBrujinIndex) = DeBrujinIndex(idx.i + 1, idx.type)

struct DeBrujinConstant <: DeBrujinLambdaTerm
    name::Symbol
    type::LambdaType
end

name(c::DeBrujinConstant) = c.name
type(c::DeBrujinConstant) = c.type

Constant(dbc::DeBrujinConstant) = Constant(name(dbc), type(dbc))
DeBrujinConstant(c::Constant) = DeBrujinConstant(name(c), type(c))

struct DeBrujinAbstraction <: DeBrujinLambdaTerm
    source_type::LambdaType
    body::DeBrujinLambdaTerm
end

source_type(abs::DeBrujinAbstraction) = abs.source_type
body(abs::DeBrujinAbstraction) = abs.body
type(abs::DeBrujinAbstraction) = ArrowType(source_type(abs), type(body(abs)))

struct DeBrujinApplication <: DeBrujinLambdaTerm
    operator::DeBrujinLambdaTerm
    operand::DeBrujinLambdaTerm
    function DeBrujinApplication(operator::DeBrujinLambdaTerm, operand::DeBrujinLambdaTerm)
        if type(operator) isa ArrowType && type(operand) == source(type(operator))
            new(operator, operand)
        else
            #throw(LambdaTypeError("type mismatch: expected " *
                                  #"$(source(type(operator)))" *
                                  #" got $(type(operand))"))
        end
    end
end

operator(app::DeBrujinApplication) = app.operator
operand(app::DeBrujinApplication) = app.operand
type(app::DeBrujinApplication) = target(type(operator(app)))

# XXX refactor?

const IdxSubsType = Dict{Int,Variable}
const VARORDER = [Symbol(c) for c in "xyzwuvpqrstabcdefghijklmno"]
function debrujin_to_named(abs::DeBrujinAbstraction)
    substitute_body(i::DeBrujinIndex, subs::IdxSubsType) = subs[idx(i)]
    substitute_body(c::DeBrujinConstant, subs::IdxSubsType) = Constant(c)
    substitute_body(app::DeBrujinApplication, subs::IdxSubsType) =
        Application(substitute_body(operator(app), subs),
                    substitute_body(operand(app), subs))
    function substitute_body(abs::DeBrujinAbstraction, subs::IdxSubsType)
        new_subs = Dict((k + 1) => v for (k, v) in subs)
        new_subs[1] = Variable(VARORDER[length(subs)+1], source_type(abs))
        Abstraction(new_subs[1],
                         substitute_body(body(abs), new_subs))
    end

    init_subs = Dict(1 => Variable(VARORDER[1], source_type(abs)))
    Abstraction(init_subs[1],
                     substitute_body(body(abs), init_subs))
end

const NamedSubsType = Dict{Symbol,DeBrujinIndex}
function named_to_debrujin(abs::Abstraction)
    substitute_body(i::Variable, subs::NamedSubsType) = get(subs, name(i), i)
    substitute_body(c::Constant, _::NamedSubsType) = DeBrujinConstant(c)
    substitute_body(app::Application, subs::NamedSubsType) =
        DeBrujinApplication(substitute_body(operator(app), subs),
                            substitute_body(operand(app), subs))
    function substitute_body(abs::Abstraction, subs::NamedSubsType)
        new_subs = Dict(zip(keys(subs), map(incr, values(subs))))
        new_subs[name(var(abs))] = DeBrujinIndex(1, source(type(abs)))
        DeBrujinAbstraction(source(type(abs)),
                            substitute_body(body(abs), new_subs))
    end

    init_subs = Dict(name(var(abs)) => DeBrujinIndex(1, source(type(abs))))
    DeBrujinAbstraction(source(type(abs)),
                        substitute_body(body(abs), init_subs))
               
end
