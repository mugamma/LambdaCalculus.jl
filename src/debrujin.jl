######################
# De Brujin Indexing #
######################

abstract type DeBrujinLambdaTerm end

struct DeBrujinIndex <: DeBrujinLambdaTerm
    i::Int
    type::LambdaType
    context::Context
end

idx(idx::DeBrujinIndex) = idx.i
type(idx::DeBrujinIndex) = idx.type
context(idx::DeBrujinIndex) = idx.context
incr(i::DeBrujinIndex) = DeBrujinIndex(idx(i) + 1, type(i), context(i))

struct DeBrujinAbstraction <: DeBrujinLambdaTerm
    source_type::LambdaType
    body::DeBrujinLambdaTerm
    context::Context
end

source_type(abs::DeBrujinAbstraction) = abs.source_type
body(abs::DeBrujinAbstraction) = abs.body
context(abs::DeBrujinAbstraction) = abs.context
type(abs::DeBrujinAbstraction) = ArrowType(source_type(abs), type(body(abs)))

struct DeBrujinApplication <: DeBrujinLambdaTerm
    operator::DeBrujinLambdaTerm
    operand::DeBrujinLambdaTerm
    context::Context
    function DeBrujinApplication(operator::DeBrujinLambdaTerm,
                                 operand::DeBrujinLambdaTerm,
                                 context::Context)
        if check_context(operator, operand, context) &&
           type(operator) isa ArrowType &&
           type(operand) == source(type(operator))
            new(operator, operand, context)
        else
            #throw(LambdaTypeError("type mismatch: expected " *
                                  #"$(source(type(operator)))" *
                                  #" got $(type(operand))"))
        end
    end
end

operator(app::DeBrujinApplication) = app.operator
operand(app::DeBrujinApplication) = app.operand
context(app::DeBrujinApplication) = app.context
type(app::DeBrujinApplication) = target(type(operator(app)))

# XXX refactor?

const VARORDER = [Symbol(c) for c in "xyzwuvpqrstabcdefghijklmno"]
debrujin_to_named(t::DeBrujinLambdaTerm) = 
    _debrujin_to_named(t, Dict(enumerate(identifiers(context(t)))))

_debrujin_to_named(i::DeBrujinIndex, subs::Dict{Int,Identifier}) = subs[idx(i)]
_debrujin_to_named(app::DeBrujinApplication, subs::Dict{Int,Identifier}) =
    Application(map(f->_debrujin_to_named(f(app), subs), (operator, operand))...)
function _debrujin_to_named(abs::DeBrujinAbstraction, subs::Dict{Int,Identifier})
    used_vars = collect(map(name, values(subs)))
    new_var = Variable(VARORDER[findfirst(c->!(c in used_vars), VARORDER)],
                       source_type(abs))
    new_subs = setindex!(Dict((k + 1) => v for (k, v) in subs), new_var, 1)
    Abstraction(new_var, _debrujin_to_named(body(abs), new_subs))
end

#=function debrujin_to_named(abs::DeBrujinAbstraction)
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

    init_subs = 
    Abstraction(init_subs[1],
                     substitute_body(body(abs), init_subs))
end=#

named_to_debrujin(t::LambdaTerm) = 
    _named_to_debrujin(t, Dict(v => DeBrujinIndex(i, type(v), context(v))
                               for (i, v) in enumerate(identifiers(context(t)))))

_named_to_debrujin(i::Identifier, subs::Dict{<:Identifier,DeBrujinIndex}) = subs[i]
_named_to_debrujin(app::Application, subs::Dict{<:Identifier,DeBrujinIndex}) =
    DeBrujinApplication(_named_to_debrujin(operator(app), subs),
                        _named_to_debrujin(operand(app), subs), context(app))
function _named_to_debrujin(abs::Abstraction, subs::Dict{<:Identifier,DeBrujinIndex})
    new_subs = Dict(zip(keys(subs), map(incr, values(subs))))
    new_subs[var(abs)] = DeBrujinIndex(1, source(type(abs)), context(var(abs)))
    DeBrujinAbstraction(source(type(abs)), _named_to_debrujin(body(abs), new_subs),
                        context(abs))
end
    
#=function named_to_debrujin(abs::Abstraction)
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
               
end=#
