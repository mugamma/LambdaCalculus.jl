###################
# De Brujin Terms #
###################

abstract type DeBrujinLambdaTerm end
abstract type DeBrujinIndex <: DeBrujinLambdaTerm end

struct BoundDeBrujinIndex <: DeBrujinIndex
    i::Int
    type::LambdaType
    context::Context
end

struct FreeDeBrujinIndex <: DeBrujinIndex
    i::Int
    type::LambdaType
    context::Context
end

idx(idx::DeBrujinIndex) = idx.i
type(idx::DeBrujinIndex) = idx.type
context(idx::DeBrujinIndex) = idx.context

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
            #                      "$(source(type(operator)))" *
            #                      " got $(type(operand))"))
        end
    end
end

operator(app::DeBrujinApplication) = app.operator
operand(app::DeBrujinApplication) = app.operand
context(app::DeBrujinApplication) = app.context
type(app::DeBrujinApplication) = target(type(operator(app)))

##############################
# De Brujin-Indexed to Named #
##############################

const VARORDER = [Symbol(c) for c in "xyzwuvpqrstabcdefghijklmno"]
debrujin_to_named(t::DeBrujinLambdaTerm) = 
    _debrujin_to_named(t, Dict{Int,Variable}(enumerate(free_vars(context(t)))))

_debrujin_to_named(i::DeBrujinIndex, subs::Dict{Int,Variable}) = subs[idx(i)]
_debrujin_to_named(app::DeBrujinApplication, subs::Dict{Int,Variable}) =
    Application(map(f->_debrujin_to_named(f(app), subs), (operator, operand))...)
function _debrujin_to_named(abs::DeBrujinAbstraction, subs::Dict{Int,Variable})
    used_vars = collect(map(name, values(subs)))
    new_var = BoundVariable(VARORDER[findfirst(c->!(c in used_vars), VARORDER)],
                            source_type(abs), context(abs))
    new_subs = setindex!(Dict{Int,Variable}((i + 1) => v for (i, v) in subs), new_var, 1)
    Abstraction(new_var, _debrujin_to_named(body(abs), new_subs), context(abs))
end

##############################
# Named to De Brujin-Indexed #
##############################

named_to_debrujin(t::LambdaTerm) = 
    _named_to_debrujin(t, Dict{Variable,DeBrujinIndex}(v => FreeDeBrujinIndex(i, type(v), context(v))
                               for (i, v) in enumerate(free_vars(context(t)))))

_named_to_debrujin(v::Variable, subs::Dict{Variable,DeBrujinIndex}) = subs[v]
_named_to_debrujin(app::Application, subs::Dict{Variable,DeBrujinIndex}) =
    DeBrujinApplication(_named_to_debrujin(operator(app), subs),
                        _named_to_debrujin(operand(app), subs), context(app))
function _named_to_debrujin(abs::Abstraction, subs::Dict{Variable,DeBrujinIndex})
    new_subs = Dict{Variable,DeBrujinIndex}(v => i + 1 for (v, i) in subs)
    new_subs[var(abs)] = BoundDeBrujinIndex(1, source(type(abs)), context(abs))
    DeBrujinAbstraction(source(type(abs)), _named_to_debrujin(body(abs), new_subs),
                        context(abs))
end

##################
# Index Shifting #
##################

#Base.:+(i::BoundDeBrujinIndex, j::Int) = BoundDeBrujinIndex(idx(i) + j, type(i))
#Base.:+(i::FreeDeBrujinIndex, j::Int) = FreeDeBrujinIndex(idx(i) + j, type(i), context(i))
Base.:+(i::T, j::Int) where {T<:DeBrujinIndex} = T(idx(i) + j, type(i), context(i))
Base.:+(app::DeBrujinApplication, j::Int) =
    DeBrujinApplication(operator(app) + j, operand(app) + j, context(app))
function Base.:+(abs::DeBrujinAbstraction, j::Int)
    add_free(i::DeBrujinIndex, j, depth) = idx(i) > depth ? i + j : i
    add_free(app::DeBrujinApplication, j, depth) =
        DeBrujinApplication(add_free(operator(app), j, depth),
                            add_free(operand(app), j, depth), context(abs))
    add_free(abs::DeBrujinAbstraction, j, depth) =
        DeBrujinAbstraction(source_type(abs),
                            add_free(body(abs), j, depth + 1), context(abs))
    add_free(abs, j, 0)
end
Base.:+(j::Int, i::DeBrujinLambdaTerm) = i + j
Base.:-(i::DeBrujinLambdaTerm, j::Int) = i + (-j)
Base.:-(j::Int, i::DeBrujinLambdaTerm) = i - j
