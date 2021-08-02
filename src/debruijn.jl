###################
# De Bruijn Terms #
###################

abstract type DeBruijnLambdaTerm end
abstract type DeBruijnIndex <: DeBruijnLambdaTerm end

struct BoundDeBruijnIndex <: DeBruijnIndex
    i::Int
    type::LambdaType
    context::Context
end

struct FreeDeBruijnIndex <: DeBruijnIndex
    i::Int
    type::LambdaType
    context::Context
end

idx(idx::DeBruijnIndex) = idx.i
type(idx::DeBruijnIndex) = idx.type
context(idx::DeBruijnIndex) = idx.context

struct DeBruijnAbstraction <: DeBruijnLambdaTerm
    source_type::LambdaType
    body::DeBruijnLambdaTerm
    context::Context
end

source_type(abs::DeBruijnAbstraction) = abs.source_type
body(abs::DeBruijnAbstraction) = abs.body
context(abs::DeBruijnAbstraction) = abs.context
type(abs::DeBruijnAbstraction) = ArrowType(source_type(abs), type(body(abs)))

function type_check(operator::DeBruijnLambdaTerm, operand::DeBruijnLambdaTerm)
    type(operator) == type(operand) == UNTYPED ||
       (type(operator) isa ArrowType &&
        type(operand) == source(type(operator)))
end

struct DeBruijnApplication <: DeBruijnLambdaTerm
    operator::DeBruijnLambdaTerm
    operand::DeBruijnLambdaTerm
    context::Context

    function DeBruijnApplication(operator::DeBruijnLambdaTerm,
                                 operand::DeBruijnLambdaTerm,
                                 context::Context)
        if check_context(operator, operand, context) && type_check(operator, operand)
            new(operator, operand, context)
        else
            throw(LambdaTypeError("type mismatch: $(type(operator)) " *
                                  "applied to $(type(operand))"))
        end
    end
end

operator(app::DeBruijnApplication) = app.operator
operand(app::DeBruijnApplication) = app.operand
context(app::DeBruijnApplication) = app.context
type(app::DeBruijnApplication) = target(type(operator(app)))

##############################
# De Bruijn-Indexed to Named #
##############################

debruijn_to_named(t::DeBruijnLambdaTerm) = 
    _debruijn_to_named(t, Dict{Int,Variable}(enumerate(free_vars(context(t)))))

_debruijn_to_named(i::DeBruijnIndex, subs::Dict{Int,Variable}) = subs[idx(i)]
_debruijn_to_named(app::DeBruijnApplication, subs::Dict{Int,Variable}) =
    Application(map(f->_debruijn_to_named(f(app), subs), (operator, operand))...)
function _debruijn_to_named(abs::DeBruijnAbstraction, subs::Dict{Int,Variable})
    new_var = BoundVariable(_find_name(subs), source_type(abs), context(abs))
    new_subs = setindex!(Dict{Int,Variable}((i + 1) => v for (i, v) in subs), new_var, 1)
    Abstraction(new_var, _debruijn_to_named(body(abs), new_subs), context(abs))
end

const VARORDER = "xyzwuvpqrstabcdefghijklmno"
Base.iterate(::Val{:var_names}) = (Symbol(VARORDER[1]), (2, 1))
Base.iterate(::Val{:var_names}, (i, rep)) =
    (Symbol(VARORDER[i]^rep), i == length(VARORDER) ? (1, rep+1) : (i+1, rep))
function _find_name(subs)
    used_names = collect(map(name, values(subs)))
    for name in Val(:var_names)
        if !(name in used_names)
            return name
        end
    end
end

##############################
# Named to De Bruijn-Indexed #
##############################

named_to_debruijn(t::LambdaTerm) = 
    _named_to_debruijn(t, Dict{Variable,DeBruijnIndex}(v => FreeDeBruijnIndex(i, type(v), context(v))
                               for (i, v) in enumerate(free_vars(context(t)))))

_named_to_debruijn(v::Variable, subs::Dict{Variable,DeBruijnIndex}) = subs[v]
_named_to_debruijn(app::Application, subs::Dict{Variable,DeBruijnIndex}) =
    DeBruijnApplication(_named_to_debruijn(operator(app), subs),
                        _named_to_debruijn(operand(app), subs), context(app))
function _named_to_debruijn(abs::Abstraction, subs::Dict{Variable,DeBruijnIndex})
    new_subs = Dict{Variable,DeBruijnIndex}(v => i + 1 for (v, i) in subs)
    new_subs[var(abs)] = BoundDeBruijnIndex(1, source(type(abs)), context(abs))
    DeBruijnAbstraction(source(type(abs)), _named_to_debruijn(body(abs), new_subs),
                        context(abs))
end

##################
# Index Shifting #
##################

#Base.:+(i::BoundDeBruijnIndex, j::Int) = BoundDeBruijnIndex(idx(i) + j, type(i))
#Base.:+(i::FreeDeBruijnIndex, j::Int) = FreeDeBruijnIndex(idx(i) + j, type(i), context(i))
Base.:+(i::T, j::Int) where {T<:DeBruijnIndex} = T(idx(i) + j, type(i), context(i))
Base.:+(app::DeBruijnApplication, j::Int) =
    DeBruijnApplication(operator(app) + j, operand(app) + j, context(app))
function Base.:+(abs::DeBruijnAbstraction, j::Int)
    add_free(i::DeBruijnIndex, j, depth) = idx(i) > depth ? i + j : i
    add_free(app::DeBruijnApplication, j, depth) =
        DeBruijnApplication(add_free(operator(app), j, depth),
                            add_free(operand(app), j, depth), context(abs))
    add_free(abs::DeBruijnAbstraction, j, depth) =
        DeBruijnAbstraction(source_type(abs),
                            add_free(body(abs), j, depth + 1), context(abs))
    add_free(abs, j, 0)
end
Base.:+(j::Int, i::DeBruijnLambdaTerm) = i + j
Base.:-(i::DeBruijnLambdaTerm, j::Int) = i + (-j)
Base.:-(j::Int, i::DeBruijnLambdaTerm) = i - j
