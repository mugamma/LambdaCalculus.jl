#########
# Types #
#########

"""
The supertype of the type ojbects of lambda terms.
"""
abstract type LambdaType end

"""
The singleton type representing the type of terms in untyped lambda calculus.

`Untyped` has a single instance (accessible as `UNTYPED`) which has the property
that a lambda term is of type `UNTYPED` if and only if it is of type
`ArrowType(UNTYPED,UNTYPED)`.
"""
struct Untyped <: LambdaType end

"""
The type of the terms in untyped lambda calculus.

The only instance of `Untyped`.
"""
const UNTYPED = Untyped()

source(::Untyped) = UNTYPED
target(::Untyped) = UNTYPED

"""
The basic type of a term.
"""
struct AtomicType <: LambdaType
    name::Symbol
end

name(t::AtomicType) = t.name

struct ArrowType <: LambdaType
    source::LambdaType
    target::LambdaType
end

ArrowType(::Untyped, ::Untyped) = UNTYPED

source(t::ArrowType) = t.source
target(t::ArrowType) = t.target

##########################
# Variables and Contexts #
##########################

abstract type LambdaTerm end

abstract type Variable <: LambdaTerm end

struct Context
    free_vars::Vector{Variable}
end

Context() = Context(FreeVariable[])

struct ContextError <: Exception
    msg::String
end

struct BoundVariable <: Variable
    name::Symbol
    type::LambdaType
    context::Context
end

struct FreeVariable <: Variable
    name::Symbol
    type::LambdaType
    context::Context
    FreeVariable(name::Symbol, type::LambdaType, context::Context) =
        register(context, new(name, type, context))
end

const GLOBAL_CONTEXT = Context()

BoundVariable(name::Symbol, type::LambdaType) =
    BoundVariable(name, type, GLOBAL_CONTEXT)
FreeVariable(name::Symbol, type::LambdaType) =
    FreeVariable(name, type, GLOBAL_CONTEXT)
name(v::Variable) = v.name
type(v::Variable) = v.type
context(v::Variable) = v.context

free_vars(c::Context) = c.free_vars
register(c::Context, fv::FreeVariable) = (push!(c.free_vars, fv); fv)
check_context(s, t, c::Context) =
    context(s) === context(t) === c || throw(ContextError("mismatching contexts"))


"""
    type(t::Union{LambdaTerm,DeBruijnLambdaTerm})

Return the type associated with the lambda term `t`.
"""
function type end

"""
    context(t::Union{LambdaTerm,DeBruijnLambdaTerm})

Return the context of evaluation for the lambda term `t`.
"""
function context end


###############
# Abstraction #
###############

struct Abstraction <: LambdaTerm 
    var::BoundVariable
    body::LambdaTerm
    context::Context
end

Abstraction(var::BoundVariable, body::LambdaTerm) =
    Abstraction(var, body, context(var))

var(abs::Abstraction) = abs.var
body(abs::Abstraction) = abs.body
context(abs::Abstraction) = abs.context
type(abs::Abstraction) = ArrowType(type(var(abs)), type(body(abs)))

###############
# Application #
###############

struct LambdaTypeError <: Exception
    msg::String
end

function type_check(operator::LambdaTerm, operand::LambdaTerm)
    type(operator) == type(operand) == UNTYPED ||
       (type(operator) isa ArrowType &&
        type(operand) == source(type(operator)))
end

struct Application <: LambdaTerm
    operator::LambdaTerm
    operand::LambdaTerm
    context::Context

    function Application(operator::LambdaTerm, operand::LambdaTerm, context::Context)
        if check_context(operator, operand, context) && type_check(operator, operand)
            new(operator, operand, context)
        else
            throw(LambdaTypeError("type mismatch: $(type(operator)) " *
                                  "applied to $(type(operand))"))
        end
    end
end

Application(operator::LambdaTerm, operand::LambdaTerm) =
    Application(operator, operand, context(operator))

operator(app::Application) = app.operator
operand(app::Application) = app.operand
context(app::Application) = app.context
type(app::Application) = target(type(operator(app)))

free_vars(var::Variable) = Set{Variable}((var,))

free_vars(abs::Abstraction) = setdiff(free_vars(body(abs)), (var(abs),))

free_vars(app::Application) = union(free_vars(operator(app)),
                                    free_vars(operand(app)))

bound_vars(var::Variable) = Set{Variable}()

bound_vars(abs::Abstraction) = union(bound_vars(body(abs)), (var(abs),))

bound_vars(app::Application) = union(bound_vars(operator(app)),
                                     bound_vars(operand(app)))

all_vars(term::LambdaTerm) = union(free_vars(term), bound_vars(term))
