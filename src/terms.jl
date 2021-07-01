#########
# Types #
#########

abstract type LambdaType end

struct AtomicType <: LambdaType
    name::Symbol
end

name(t::AtomicType) = t.name

struct ArrowType <: LambdaType
    source::LambdaType
    target::LambdaType
end

source(t::ArrowType) = t.source
target(t::ArrowType) = t.target

############################
# Identifiers and Contexts #
############################

abstract type LambdaTerm end

abstract type Identifier <: LambdaTerm end

struct ContextError <: Exception
    msg::String
end

struct Context
    identifiers::Vector{Identifier}
end

identifiers(c::Context) = c.identifiers

const GLOBAL_CONTEXT = Context(Identifier[])

register(c::Context, i::Identifier) = (push!(c.identifiers, i); i)
check_context(s, t, c::Context) =
    context(s) === context(t) === c || throw(ContextError("mismatching contexts"))

struct Constant <: Identifier
    name::Symbol
    type::LambdaType
    context::Context
    Constant(name::Symbol, type::LambdaType, context::Context) =
        register(context, new(name, type, context))
end

Constant(name::Symbol, type::LambdaType) =
    Constant(name, type, GLOBAL_CONTEXT)

struct Variable <: Identifier
    name::Symbol
    type::LambdaType
    context::Context
    Variable(name::Symbol, type::LambdaType, context::Context) =
        register(context, new(name, type, context))
end

Variable(name::Symbol, type::LambdaType) =
    Variable(name, type, GLOBAL_CONTEXT)

name(identifier::Identifier) = identifier.name
type(identifier::Identifier) = identifier.type
context(identifier::Identifier) = identifier.context


###############
# Abstraction #
###############

struct Abstraction <: LambdaTerm 
    var::Variable
    body::LambdaTerm
    context::Context
    Abstraction(var::Variable, body::LambdaTerm, context::Context) =
        (check_context(var, body, context); new(var, body, context))
end

Abstraction(var::Variable, body::LambdaTerm) =
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

struct Application <: LambdaTerm
    operator::LambdaTerm
    operand::LambdaTerm
    context::Context

    function Application(operator::LambdaTerm, operand::LambdaTerm, context::Context)
        if check_context(operator, operand, context) &&
           type(operator) isa ArrowType &&
           type(operand) == source(type(operator))
            new(operator, operand, context)
        else
            throw(LambdaTypeError("type mismatch: expected " *
                                  "$(source(type(operator)))" *
                                  " got $(type(operand))"))
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

free_vars(constant::Constant) = Set{Variable}()

free_vars(abs::Abstraction) = setdiff(free_vars(body(abs)), (var(abs),))

free_vars(app::Application) = union(free_vars(operator(app)),
                                    free_vars(operand(app)))

bound_vars(var::Identifier) = Set{Variable}()

bound_vars(abs::Abstraction) = union(bound_vars(body(abs)), (var(abs),))

bound_vars(app::Application) = union(bound_vars(operator(app)),
                                     bound_vars(operand(app)))

all_vars(term::LambdaTerm) = union(free_vars(term), bound_vars(term))


