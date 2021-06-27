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

###############
# Identifiers #
###############

abstract type LambdaTerm end

struct Constant <: LambdaTerm
    name::Symbol
    type::LambdaType
end

struct Variable <: LambdaTerm
    name::Symbol
    type::LambdaType
end

const Identifier = Union{Constant,Variable}

name(identifier::Identifier) = identifier.name
type(identifier::Identifier) = identifier.type

###############
# Abstraction #
###############

abstract type Abstraction <: LambdaTerm end

struct NamedAbstraction <: Abstraction
    var::Variable
    body::LambdaTerm
end

var(abs::NamedAbstraction) = abs.var
body(abs::NamedAbstraction) = abs.body
type(abs::NamedAbstraction) = ArrowType(type(var(abs)), type(body(abs)))

###############
# Application #
###############

struct LambdaTypeError <: Exception
    msg::String
end

struct Application <: LambdaTerm
    operator::LambdaTerm
    operand::LambdaTerm

    function Application(operator::LambdaTerm, operand::LambdaTerm)
        if type(operator) isa ArrowType && type(operand) == source(type(operator))
            new(operator, operand)
        else
            throw(LambdaTypeError("type mismatch: expected " *
                                  "$(source(type(operator)))" *
                                  " got $(type(operand))"))
        end
    end
end

operator(app::Application) = app.operator
operand(app::Application) = app.operand
type(app::Application) = target(type(operator(app)))


#=free_vars(var::Variable) = Set{Variable}((var,))

free_vars(constant::Constant) = Set{Variable}()

free_vars(abs::Abstraction) = setdiff(free_vars(body(abs)), (var(abs),))

free_vars(app::Application) = union(free_vars(operator(app)),
                                    free_vars(operand(app)))

bound_vars(var::Identifier) = Set{Variable}()

bound_vars(abs::Abstraction) = union(bound_vars(body(abs)), (var(abs),))

bound_vars(app::Application) = union(bound_vars(operator(app)),
                                     bound_vars(operand(app)))

all_vars(term::LambdaTerm) = union(free_vars(term), bound_vars(term))=#


