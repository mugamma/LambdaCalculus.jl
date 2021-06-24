######################
# Construction Sugar #
######################

#=macro variable(names...)
    definition_macro_builder(names, Variable)
end

macro constant(names...)
    definition_macro_builder(names, Constant)
end=#

struct VariableReference
    name::Symbol
end

annotate(v::VariableReference, t::LambdaType) = Variable(v.name, t)
Base.getindex(v::VariableReference, t::LambdaType) = Variable(v.name, t)

lambda(var::Variable, body::LambdaTerm) = Abstraction(var, body)

lambda(var::Symbol, body::LambdaTerm) = lambda(Variable(var), body)

lambda(var::Symbol, body::Symbol) = lambda(Variable(var), Variable(body))

(operator::LambdaTerm)(operand::LambdaTerm) = Application(operator, operand)

(operator::Symbol)(operand::LambdaTerm) = Variable(operator)(operand)

(operator::LambdaTerm)(operand::Symbol) = operator(Variable(operand))

(operator::Symbol)(operand::Symbol) = Variable(operator)(Variable(operand))

apply(f::LambdaTerm, x::LambdaTerm) = f(x)


