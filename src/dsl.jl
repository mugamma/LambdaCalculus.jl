######################
# Construction Sugar #
######################

macro atomic_type(names...)
    clean_names = [split(string(name), ".")[end] for name in names]
    Expr(:block,
         [:($(esc(name)) = AtomicType(Symbol($clean_name)))
          for (name, clean_name) in zip(names, clean_names)]...)
end

macro variable(tokens...)
    names = map(token->token.args[1], tokens)
    types = map(token->token.args[2], tokens)
    Expr(:block,
         [:($(esc(name)) = Variable(Symbol($(string(name))), $(esc(typ))))
          for (name, typ) in zip(names, types)]...)
end

macro constant(names...)
    #definition_macro_builder(names, Constant)
end

≃(x, y) = alpha_equivalent(x, y)

Base.Pair(s::LambdaType, t::LambdaType) = ArrowType(s, t)

struct VariableReference
    name::Symbol
end

annotate(v::VariableReference, t::LambdaType) = Variable(v.name, t)
Base.getindex(v::VariableReference, t::LambdaType) = Variable(v.name, t)

lambda(var::Variable, body::LambdaTerm) = Abstraction(var, body)

λ = lambda

(operator::LambdaTerm)(operand::LambdaTerm) = Application(operator, operand)

