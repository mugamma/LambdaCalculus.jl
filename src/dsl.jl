##############
# DSL Macros #
##############

create_declarations(constructor, names, arg_lists) =
    Expr(:block, [:($(esc(name)) = $constructor($(args...)))
                  for (name, args) in zip(names, arg_lists)]...)

macro atomic_type(names...)
    create_declarations(:AtomicType, names, map(x->(QuoteNode(x),), names))
end

function declare_variables_in_context(constructor, tokens, context)
    names = map(token->token.args[1], tokens)
    types = map(token->token.args[2], tokens)
    create_declarations(constructor, names,
                        [(QuoteNode(name), esc(typ), esc(context))
                         for (name, typ) in zip(names, types)])
end

macro variable(tokens...)
    declare_variables_in_context(:BoundVariable, tokens, :GLOBAL_CONTEXT)
end

macro free_variable(tokens...)
    declare_variables_in_context(:FreeVariable, tokens, :GLOBAL_CONTEXT)
end

macro delay(e)
    esc(e)
end

is_valid_context_declaration(name, body) = 
    name isa Symbol && body isa Expr && body.head == :block

contextualize(lnn::LineNumberNode, context_name) = lnn

function contextualize(expr::Expr, context_name)
    if expr.head == :macrocall
        args = filter(n->n isa Union{Expr,Symbol}, expr.args)
        if args[1] in (Symbol("@variable"), Symbol("@free_variable"))
            constructor = args[1] == Symbol("@variable") ? :BoundVariable : :FreeVariable
            return :(@delay $(declare_variables_in_context(constructor, args[2:end], context_name)))
        end
    end
    esc(expr)
end

macro context(name, body)
    if !is_valid_context_declaration(name, body)
        throw(error("syntax: expected a context name and a block of "*
                    "declarations."))
    end
    Expr(:block, :($(esc(name)) = Context()),
         map(n->contextualize(n, name), body.args)...)
end

#########
# Sugar #
#########

≃(x, y) = alpha_equivalent(x, y)

Base.Pair(s::LambdaType, t::LambdaType) = ArrowType(s, t)

lambda(var::BoundVariable, body::LambdaTerm) = Abstraction(var, body)

λ = lambda

(operator::LambdaTerm)(operand::LambdaTerm) = Application(operator, operand)

