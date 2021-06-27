#################
# α-equivalence #
#################

alpha_equivalent(v::Identifier, u::Identifier) = v == u

alpha_equivalent(f::DeBrujinAbstraction, g::DeBrujinAbstraction) = f == g

alpha_equivalent(f::NamedAbstraction, g::NamedAbstraction) = 
    named_to_debrujin(f) == named_to_debrujin(g)

alpha_equivalent(s::Application, t::Application) =
    alpha_equivalent(operator(s), operator(t)) &&
    alpha_equivalent(operand(s), operand(t))

≃(x, y) = alpha_equivalent(x, y)

################
# α-conversion #
################

#=function alpha_convert(abs::NamedAbstraction, new_var::Variable)
    if !(new_var in free_vars(abs)) && !(new_var in bound_vars(abs))
        rename_var(abs, var(abs), new_var)
    else
        error("Invalid α-conversion: $new_var is free or bound in $abs")
    end
end

alpha_convert(abs::Abstraction, new_var::Symbol) =
    alpha_convert(abs, Variable(new_var))

rename_var(identifier::Identifier, from::Variable, to::Variable) =
    identifier == from ? to : identifier

rename_var(abs::Abstraction, from::Variable, to::Variable) = 
    lambda(rename_var(var(abs), from, to), rename_var(body(abs), from , to))

rename_var(app::Application, from::Variable, to::Variable) = 
    rename_var(operator(app), from, to)(rename_var(operand(app), from, to));=#


###############
# β-reduction #
###############

substitute(constant::Constant, from::Variable, to::LambdaTerm) = constant

substitute(var::Variable, from::Variable, to::LambdaTerm) =
    var == from ? to : var

function substitute(abs::Abstraction, from::Variable, to::LambdaTerm)
    if var(abs) == from 
        abs
    elseif !(var(abs) in free_vars(to) && from in free_vars(abs))
        lambda(var(abs), substitute(body(abs), from, to))
    else
        new_var = random_neither_free_nor_bound_var(abs, to, from)
        lambda(new_var,
               substitute(rename_var(body(abs), var(abs), new_var), from, to))
    end
end

substitute(app::Application, from::Variable, to::LambdaTerm) =
    substitute(operator(app), from, to)(substitute(operand(app), from, to))

function beta_reduce(app::Application)
    if operator(app) isa Abstraction
        substitute(body(operator(app)), var(operator(app)), operand(app))
    else
        app
    end
end

###############
# η-reduction #
###############

function is_eta_redex(abs::Abstraction)
    body(abs) isa Application && var(abs) == operand(body(abs)) 
end

eta_reduce(abs::Abstraction) = is_eta_redex(abs) ? operator(body(abs)) : abs

#############
# normalize #
#############

lambda_reduce(identifier::Identifier, _::Union{Nothing,LambdaTerm}=nothing) =
     identifier

function lambda_reduce(app::Application,
                       prev_term::Union{Nothing,LambdaTerm}=nothing)
    if app ≃ prev_term
        app
    else
        lambda_reduce(beta_reduce(apply(lambda_reduce(operator(app)),
                                        lambda_reduce(operand(app)))), app)
    end
end

function lambda_reduce(abs::Abstraction,
                       prev_term::Union{Nothing,LambdaTerm}=nothing)
    if abs ≃ prev_term
        abs
    else
        simplified = eta_reduce(abs)
        if simplified isa Abstraction
            lambda_reduce(lambda(var(simplified), lambda_reduce(body(simplified))), abs)
        else
            lambda_reduce(simplified)
        end
    end
end


