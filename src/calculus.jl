#################
# α-equivalence #
#################

alpha_equivalent(f::DeBruijnLambdaTerm, g::DeBruijnLambdaTerm) = f == g

alpha_equivalent(v::FreeVariable, u::FreeVariable) = v == u

alpha_equivalent(f::Abstraction, g::Abstraction) = 
    alpha_equivalent(map(named_to_debruijn, (f, g))...)

alpha_equivalent(s::Application, t::Application) =
    alpha_equivalent(operator(s), operator(t)) &&
    alpha_equivalent(operand(s), operand(t))

≃(x, y) = alpha_equivalent(x, y)

###############
# β-reduction #
###############

is_beta_redex(t::LambdaTerm) = is_beta_redex(named_to_debruijn(t))
is_beta_redex(a::DeBruijnLambdaTerm) = false
# an application exists iff it is type-checked
is_beta_redex(a::DeBruijnApplication) = operator(a) isa DeBruijnAbstraction 

beta_reduce(t::LambdaTerm) = debruijn_to_named(beta_reduce(named_to_debruijn(t)))

function beta_reduce(t::DeBruijnLambdaTerm)
    substitute(i::DeBruijnIndex, t::DeBruijnLambdaTerm, depth) =
        idx(i) == depth ? t : idx(i) > depth ? i - 1 : i
    substitute(app::DeBruijnApplication, t::DeBruijnLambdaTerm, depth) =
        DeBruijnApplication(substitute(operator(app), t, depth),
                            substitute(operand(app), t, depth), 
                            context(app))
    substitute(abs::DeBruijnAbstraction, t::DeBruijnLambdaTerm, depth) =
        DeBruijnAbstraction(source_type(abs),
                            substitute(body(abs), t+1, depth+1), 
                            context(abs))
    
    is_beta_redex(t) ? substitute(body(operator(t)), operand(t), 1) : t
end

###############
# η-reduction #
###############

is_eta_redex(t::LambdaTerm) = is_eta_redex(named_to_debruijn(t))
is_eta_redex(a::DeBruijnLambdaTerm) = false

is_eta_redex(abs::DeBruijnAbstraction) =
    body(abs) isa DeBruijnApplication &&
    operand(body(abs)) isa DeBruijnIndex &&
    idx(operand(body(abs))) == 1 &&
    type(operand(body(abs))) == source_type(abs)

eta_reduce(t::LambdaTerm) = debruijn_to_named(eta_reduce(named_to_debruijn(t)))

eta_reduce(t::DeBruijnAbstraction) =
    is_eta_redex(t) ? operator(body(t)) - 1 : t

#################
# normalization #
#################

normalize(t::LambdaTerm) = debruijn_to_named(normalize(named_to_debruijn(t)))

normalize(i::DeBruijnIndex) = i
function normalize(app::DeBruijnApplication)
    simplified = DeBruijnApplication(normalize(operator(app)),
                                     normalize(operand(app)),
                                     context(app))
    is_beta_redex(simplified) ? normalize(beta_reduce(simplified)) : simplified
end
function normalize(abs::DeBruijnAbstraction)
    simplified = DeBruijnAbstraction(source_type(abs), normalize(body(abs)), context(abs))
    is_eta_redex(simplified) ? normalize(eta_reduce(simplified)) : simplified
end
