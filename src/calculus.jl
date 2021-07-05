#################
# α-equivalence #
#################

alpha_equivalent(f::DeBrujinLambdaTerm, g::DeBrujinLambdaTerm) = f == g

alpha_equivalent(v::Identifier, u::Identifier) = v == u

alpha_equivalent(f::Abstraction, g::Abstraction) = 
    alpha_equivalent(map(named_to_debrujin, (f, g))...)

alpha_equivalent(s::Application, t::Application) =
    alpha_equivalent(operator(s), operator(t)) &&
    alpha_equivalent(operand(s), operand(t))

###############
# β-reduction #
###############

is_beta_redex(t::LambdaTerm) = is_beta_redex(named_to_debrujin(t))
is_beta_redex(a::DeBrujinLambdaTerm) = false
# an application exists iff it is type-checked
is_beta_redex(a::DeBrujinApplication) = operator(a) isa DeBrujinAbstraction 

beta_reduce(t::LambdaTerm) = debrujin_to_named(beta_reduce(named_to_debrujin(t)))

function beta_reduce(t::DeBrujinLambdaTerm)
    substitute(i::DeBrujinIndex, t::DeBrujinLambdaTerm, depth) =
        idx(i) == depth ? t : idx(i) > depth ? i - 1 : i
    substitute(app::DeBrujinApplication, t::DeBrujinLambdaTerm, depth) =
        DeBrujinApplication(substitute(operator(app), t, depth),
                            substitute(operand(app), t, depth), 
                            context(app))
    substitute(abs::DeBrujinAbstraction, t::DeBrujinLambdaTerm, depth) =
        DeBrujinAbstraction(source_type(abs),
                            substitute(body(abs), t+1, depth+1), 
                            context(abs))
    
    is_beta_redex(t) ? substitute(body(operator(t)), operand(t), 1) : t
end

###############
# η-reduction #
###############

is_eta_redex(t::LambdaTerm) = is_eta_redex(named_to_debrujin(t))
is_eta_redex(a::DeBrujinLambdaTerm) = false

is_eta_redex(abs::DeBrujinAbstraction) =
    body(abs) isa DeBrujinApplication &&
    operand(body(abs)) isa DeBrujinIndex &&
    idx(operand(body(abs))) == 1 &&
    type(operand(body(abs))) == source_type(abs)

eta_reduce(t::LambdaTerm) = debrujin_to_named(eta_reduce(named_to_debrujin(t)))

eta_reduce(t::DeBrujinAbstraction) =
    is_eta_redex(t) ? operator(body(t)) - 1 : t

#################
# normalization #
#################

normalize(t::LambdaTerm) = debrujin_to_named(normalize(named_to_debrujin(t)))

normalize(i::DeBrujinIndex) = i
function normalize(app::DeBrujinApplication)
    simplified = DeBrujinApplication(normalize(operator(app)),
                                     normalize(operand(app)),
                                     context(app))
    is_beta_redex(simplified) ? normalize(beta_reduce(simplified)) : simplified
end
function normalize(abs::DeBrujinAbstraction)
    simplified = DeBrujinAbstraction(source_type(abs), normalize(body(abs)), context(abs))
    is_eta_redex(simplified) ? normalize(eta_reduce(simplified)) : simplified
end
