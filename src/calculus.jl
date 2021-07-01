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

≃(x, y) = alpha_equivalent(x, y)

###############
# β-reduction #
###############

is_beta_redex(t::LambdaTerm) = is_beta_redex(named_to_debrujin(t))
is_beta_redex(a::DeBrujinLambdaTerm) = false
# an application exists iff it is type-checked
is_beta_redex(a::DeBrujinApplication) = operator(a) isa DeBrujinAbstraction 

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
