#################
# α-equivalence #
#################

"""
    alpha_equivalent(t::LambdaTerm, s::LambdaTerm)

Tell whether the terms `t` and `s` are alpha-equivalent.

Under the hood, free variables are checked for equality, and abstractions are
first expressed using De Bruijn indices and then checked for equality.
"""
alpha_equivalent(t::LambdaTerm, s::LambdaTerm) = false

alpha_equivalent(f::DeBruijnLambdaTerm, g::DeBruijnLambdaTerm) = f == g

alpha_equivalent(v::FreeVariable, u::FreeVariable) = v == u

alpha_equivalent(f::Abstraction, g::Abstraction) = 
    alpha_equivalent(map(named_to_debruijn, (f, g))...)

alpha_equivalent(s::Application, t::Application) =
    alpha_equivalent(operator(s), operator(t)) &&
    alpha_equivalent(operand(s), operand(t))

###############
# β-reduction #
###############

"""
    is_beta_redex(t::LambdaTerm)

Tell whether the term `t` is a beta redex.
"""
is_beta_redex(t::LambdaTerm) = is_beta_redex(named_to_debruijn(t))
is_beta_redex(a::DeBruijnLambdaTerm) = false
# an application exists iff it is type-checked
is_beta_redex(a::DeBruijnApplication) = operator(a) isa DeBruijnAbstraction 

"""
    beta_reduce(t::LambdaTerm)

Beta-reduce the term `t`, or return `t` if it is not a beta-redex.

Named lambda terms are not directly beta-reduced. They are first translated to
De Bruijn-indexed terms, beta-reduced, and then translated to a named term
again. This can be inefficient in applications where programmatically
constructed lambda terms are combined and beta reduced. For better performance,
translate the terms using `named_to_debruijn`, perform all the operations, and
translate back using `debruijn_to_named`.
"""
beta_reduce(t::LambdaTerm) = debruijn_to_named(beta_reduce(named_to_debruijn(t)))

"""
    beta_reduce(t::DeBruijnLambdaTerm)

Beta-reduce the De Bruijn-indexed term `t`, or return `t` if it is not a beta-redex.
"""
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
"""
    is_eta_redex(t::LambdaTerm)

Tell whether the term `t` is an eta redex.
"""
is_eta_redex(t::LambdaTerm) = is_eta_redex(named_to_debruijn(t))
is_eta_redex(a::DeBruijnLambdaTerm) = false

is_eta_redex(abs::DeBruijnAbstraction) =
    body(abs) isa DeBruijnApplication &&
    operand(body(abs)) isa DeBruijnIndex &&
    idx(operand(body(abs))) == 1 &&
    type(operand(body(abs))) == source_type(abs)

"""
    eta_reduce(t::LambdaTerm)

Eta-reduce the term `t`, or return `t` if it is not an eta-redex.

Named lambda terms are not directly eta-reduced. They are first translated to
De Bruijn-indexed terms, eta-reduced, and then translated to a named term
again. This can be inefficient in applications where programmatically
constructed lambda terms are combined and eta reduced. For better performance,
translate the terms using `named_to_debruijn`, perform all the operations, and
translate back using `debruijn_to_named`.
"""
eta_reduce(t::LambdaTerm) = debruijn_to_named(eta_reduce(named_to_debruijn(t)))

"""
    eta_reduce(t::DeBruijnLambdaTerm)

Eta-reduce the De Bruijn-indexed term `t`, or return `t` if it is not an eta-redex.
"""
eta_reduce(t::DeBruijnAbstraction) =
    is_eta_redex(t) ? operator(body(t)) - 1 : t

#################
# normalization #
#################

"""
    normalize(t::LambdaTerm)

Return the beta-eta-normal form associated with the term `t`.

Recursively beta- and eta- reduce `t` and all its subexpressions so that no
subexpression of `t` is a beta- or eta- redex.

Similar to `beta_reduce` and `eta_reduce`, working with named terms directly
can be inefficient.

"""
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
