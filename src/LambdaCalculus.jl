module LambdaCalculus

# calculus operations
export alpha_equivalent, is_beta_redex, beta_reduce, is_eta_redex, eta_reduce,
       normalize

# term and De Bruijn-indexed accessors
export UNTYPED, source, target, name, type, context, var, body, GLOBAL_CONTEXT,
       LambdaTypeError, source_type, operator, operand, free_vars, bound_vars,
       all_vars, debruijn_to_named, named_to_debruijn

# DSL 
export @atomic_type, @variable, @free_variable, @context, ≃, λ, lambda

include("terms.jl")

include("debruijn.jl")

include("calculus.jl")

include("io.jl")

include("dsl.jl")

end 
