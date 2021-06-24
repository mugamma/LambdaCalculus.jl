#########
# Misc. #
#########

function compose(f::Abstraction, g::Abstraction)
    new_var = random_neither_free_nor_bound_var(f, g)
    lambda(new_var, lambda_reduce(f(g(new_var))))
end

function beautify(term::LambdaTerm)
    term_vars = all_vars(term)
    nice_vars = setdiff(Set(map(i->Variable(Symbol('a' + i)), 0:25)), term_vars)
    reduce((term, v)-> rename_var(term, v, pop!(nice_vars)),
           filter(v->startswith(string(v), '#'), term_vars);
           init=term)
end


