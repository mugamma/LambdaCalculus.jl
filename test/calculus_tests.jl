@testset "Reduction and Normalization Tests" begin
    import LambdaCalculus: AtomicType, ArrowType, Constant, Variable,
                           Abstraction, VariableReference, body, type,
                           alpha_equivalent, is_eta_redex, eta_reduce,
                           is_beta_redex, beta_reduce, GLOBAL_CONTEXT,
                           identifiers, named_to_debrujin, normalize

    empty!(identifiers(GLOBAL_CONTEXT))

    ind_t = AtomicType(:ind)
    fn_t = ArrowType(ind_t, ind_t)
    f = Variable(:f, ArrowType(ind_t, fn_t))
    g = Variable(:g, fn_t)
    x, y = map(s->Variable(s, ind_t), (:x, :y))
    v = VariableReference(:v)

    I = Abstraction(x, x)
    K = Abstraction(x, Abstraction(y, x))
    S = Abstraction(f, Abstraction(g, Abstraction(x,
          Application(Application(f, x),
                      Application(g, x)))))


    repeat_g(i) = i == 0 ? x : Application(g, repeat_g(i - 1))
    numerals = map(i->Abstraction(g, Abstraction(x, repeat_g(i))), 1:10)
    numeral_t = ArrowType(fn_t, fn_t)
    n, m = map(s->Variable(s, numeral_t), (:n, :m))

    succ = Abstraction(n, Abstraction(g, Abstraction(x,
             Application(Application(n, g), Application(g, x)))))
    add = Abstraction(n, Abstraction(m, Abstraction(g, Abstraction(x,
            Application(Application(m, g), Application(Application(n, g), x))))))
    prod = Abstraction(n, Abstraction(m, Abstraction(g, Abstraction(x,
                Application(Application(n, Application(m, g)), x)))))

    @testset "beta reduction" begin
        @test !is_beta_redex(Application(g, x))
        @test !is_beta_redex(g)
        @test !is_beta_redex(Abstraction(x, x))
        @test is_beta_redex(Application(Abstraction(x, x), x))

        @test alpha_equivalent(beta_reduce(x), x)
        @test alpha_equivalent(beta_reduce(Application(I, x)), x)
        @test alpha_equivalent(beta_reduce(Application(I, y)), y)
        @test alpha_equivalent(beta_reduce(Application(
               Abstraction(v[type(S)], v[type(S)]), S)), S)

        x_fn = beta_reduce(Application(K, x))
        @test alpha_equivalent(x_fn, Abstraction(v[ind_t], x))
        @test alpha_equivalent(beta_reduce(Application(x_fn, y)), x)
    end

    @testset "eta reduction" begin
        @test !is_eta_redex(g) 
        @test is_eta_redex(Abstraction(x, Application(g, x)))
        @test alpha_equivalent(eta_reduce(Abstraction(x, Application(g, x))), g)
    end

    @testset "normalization" begin
        @test alpha_equivalent(normalize(Application(succ, numerals[1])), numerals[2])
        @test alpha_equivalent(normalize(Application(succ, numerals[2])), numerals[3])
        @test alpha_equivalent(normalize(Application(Application(add, numerals[2]),
                                                     numerals[4])), numerals[6])
        @test alpha_equivalent(normalize(Application(Application(add, numerals[3]),
                                                     numerals[5])), numerals[8])
        @test alpha_equivalent(normalize(Application(Application(prod, numerals[2]),
                                                     numerals[3])), numerals[6])
        @test alpha_equivalent(normalize(Application(Application(prod, numerals[2]),
                                                     numerals[4])), numerals[8])
    end
end
