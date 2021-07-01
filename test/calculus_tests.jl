
@testset "Reduction and Normalization Tests" begin
    import LambdaCalculus: AtomicType, ArrowType, Constant, Variable,
                           Abstraction, body, type, alpha_equivalent,
                           is_eta_redex, eta_reduce, is_beta_redex,
                           GLOBAL_CONTEXT, identifiers, named_to_debrujin

    empty!(identifiers(GLOBAL_CONTEXT))

    ind_t = AtomicType(:ind)
    fn_t = ArrowType(ind_t, ind_t)
    f = Constant(:f, fn_t)
    x = Variable(:x, ind_t)

    @testset "beta reduction" begin
        @test !is_beta_redex(Application(f, x))
        @test !is_beta_redex(f)
        @test !is_beta_redex(Abstraction(x, x))
        @test is_beta_redex(Application(Abstraction(x, x), x))
    end

    @testset "eta reduction" begin
        @test !is_eta_redex(f) 
        @test is_eta_redex(Abstraction(x, Application(f, x)))
        @test alpha_equivalent(eta_reduce(Abstraction(x, Application(f, x))), f)
    end
end
