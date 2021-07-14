@testset "untyped lambda calculus tests" begin
    import LambdaCalculus: UNTYPED, @variable, @free_variable, beta_reduce, λ,
                           type, ≃

    @variable x[UNTYPED] f[UNTYPED]
    @free_variable g[UNTYPED]

    @testset "Ω combinator" begin
        ω = λ(x, x(x))
        Ω = ω(ω)

        @test type(ω) == UNTYPED
        @test type(Ω) == UNTYPED
        @test beta_reduce(Ω) ≃ Ω
    end

end
