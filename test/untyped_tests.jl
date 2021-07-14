@testset "untyped lambda calculus tests" begin
    import LambdaCalculus: UNTYPED, @variable, @free_variable, beta_reduce, λ,
                           type, ≃, normalize

    @variable x[UNTYPED] y[UNTYPED] f[UNTYPED] n[UNTYPED] m[UNTYPED]
    @free_variable g[UNTYPED]

    @testset "Ω combinator" begin
        U = λ(x, x(x))
        Ω = U(U)

        @test type(U) == UNTYPED
        @test type(Ω) == UNTYPED
        @test beta_reduce(Ω) ≃ Ω
    end

    @testset "Numerals" begin
        repeat_f(i) = i == 0 ? x : f(repeat_f(i - 1))
    numerals = map(i->λ(f, λ(x, repeat_f(i))), 1:10)

    succ = λ(n, λ(f, λ(x, n(f)(f(x)))))
    add = λ(n, λ(m, λ(f, λ(x, n(f)(m(f)(x))))))
    prod = λ(n, λ(m, λ(f, λ(x, n(m(f))(x)))))
    exp = λ(m, λ(n, n(m)))

    @test normalize(succ(numerals[1])) ≃ numerals[2]
    @test normalize(succ(numerals[4])) ≃ numerals[5]
    @test normalize(add(numerals[4])(numerals[3])) ≃ numerals[7]
    @test normalize(prod(numerals[2])(numerals[5])) ≃ numerals[10]
    @test normalize(exp(numerals[2])(numerals[3])) ≃ numerals[8]
    @test normalize(exp(numerals[3])(numerals[2])) ≃ numerals[9]
    end

end
