import LambdaCalculus: @atomic_type, @variable, λ, ≃, normalize

@testset "DSL tests" begin

    @atomic_type ind_t

    fn_t = ind_t=>ind_t
    numeral_t = fn_t=>fn_t

    @variable x[ind_t] g[fn_t] n[numeral_t] m[numeral_t]


    repeat_g(i) = i == 0 ? x : g(repeat_g(i - 1))
    numerals = map(i->λ(g, λ(x, repeat_g(i))), 1:10)

    succ = λ(n, λ(g, λ(x, n(g)(g(x)))))
    add = λ(n, λ(m, λ(g, λ(x, m(g)(n(g)(x))))))
    prod = λ(n, λ(m, λ(g, λ(x, m(n(g))(x)))))

    @testset "normalization" begin
        @test normalize(succ(numerals[1])) ≃ numerals[2]
        @test normalize(succ(numerals[2])) ≃ numerals[3]
        @test normalize(add(numerals[2])(numerals[4])) ≃ numerals[6]
        @test normalize(add(numerals[3])(numerals[5])) ≃ numerals[8]
        @test normalize(prod(numerals[2])(numerals[4])) ≃ numerals[8]
    end


end
