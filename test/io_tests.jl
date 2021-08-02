@testset "io" begin

    import LambdaCalculus: AtomicType, ArrowType, Variable, Abstraction,
                           Application, operator, named_to_debrujin,
                           GLOBAL_CONTEXT, identifiers
    
    empty!(identifiers(GLOBAL_CONTEXT))

    ind_t = AtomicType(:ind_t)
    arr_t = ArrowType(ind_t, ind_t)
    arr2_t = ArrowType(ind_t, ArrowType(ind_t, ind_t))
    x, y, z = map(s->Variable(s, ind_t), (:x, :y, :z))
    f = Variable(:f, arr2_t)
    g = Variable(:g, arr_t)

    I = Abstraction(x, x)
    K = Abstraction(x, Abstraction(y, x))
    S = Abstraction(f, Abstraction(g, Abstraction(z, 
           Application(Application(f, z), Application(g, z)))))

    dI, dK, dS = map(named_to_debrujin, (I, K, S))

    @test string(I) == "λx.x"
    @test string(K) == "λx.λy.x"
    @test string(S) == "λf.λg.λz.((f z) (g z))"

    @test string(dI) == "λ 1"
    @test string(dK) == "λ λ 2"
    @test string(dS) == "λ λ λ ((3 1) (2 1))"

    @test string(ind_t) == "ind_t"
    @test string(arr_t) == "ind_t => ind_t"
    @test string(arr2_t) == "ind_t => ind_t => ind_t"
    @test string(ArrowType(arr_t, ind_t)) == "(ind_t => ind_t) => ind_t"

    buf = IOBuffer()

    show(buf, MIME("text/plain"), I)
    @test String(take!(buf)) == "λx.x"
    show(buf, MIME("text/plain"), K)
    @test String(take!(buf)) == "λx.λy.x"
    show(buf, MIME("text/plain"), S)
    @test String(take!(buf)) == "λf.λg.λz.((f z) (g z))"

    show(buf, MIME("text/plain"), dI)
    @test String(take!(buf)) == "λ 1"
    show(buf, MIME("text/plain"), dK)
    @test String(take!(buf)) == "λ λ 2"
    show(buf, MIME("text/plain"), dS)
    @test String(take!(buf)) == "λ λ λ ((3 1) (2 1))"

end

