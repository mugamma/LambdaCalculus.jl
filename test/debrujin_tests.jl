
import LambdaCalculus: AtomicType, ArrowType, Variable, NamedAbstraction,
                       named_to_debrujin, debrujin_to_named, source_type, body,
                       type, DeBrujinIndex, DeBrujinAbstraction,
                       alpha_equivalent

@testset "De Brujin Indexing Tests" begin
    ind_t = AtomicType(:ind)
    arr_t = ArrowType(ind_t, ind_t)
    arr2_t = ArrowType(ind_t, ArrowType(ind_t, ind_t))
    x, y, z = map(s->Variable(s, ind_t), (:x, :y, :z))
    f = Variable(:f, arr2_t)
    g = Variable(:g, arr_t)

    I = NamedAbstraction(x, x)
    K = NamedAbstraction(x, NamedAbstraction(y, x))
    S = NamedAbstraction(f, NamedAbstraction(g, NamedAbstraction(z, 
           Application(Application(f, z), Application(g, z)))))
    dI, dK, dS = map(named_to_debrujin, (I, K, S))

    _1, _2, _3 = map(i->DeBrujinIndex(i, ind_t), (1, 2, 3))

    @testset "converting from named to indexed" begin
        @test source_type(dI) == ind_t
        @test body(dI) == _1
        @test type(dI) == ArrowType(ind_t, ind_t)

        @test source_type(dK) == ind_t
        @test body(dK) == DeBrujinAbstraction(ind_t, _2)
        @test type(dK) == ArrowType(ind_t, ArrowType(ind_t, ind_t))

        _3 = DeBrujinIndex(3, arr2_t)
        _2 = DeBrujinIndex(2, arr_t)
        @test source_type(dS) == arr2_t
        @test dS == DeBrujinAbstraction(arr2_t, 
                      DeBrujinAbstraction(arr_t, 
                        DeBrujinAbstraction(ind_t,
                          Application(Application(_3, _1),
                                      Application(_2, _1)))))
        @test type(dS) == ArrowType(arr2_t, ArrowType(arr_t, arr_t))
    end

    @testset "alpha-equivalence" begin
        bool_t = AtomicType(:bool)
        is_behind = Constant(:is_behind, ArrowType(ind_t, ArrowType(ind_t, bool_t)))
        @test alpha_equivalent(I, NamedAbstraction(y, y)) 
        @test alpha_equivalent(K, NamedAbstraction(y, NamedAbstraction(x, y)))
        @test alpha_equivalent(NamedAbstraction(x, NamedAbstraction(y,
                                   Application(Application(is_behind, x), y))),
                               NamedAbstraction(y, NamedAbstraction(x,
                                   Application(Application(is_behind, y), x))))
    end

    @testset "converting from indexed to named" begin
        nI, nK, nS = map(debrujin_to_named, (dI, dK, dS))
        @test type(dI) == type(I)
        @test alpha_equivalent(nI, I)

        @test type(nK) == type(nK)
        @test alpha_equivalent(nK, K)

        @test type(nS) == type(S)
        @test alpha_equivalent(nS, S)
    end
end
