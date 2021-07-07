
import LambdaCalculus: AtomicType, ArrowType, Variable, Abstraction,
                       named_to_debrujin, debrujin_to_named, source_type, body,
                       type, DeBrujinIndex, DeBrujinAbstraction, idx, context,
                       DeBrujinApplication, alpha_equivalent, GLOBAL_CONTEXT,
                       identifiers

@testset "De Brujin Indexing Tests" begin
    ind_t = AtomicType(:ind)
    arr_t = ArrowType(ind_t, ind_t)
    arr2_t = ArrowType(ind_t, ArrowType(ind_t, ind_t))

    empty!(identifiers(GLOBAL_CONTEXT))

    x, y, z = map(s->Variable(s, ind_t), (:x, :y, :z))
    f = Variable(:f, arr2_t)
    g = Variable(:g, arr_t)

    gx = Application(g, x)
    I = Abstraction(x, x)
    K = Abstraction(x, Abstraction(y, x))
    S = Abstraction(f, Abstraction(g, Abstraction(z, 
           Application(Application(f, z), Application(g, z)))))
    dgx = named_to_debrujin(gx)
    dI, dK, dS = map(named_to_debrujin, (I, K, S))

    _1, _2, _3 = map(i->DeBrujinIndex(i, ind_t, GLOBAL_CONTEXT), 1:3)
    _4 = DeBrujinIndex(4, arr2_t, GLOBAL_CONTEXT)
    _5 = DeBrujinIndex(5, arr_t, GLOBAL_CONTEXT)

    dbi(n, t) = DeBrujinIndex(n, t, GLOBAL_CONTEXT)
    dbabs(st, b) = DeBrujinAbstraction(st, b, GLOBAL_CONTEXT)
    dbapp(opr, opd) = DeBrujinApplication(opr, opd, GLOBAL_CONTEXT)


    @testset "converting from named to indexed" begin
        @test named_to_debrujin(x) isa DeBrujinIndex
        @test type(named_to_debrujin(x)) == ind_t
        @test named_to_debrujin(x) == _1
        @test context(named_to_debrujin(x)) == GLOBAL_CONTEXT

        @test dgx isa DeBrujinApplication
        @test type(dgx) == ind_t
        @test operator(dgx) == _5
        @test operand(dgx) == _1
        @test context(dgx) == GLOBAL_CONTEXT


        @test source_type(dI) == ind_t
        @test body(dI) == _1
        @test type(dI) == ArrowType(ind_t, ind_t)

        @test source_type(dK) == ind_t
        @test body(dK) == dbabs(ind_t, _2)
        @test type(dK) == ArrowType(ind_t, ArrowType(ind_t, ind_t))

        @test source_type(dS) == arr2_t
        @test dS == dbabs(arr2_t, dbabs(arr_t, dbabs(ind_t,
                 dbapp(dbapp(dbi(3, arr2_t), _1), dbapp(dbi(2, arr_t), _1)))))
        @test type(dS) == ArrowType(arr2_t, ArrowType(arr_t, arr_t))
    end

    @testset "alpha-equivalence" begin
        bool_t = AtomicType(:bool)
        is_behind = Constant(:is_behind, ArrowType(ind_t, ArrowType(ind_t, bool_t)))
        @test alpha_equivalent(I, Abstraction(y, y)) 
        @test alpha_equivalent(K, Abstraction(y, Abstraction(x, y)))
        @test alpha_equivalent(Abstraction(x, Abstraction(y,
                                   Application(Application(is_behind, x), y))),
                               Abstraction(y, Abstraction(x,
                                   Application(Application(is_behind, y), x))))
    end

    @testset "converting from indexed to named" begin
        nI, nK, nS = map(debrujin_to_named, (dI, dK, dS))

        @test alpha_equivalent(debrujin_to_named(_1), x)

        @test alpha_equivalent(debrujin_to_named(dgx), gx)

        @test type(dI) == type(I)
        @test alpha_equivalent(nI, I)

        @test type(nK) == type(nK)
        @test alpha_equivalent(nK, K)

        @test type(nS) == type(S)
        @test alpha_equivalent(nS, S)
    end

    @testset "index shifting" begin
        @test _1 + 2 == _3
        @test _3 - 1 == _2
        @test dI + 4 == dI
        _2 = DeBrujinIndex(2, arr_t, GLOBAL_CONTEXT)
        _3 = DeBrujinIndex(3, arr_t, GLOBAL_CONTEXT)
        s = dbabs(ind_t, dbapp(_2, dbapp(_2, _1)))
        t = dbabs(ind_t, dbapp(_3, dbapp(_3, _1)))
        @test s + 1 == t
        @test dgx + 4 == dbapp(dbi(9, arr_t), dbi(5, ind_t))
    end
end
