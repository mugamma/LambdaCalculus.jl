
import LambdaCalculus: AtomicType, ArrowType, FreeVariable, BoundVariable,
                       Abstraction, named_to_debruijn, debruijn_to_named,
                       source_type, body, type, FreeDeBruijnIndex,
                       BoundDeBruijnIndex, DeBruijnAbstraction, idx, context,
                       DeBruijnApplication, alpha_equivalent, GLOBAL_CONTEXT,
                       free_vars, LambdaTypeError, check_context

@testset "De Bruijn Indexing Tests" begin
    ind_t = AtomicType(:ind)
    arr_t = ArrowType(ind_t, ind_t)
    arr2_t = ArrowType(ind_t, ArrowType(ind_t, ind_t))

    empty!(free_vars(GLOBAL_CONTEXT))

    x, y, z = map(s->BoundVariable(s, ind_t), (:x, :y, :z))
    f = BoundVariable(:f, arr2_t)
    g = BoundVariable(:g, arr_t)

    fn = FreeVariable(:fn, arr_t)
    pt = FreeVariable(:pt, ind_t)

    fnpt = Application(fn, pt)
    I = Abstraction(x, x)
    K = Abstraction(x, Abstraction(y, x))
    S = Abstraction(f, Abstraction(g, Abstraction(z, 
           Application(Application(f, z), Application(g, z)))))
    dfnpt = named_to_debruijn(fnpt)
    dI, dK, dS = map(named_to_debruijn, (I, K, S))


    dbi(n, t) = BoundDeBruijnIndex(n, t, GLOBAL_CONTEXT)
    dbabs(st, b) = DeBruijnAbstraction(st, b, GLOBAL_CONTEXT)
    dbapp(opr, opd) = DeBruijnApplication(opr, opd, GLOBAL_CONTEXT)

    _1, _2, _3 = map(i->dbi(i, ind_t), 1:3)
    _4 = dbi(4, arr2_t)
    _5 = dbi(5, arr_t)

    @test_throws LambdaTypeError dbapp(_1, _2)

    @testset "converting from named to indexed" begin
        @test named_to_debruijn(fn) isa FreeDeBruijnIndex
        @test type(named_to_debruijn(fn)) == arr_t
        @test named_to_debruijn(fn) == FreeDeBruijnIndex(1, arr_t, GLOBAL_CONTEXT)
        @test context(named_to_debruijn(fn)) == GLOBAL_CONTEXT

        @test dfnpt isa DeBruijnApplication
        @test type(dfnpt) == ind_t
        @test operator(dfnpt) == FreeDeBruijnIndex(1, arr_t, GLOBAL_CONTEXT)
        @test operand(dfnpt) == FreeDeBruijnIndex(2, ind_t, GLOBAL_CONTEXT)
        @test context(dfnpt) == GLOBAL_CONTEXT


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
        is_behind = FreeVariable(:is_behind, ArrowType(ind_t, ArrowType(ind_t, bool_t)))
        @test alpha_equivalent(I, Abstraction(y, y)) 
        @test alpha_equivalent(K, Abstraction(y, Abstraction(x, y)))
        @test alpha_equivalent(Abstraction(x, Abstraction(y,
                                   Application(Application(is_behind, x), y))),
                               Abstraction(y, Abstraction(x,
                                   Application(Application(is_behind, y), x))))
    end

    @testset "converting from indexed to named" begin
        nI, nK, nS = map(debruijn_to_named, (dI, dK, dS))

        @test alpha_equivalent(debruijn_to_named(_1), fn)

        @test alpha_equivalent(debruijn_to_named(dfnpt), fnpt)

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
        _2, _3 = map(x->FreeDeBruijnIndex(x, arr_t, GLOBAL_CONTEXT), (2, 3))
        _5 = FreeDeBruijnIndex(5, arr_t, GLOBAL_CONTEXT)
        _6 = FreeDeBruijnIndex(6, ind_t, GLOBAL_CONTEXT)
        s = dbabs(ind_t, dbapp(_2, dbapp(_2, _1)))
        t = dbabs(ind_t, dbapp(_3, dbapp(_3, _1)))
        @test s + 1 == t
        @test dfnpt + 4 == dbapp(_5, _6)
    end
end
