@testset "lambda terms" begin
    import LambdaCalculus: AtomicType, ArrowType, source, target, type, name,
                           FreeVariable, BoundVariable, 
                           Abstraction, var, body, Application, operator,
                           operand, LambdaTypeError, free_vars, bound_vars,
                           all_vars, GLOBAL_CONTEXT
   
    empty!(free_vars(GLOBAL_CONTEXT))

    ind_t = AtomicType(:ind)
    bool_t = AtomicType(:bool)
    @testset "atomic types" begin
        @test name(ind_t) == :ind
        @test name(bool_t) == :bool
    end

    prop_t = ArrowType(ind_t, bool_t)
    @testset "arrow types" begin
        @test source(prop_t) == ind_t
        @test target(prop_t) == bool_t
    end

    socrates = FreeVariable(:socrates, ind_t)
    true_ = FreeVariable(:true_, bool_t)
    false_ = FreeVariable(:false_, bool_t)
    @testset "constants" begin
        @test name(socrates) == :socrates
        @test type(socrates) == ind_t
        @test name(true_) == :true_
        @test type(true_) == bool_t
        @test name(false_) == :false_
        @test type(false_) == bool_t
    end

    person = BoundVariable(:person, ind_t)
    truth = BoundVariable(:truth, bool_t)
    @testset "variables" begin
        @test name(person) == :person
        @test type(person) == ind_t
        @test name(truth) == :truth
        @test type(truth) == bool_t
    end

    self = Abstraction(person, person)
    is_mortal = Abstraction(person, true_)

    @testset "named abstractions" begin
        @test var(self) == person
        @test body(self) == person
        @test type(self) == ArrowType(ind_t, ind_t)
        @test var(is_mortal) == person
        @test body(is_mortal) == true_
        @test type(is_mortal) == prop_t
    end

    socrates_self = Application(self, socrates)
    socrates_is_mortal = Application(is_mortal, socrates)

    @testset "application" begin
        @test_throws LambdaTypeError Application(self, is_mortal)
        @test socrates_self isa Application
        @test operator(socrates_self) == self
        @test operand(socrates_self) == socrates
        @test type(socrates_self) == ind_t
        @test socrates_is_mortal isa Application
        @test operator(socrates_is_mortal) == is_mortal
        @test operand(socrates_is_mortal) == socrates
        @test type(socrates_is_mortal) == bool_t
    end

        arr_t = ArrowType(ind_t, ind_t)
        arr2_t = ArrowType(ind_t, ArrowType(ind_t, ind_t))
        x, y, z = map(s->BoundVariable(s, ind_t), (:x, :y, :z))
        f = BoundVariable(:f, arr2_t)
        g = BoundVariable(:g, arr_t)

        I = Abstraction(x, x)
        K = Abstraction(x, Abstraction(y, x))
        S = Abstraction(f, Abstraction(g, Abstraction(z, 
               Application(Application(f, z), Application(g, z)))))

    @testset "combinators" begin
        @test type(I) == arr_t
        @test type(K) == arr2_t
        @test type(S) == ArrowType(arr2_t, ArrowType(arr_t, arr_t))
    end

    @testset "free and bound variables" begin
        @test isempty(free_vars(I))
        @test isempty(free_vars(K))
        @test isempty(free_vars(S))
        @test socrates in free_vars(socrates_is_mortal) 
        @test true_ in free_vars(socrates_is_mortal) 
        @test person in bound_vars(socrates_is_mortal)
        @test socrates in all_vars(socrates_is_mortal) 
        @test true_ in all_vars(socrates_is_mortal) 
        @test person in all_vars(socrates_is_mortal)

    end
end
