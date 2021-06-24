include("../src/LambdaCalculus.jl")

import LambdaCalculus: AtomicType, ArrowType, source, target, type, name,
                       Constant, Variable, VariableReference, annotate,
                       Abstraction, var, body, Application, operator, operand,
                       LambdaTypeError


@testset "lambda terms" begin
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

    socrates = Constant(:socrates, ind_t)
    true_ = Constant(:true_, bool_t)
    false_ = Constant(:false_, bool_t)
    @testset "constants" begin
        @test name(socrates) == :socrates
        @test type(socrates) == ind_t
        @test name(true_) == :true_
        @test type(true_) == bool_t
        @test name(false_) == :false_
        @test type(false_) == bool_t
    end

    person = Variable(:person, ind_t)
    truth = Variable(:truth, bool_t)
    @testset "variables" begin
        @test name(person) == :person
        @test type(person) == ind_t
        @test name(truth) == :truth
        @test type(truth) == bool_t
    end

    self = Abstraction(person, person)
    is_mortal = Abstraction(person, true_)

    @testset "abstractions" begin
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
end

