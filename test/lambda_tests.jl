include("../src/LambdaCalculus.jl")

import LambdaCalculus: lambda, compose, alpha_equivalent


@testset "Testing lambda composition" begin
    f = lambda(:x, :foo(:x))
    g = lambda(:x, :bar(:x))
    @test alpha_equivalent(compose(f, g), lambda(:x, :foo(:bar(:x))))
end

