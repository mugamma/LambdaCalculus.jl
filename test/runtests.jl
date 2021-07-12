using Test

@testset "LambdaCalculus.jl" begin
    include("terms_tests.jl")
    include("debruijn_tests.jl")
    include("calculus_tests.jl")
    include("io_tests.jl")
    include("dsl_tests.jl")
end
