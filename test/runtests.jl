using Test

@testset "LambdaCalculus.jl" begin
    include("terms_tests.jl")
    include("debrujin_tests.jl")
    include("calculus_tests.jl")
end



