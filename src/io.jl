######
# IO #
######

Base.string(v::Variable) = string(name(v))

Base.string(f::Abstraction) = "λ$(string(var(f))).$(string(body(f)))"

Base.string(a::Union{Application,DeBruijnApplication}) =
    "($(string(operator(a))) $(string(operand(a))))"

Base.string(i::DeBruijnIndex) = string(idx(i))

Base.string(f::DeBruijnAbstraction) = "λ $(string(body(f)))"

Base.show(io::IO, ::MIME"text/plain", exp::Union{LambdaTerm,DeBruijnLambdaTerm}) =
    print(io, string(exp))


