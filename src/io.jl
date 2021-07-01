######
# IO #
######

Base.string(id::Identifier) = string(name(id))

Base.string(f::Abstraction) = "λ$(string(var(f))).$(string(body(f)))"

Base.string(a::Union{Application,DeBrujinApplication}) =
    "($(string(operator(a))) $(string(operand(a))))"

Base.string(i::DeBrujinIndex) = string(idx(i))

Base.string(f::DeBrujinAbstraction) = "λ $(string(body(f)))"

Base.show(io::IO, ::MIME"text/plain", exp::Union{LambdaTerm,DeBrujinLambdaTerm}) =
    print(io, string(exp))


