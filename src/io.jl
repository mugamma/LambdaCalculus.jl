######
# IO #
######

Base.string(id::Identifier) = string(name(id))

Base.string(f::Abstraction) = "λ$(string(var(f))).$(string(body(f)))"

Base.string(a::Union{Application,DeBrujinApplication}) =
    "($(string(operator(a))) $(string(operand(a))))"

Base.string(i::DeBrujinIndex) = string(idx(i))

Base.string(f::DeBrujinAbstraction) = "λ $(string(body(f)))"

Base.string(t::AtomicType) = string(name(t))
function Base.string(t::ArrowType)
    srcstr = source(t) isa AtomicType ? string(source(t)) : "($(string(source(t))))"
    srcstr * " => $(string(target(t)))"
end

Base.show(io::IO, ::MIME"text/plain", exp::Union{LambdaTerm,DeBruijnLambdaTerm}) =
    print(io, string(exp))


