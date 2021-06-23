######
# IO #
######

Base.string(identifier::Identifier) = string(name(identifier))

Base.string(f::Abstraction) = "λ$(var(f)).$(string(body(f)))"

Base.string(a::Application) = "($(string(operator(a))) $(string(operand(a))))"

Base.show(io::IO, exp::LambdaTerm) = print(io, string(exp))


