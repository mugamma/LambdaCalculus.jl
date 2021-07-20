u = [cos(π/3), sin(π/3)]
v = [cos(π/3), 0]
s = [0, sin(π/3)]

ux = [cos(π/3), -sin(π/3)]
uy = [-cos(π/3), sin(π/3)]


scale = 400

coords(x) = "$(x[1] * scale),$(x[2] * scale)"

greens = join(map(coords, [u, u + v, v + 0.1u, 0.1u]), " ")
purples = join(map(coords, [u, u + v, 2u + v, 2u]), " ")
reds = join(map(coords, [u, u + v, u + v + uy, u + uy]), " ")


svg = """
<!DOCTYPE html>
<html>
<body>

<svg height="$(2scale)" width="$(2scale)">
    <polygon points="$(reds...)" style="fill:#CB3C33;" />
    <polygon points="$(greens...)" style="fill:#389826;" />
    <polygon points="$(purples...)" style="fill:#9558B2;" />
</svg>

</body>
</html>
"""

println(svg)
