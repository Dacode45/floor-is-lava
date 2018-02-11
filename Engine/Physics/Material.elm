module Engine.Physics.Material exposing (..)

type alias Material = {
    density: Float,
    restitution: Float
}

material: Float -> Float -> Material
material density restitution =
    { density = density, restitution = restitution }

rock : Material
rock = material 0.6 0.1

wood : Material
wood = material 0.3 0.2

metal : Material
metal = material 1.2 0.05

bouncyBall: Material
bouncyBall = material 0.3 0.8

superBall: Material
superBall = material 0.3 0.95

pillow: Material
pillow = material 0.1 0.2

static: Material
static = material 0.0 0.4