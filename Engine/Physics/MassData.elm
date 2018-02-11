module Engine.Physics.MassData exposing (..)

type alias MassData = {
    invMass: Float
    -- Not used
    -- inertia: Float
}

massData: Float -> MassData
massData invMass =
    { invMass = invMass }