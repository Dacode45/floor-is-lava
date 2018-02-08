module Engine.Component exposing (..)

import Engine.Physics exposing (Vec2)

type Component =
    Position Vec2
    | Velocity Vec2