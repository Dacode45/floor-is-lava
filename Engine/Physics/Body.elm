module Engine.Physics.Body exposing (..)

import Engine.Physics.Fixtures exposing (..)
import Engine.Physics.Vec exposing (..)

type alias RigidBody = {
    fixture: Fixture,
    invMass: Float,
    restitution: Float,
    velocity: Vec2
}