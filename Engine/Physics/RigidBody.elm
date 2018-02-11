module Engine.Physics.RigidBody exposing (..)

import Engine.Physics.MassData exposing (..)
import Engine.Physics.Material exposing (..)
import Engine.Physics.Fixtures as Fixtures exposing (Fixture)
import Engine.Physics.Vec exposing (..)

type alias RigidBody = {
    fixture: Fixture,
    material: Material,
    velocity: Vec2,
    force: Vec2,
    gravityScale: Float
}

getInvMass: RigidBody -> Float
getInvMass r = 
    Fixtures.getInvMass r.fixture r.material.density