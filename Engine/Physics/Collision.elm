module Engine.Physics.Collision exposing (..)

import Engine.Physics.Vec as Vec exposing (Vec2)
import Engine.Physics.Body exposing (..)
import Engine.Physics.Fixtures exposing (..)

type alias Manifold = {
    a: RigidBody,
    b: RigidBody,
    penetration: Float,
    normal: Vec2 
}

circleVsCircle: (RigidBody, Vec2, Float) -> (RigidBody, Vec2, Float) -> Maybe Manifold
circleVsCircle (a, posA, radA) (b, posB, radB) =
    let
        n = Vec.sub posB posA
        r = (radA + radB) ^ 2
        d = Vec.length n
    in
        if Vec.length2 n > r then
            Nothing
        else if d /= 0 then
            Just { a = a, b = b, penetration = (r - d), normal = Vec.normalize n }
        else
            Just { a = a, b = b, penetration = radA, normal = { x = 1, y = 0} }

aabbVsAABB: (RigidBody, Vec2, Vec2) -> (RigidBody, Vec2, Vec2) -> Maybe Manifold
aabbVsAABB (a, mina, maxa) (b, minb, maxb) =
    let
        posB = Vec.midpoint maxb minb
        posA = Vec.midpoint maxa mina
        n = Vec.sub posB posA
        -- Calculate half extents
        aExtent = (maxa.x - mina.x) / 2
        bExtent = (maxb.x - minb.x) / 2
        -- Caluclate Overlap
        xOverlap = aExtent + bExtent - abs(n.x)
    in
        -- SAT test on x axis
        if (xOverlap > 0) then
            let
                aExtent = (maxa.y - mina.y) / 2
                bExtent = (maxb.y - minb.y) / 2

                yOverlap = aExtent - bExtent - abs(n.y)
            in
                -- SAT test on y axis
                if yOverlap > 0 then
                    -- Find axis of least penetration
                    if xOverlap > yOverlap then
                        if n.x < 0 then
                            Just { a = a, b = b, penetration = xOverlap, normal = {x = -1, y = 0}}
                        else
                            Just { a = a, b = b, penetration = xOverlap, normal = {x = 1, y = 0}}
                    else
                        if n.y < 0 then
                            Just { a = a, b = b, penetration = yOverlap, normal = {x = 0, y = -1}}
                        else
                            Just { a = a, b = b, penetration = yOverlap, normal = {x = 0, y = 1}}
                else
                    Nothing
        else
            Nothing
                
aabbVsCircle: (RigidBody, Vec2, Vec2) -> (RigidBody, Vec2, Float) -> Maybe Manifold 
aabbVsCircle (a, mina, maxa) (b, posB, radius) =
    let
        posA = Vec.midpoint maxa mina
        n = Vec.sub posB posA

        xExtent = (maxa.x - mina.x) / 2
        yExtent = (maxa.y - mina.y) / 2

        -- Get closest point to edges of aabb
        closest = {
            x = clamp -xExtent xExtent n.x,
            y = clamp -yExtent yExtent n.y
        }
    in
        -- Circle is inside the AABB so clamp center to closest edge
        if n == closest then
        -- Find closes axis
            let
                edgeNormal = 
                    if ((abs n.x) > (abs n.y)) then 
                        if closest.x > 0 then
                            { x = xExtent, y = closest.y }
                        else
                            { x = -xExtent, y = closest.y }
                    else
                        if closest.y > 0 then
                            { x = closest.x, y = yExtent }
                        else 
                            { x = closest.x, y = -yExtent }
                newNormal = Vec.sub n edgeNormal
                d = Vec.length2 newNormal
            in
                if d > radius ^ 2 then
                    Nothing
                else
                    Just { a = a, b = b, penetration = radius - (sqrt d), normal = n}
        else
            Nothing


collisionCheck: RigidBody -> RigidBody -> Maybe Manifold
collisionCheck a b =
    case a.fixture of 
        (Circle posA radA) ->
            case b.fixture of 
                (Circle posB radB) -> circleVsCircle (a, posA, radA) (b, posB, radB)
                (AABB minb maxb) -> 
                -- Maintain order in is order out
                let
                    man = aabbVsCircle (b, minb, maxb) (a, posA, radA)
                in
                    case man of
                        Just m -> Just { m | a = m.b, b = m.a }
                        Nothing -> Nothing
        (AABB mina maxa) ->
            case b.fixture of
                (Circle posB radB) -> aabbVsCircle (a, mina, maxa) (b, posB, radB)
                (AABB minb maxb) -> aabbVsAABB (a, mina, maxa) (b, minb, maxb)

positionalCorrection: Manifold -> Manifold
positionalCorrection m =
    let
        percent = 0.2
        correction = Vec.mult m.normal ((m.penetration / (m.a.invMass + m.b.invMass)) * percent)
        a = m.a
        b = m.b
    in
        { m |
            a = { a | fixture = setPosition a.fixture (Vec.sub (getPosition a.fixture) (Vec.mult correction a.invMass))},
            b = { b | fixture = setPosition b.fixture (Vec.add (getPosition b.fixture) (Vec.mult correction b.invMass))}            
        }

resolveCollision: Manifold -> Manifold
resolveCollision m =
    let
        -- Calc relative velocity
        rv = Vec.sub m.b.velocity m.a.velocity
        velAlongNormal = Vec.dot rv m.normal
    in
        -- If moving away don't resolve
        if velAlongNormal > 0 then
            m
        else
            let
                elasticity = min m.a.restitution m.b.restitution
                -- Impulse scalar
                j = (-(1 + elasticity) * velAlongNormal) / (m.a.invMass + m.b.invMass)
                impulse = Vec.mult m.normal j
                a = m.a
                b = m.a
            in
                {m |
                    a = {a | velocity = Vec.mult impulse (-m.a.invMass)},
                    b = {b | velocity = Vec.mult impulse (m.b.invMass)}
                }


        