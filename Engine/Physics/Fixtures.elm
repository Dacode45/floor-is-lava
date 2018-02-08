module Engine.Physics.Fixtures exposing (..)

import Engine.Physics.Vec as Vec exposing (..)

type Fixture = 
    AABB Vec2 Vec2
    | Circle Vec2 Float

checkAABBwithAABB : (Vec2, Vec2) -> (Vec2, Vec2) -> Bool
checkAABBwithAABB (mina, maxa) (minb, maxb) =
    if (maxa.x < minb.x || mina.x > maxb.x) then
        False
    else if (maxa.y < minb.y || mina.y > maxb.y) then
        False
    else
        True

checkCirclewithCircleUnoptimized: (Vec2, Float) -> (Vec2, Float) -> Bool
checkCirclewithCircleUnoptimized (positiona, radiusa) (positionb, radiusb) =
    let
        r = radiusa + radiusb
        dist = distance positiona positionb
    in
        r < dist

checkCirclewithCircleOptimized: (Vec2, Float) -> (Vec2, Float) -> Bool
checkCirclewithCircleOptimized (positiona, radiusa) (positionb, radiusb) =
    let
        r = (radiusa + radiusb) ^ 2
        dist = distance2 positiona positionb
    in
        r < dist

setPosition: Fixture -> Vec2 -> Fixture
setPosition f a = 
    case f of
        AABB mina maxa ->
            let
                xExtent = (maxa.x - mina.x) / 2
                yExtent = (maxa.y - mina.y) / 2
                offset = { x = xExtent, y = yExtent }
            in
                AABB (Vec.sub a offset) (Vec.add a offset)
        Circle pos rad ->
            Circle a rad

getPosition: Fixture -> Vec2
getPosition f =
    case f of 
        AABB mina maxa ->
            Vec.midpoint maxa mina
        Circle pos _ ->
            pos