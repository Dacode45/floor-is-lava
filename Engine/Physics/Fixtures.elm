module Engine.Physics.Fixtures exposing (..)

import Engine.Physics.Vec as Vec exposing (..)

type Fixture = 
    AABB Vec2 Vec2
    | Circle Vec2 Float

circle: Vec2 -> Float -> Fixture
circle pos r =
    Circle pos r

aabb: Vec2 -> Vec2 -> Fixture
aabb a b =
    AABB a b

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

getVolume: Fixture -> Float
getVolume f =
    case f of
        AABB mina maxa ->
            let
                (w, h) = Vec.widthHeight mina maxa
            in
                w * h
        Circle _ r -> pi * (r ^ 2)

getMass: Fixture -> Float -> Float
getMass f d =
    d * (getVolume f)

getInvMass: Fixture -> Float -> Float
getInvMass f d =
    1 / (getInvMass f d)
