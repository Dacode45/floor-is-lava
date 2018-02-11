module Engine.Physics.Vec exposing (..)

type alias Vec2 =
    { x: Float, y: Float }

vec2: Float -> Float -> Vec2
vec2 x y =
    { x = x, y = y }

add: Vec2 -> Vec2 -> Vec2
add a b =
    { x = a.x + b.x, y = a.y + b.y }

distance2 : Vec2 -> Vec2 -> Float
distance2 a b =
    (a.x - b.x) ^ 2 + (a.y - b.y) ^ 2

distance : Vec2 -> Vec2 -> Float
distance a b =
    sqrt ((a.x - b.x) ^ 2 + (a.y - b.y) ^ 2)

dot: Vec2 -> Vec2 -> Float
dot a b =
    (a.x * b.x) + (a.y * b.y)

length2: Vec2 -> Float
length2 a =
    (a.x * a.x) + (a.y * a.y)

length: Vec2 -> Float
length a =
    sqrt ((a.x * a.x) + (a.y * a.y))

midpoint: Vec2 -> Vec2 -> Vec2
midpoint a b =
    let
        offset = mult (sub b a) 0.5
    in
        {x = a.x + offset.y, y = a.y + offset.y}

mult: Vec2 -> Float -> Vec2
mult a b =
    { x = a.x * b, y = a.y * b}

normal: Vec2 -> Vec2
normal a = 
    vec2 -a.y a.x

normalize: Vec2 -> Vec2
normalize a =
    let
        norm = length2 a
    in
        if ( norm > 0) then
            let
                inv = 1.0 / sqrt( norm )
            in
                { x = a.x * inv, y = a.y * inv }
        else
            a
        

sub : Vec2 -> Vec2 -> Vec2
sub a b =
    { x = (a.x - b.x), y = (a.y - b.y) }

widthHeight: Vec2 -> Vec2 -> (Float, Float)
widthHeight mina maxa =
    let
        w = maxa.x - mina.x
        h = maxa.y - mina.y
    in
        (w, h)