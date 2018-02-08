module Engine.Physics.FixturesTest exposing (..)

import Test exposing (..)
import Expect exposing (..)

import Util
import Engine.Physics.Vec as Vec exposing (vec2)
import Engine.Physics.Fixtures as Fixtures

checkAABBwithAABBTest: Test
checkAABBwithAABBTest = 
    describe "Fixtures.checkAABBwithAABB" [
        test "fixture inside" <|
            \_ ->
                let
                   mina = vec2 -5 -5
                   maxa = vec2 5 5
                   minb = vec2 -10 -10
                   maxb = vec2 10 10  
                   collided = Fixtures.checkAABBwithAABB (mina, maxa) (minb, maxb)
                in
                    Expect.equal collided True
        ,test "top left" <|
            \_ ->
                let
                   mina = vec2 -5 -5
                   maxa = vec2 5 5
                   minb = vec2 -6 4
                   maxb = vec2 -4 6  
                   collided = Fixtures.checkAABBwithAABB (mina, maxa) (minb, maxb)
                in
                    Expect.equal collided True
        ,test "top right" <|
            \_ ->
                let
                   mina = vec2 -5 -5
                   maxa = vec2 5 5
                   minb = vec2 4 4
                   maxb = vec2 6 6  
                   collided = Fixtures.checkAABBwithAABB (mina, maxa) (minb, maxb)
                in
                    Expect.equal collided True
        ,test "bottom left" <|
            \_ ->
                let
                   mina = vec2 -5 -5
                   maxa = vec2 5 5
                   minb = vec2 -6 -6
                   maxb = vec2 -4 -4  
                   collided = Fixtures.checkAABBwithAABB (mina, maxa) (minb, maxb)
                in
                    Expect.equal collided True
        ,test "bottom right" <|
            \_ ->
                let
                   mina = vec2 -5 -5
                   maxa = vec2 5 5
                   minb = vec2 4 -6
                   maxb = vec2 6 -4  
                   collided = Fixtures.checkAABBwithAABB (mina, maxa) (minb, maxb)
                in
                    Expect.equal collided True
        ,test "side by side" <|
            \_ ->
                let
                   mina = vec2 -5 -5
                   maxa = vec2 5 5
                   minb = vec2 -5 5
                   maxb = vec2 5 10  
                   collided = Fixtures.checkAABBwithAABB (mina, maxa) (minb, maxb)
                in
                    Expect.equal collided True

        ,test "outside" <|
            \_ ->
                let
                   mina = vec2 -5 -5
                   maxa = vec2 5 5
                   minb = vec2 -5 5.0001
                   maxb = vec2 -5 10  
                   collided = Fixtures.checkAABBwithAABB (mina, maxa) (minb, maxb)
                in
                    Expect.equal collided False
    ]

checkCirclewithCircleTest: Test
checkCirclewithCircleTest = 
    describe "Fixtures.checkCirclewithCircle" [
        test "optimized works" <|
            \_ ->
                let
                    posa = vec2 0 0
                    rada = 1
                    posb = vec2 2 2
                    radb = 1 + Util.fpT
                    collision = Fixtures.checkCirclewithCircleOptimized (posa, rada) (posb, radb)
                in
                    Expect.equal collision True
        ,test "unoptimized works" <|
            \_ ->
                let
                    posa = vec2 0 0
                    rada = 1
                    posb = vec2 2 2
                    radb = 1 + Util.fpT
                    collision = Fixtures.checkCirclewithCircleUnoptimized (posa, rada) (posb, radb)
                in
                    Expect.equal collision True
        ]

positionTest: Test
positionTest = 
    describe "Fixtures.getPosition Fixtures.setPosition" [
        test "get Circle Poisiton" <|
            \_ -> 
                let
                    circle = Fixtures.Circle (vec2 5 5) 1
                in
                    Expect.equal (Fixtures.getPosition circle) (vec2 5 5)
        ,test "get AABB Positon" <|
            \_ -> 
                let
                    box = Fixtures.AABB (vec2 -5 -5) (vec2 5 5)
                in
                    Expect.equal (Fixtures.getPosition box) (vec2 0 0)
        ,test "set Circle Positon" <|
            \_ -> 
                let
                    circle = Fixtures.setPosition (Fixtures.Circle (vec2 5 5) 1) (vec2 0 0)
                in
                    Expect.equal (Fixtures.getPosition circle) (vec2 0 0)
        ,test "set AABB Poisiton" <|
            \_ -> 
                let
                    box = Fixtures.AABB (vec2 -5 -5) (vec2 5 5)                    
                    update = Fixtures.setPosition box (vec2 0 0)
                in
                    Expect.equal (Fixtures.getPosition update) (vec2 0 0)
        
        
                    
    ]