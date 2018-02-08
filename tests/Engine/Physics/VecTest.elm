module Engine.Physics.VecTest exposing (..)

import Test exposing (..)
import Expect exposing (..)

import Util
import Engine.Physics.Vec as Vec

addTest : Test
addTest =
    describe "Vec.add Vec.sub"
        [
            test "actually adds" <|
                \_ ->
                    let 
                        a = { x = 5.5, y = 5.5}
                        b = { x = 4.5, y = 4.5}
                    in
                        Expect.equal (Vec.add a b) {x = 10, y = 10}
            , test "actually subtracts" <|
                \_ ->
                    let
                        a = Vec.vec2 5.5 4.5
                        b = Vec.vec2 10.5 9.5
                    in
                        Expect.equal (Vec.sub b a) (Vec.vec2 5 5)
        ]

distanceTest : Test
distanceTest = 
    describe "Vec.distance2 Vec.distance Vec.length2 Vec.length"
        [
            test "distance and length return the same result" <|
                \_ ->
                    let
                        dist = Vec.distance {x = 5.5, y = 5.5} { x = 0, y = 0 }
                        len = Vec.length {x = 5.5, y = 5.5}
                    in
                        Expect.equal dist len
            , test "distance2 and length2 return the same result" <|
                \_ ->
                    let
                        dist = Vec.distance2 {x = 5.5, y = 5.5} { x = 0, y = 0 }
                        len = Vec.length2 {x = 5.5, y = 5.5}
                    in
                        Expect.equal dist len
            , test "distance and distance2 are close" <|
                \_ ->
                    let
                        dist = Vec.distance {x = 5.5, y = 5.5} { x = 0, y = 0 }
                        dist2 = Vec.distance2 {x = 5.5, y = 5.5} { x = 0, y = 0 }
                    in
                        Expect.within (Absolute Util.fpT) (dist ^ 2) dist2
            , test "length and length2 are close" <|
                \_ ->
                    let
                        len = Vec.length {x = 5.5, y = 5.5}
                        len2 = Vec.length2 {x = 5.5, y = 5.5}
                    in
                        Expect.within (Absolute Util.fpT) (len ^ 2) len2
        ]

midpointTest : Test
midpointTest =
    describe "Vec.midpoint"
        [
            test "gets midpoint" <|
                \_ -> 
                    let
                        a = { x = 11.5, y = 10.5 }
                        b = { x = 1.5, y = 0.5 }
                    in
                        Expect.equal (Vec.midpoint a b) { x = 6.5, y = 5.5 }
        ]