module Engine.Util exposing (..)


fltMax = 999999999
fltMin = -999999999

maxOfField: (a -> comparable) -> List a -> Maybe a
maxOfField field =
    let f x acc =
        case acc of 
            Nothing -> Just x
            Just y -> if field x > field y then Just x else Just y
    in List.foldr f Nothing