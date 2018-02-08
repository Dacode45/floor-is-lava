module Engine.Entity exposing (..)

import Dict
import Engine.Component

type alias Entity componentSet = 
    {
        components: Dict.Dict String (Component componentSet)
    }