module OpenSolid.Vector2d.Extra exposing (..)

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Vector2d as Vector2d



updateComponents : ((Float, Float) -> (Float, Float)) -> Vector2d -> Vector2d
updateComponents updater =
    Vector2d.components >> updater >> Vector2d


setX : Float -> Vector2d -> Vector2d
setX newX =
    updateComponents (Tuple.mapFirst (\_ -> newX))


setY : Float -> Vector2d -> Vector2d
setY newY =
    updateComponents (Tuple.mapSecond (\_ -> newY))
