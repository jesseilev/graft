module OpenSolid.Extra exposing (..)


import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.Vector2d as Vector2d
import OpenSolid.LineSegment2d as LineSegment2d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Polygon2d as Polygon2d
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.Frame2d as Frame2d


boundingBoxOrOrigin =
    Polygon2d.boundingBox
        >> Maybe.withDefault (BoundingBox2d.singleton Point2d.origin)


pointFromBoundingBox getX getY bb =
    Point2d (getX bb, getY bb)


centroid =
    boundingBoxOrOrigin >> BoundingBox2d.centroid


pointFromPolygon getBoundingBoxX getBoundingBoxY =
    boundingBoxOrOrigin >> (pointFromBoundingBox getBoundingBoxX getBoundingBoxY)

topLeft =
    pointFromPolygon BoundingBox2d.minX BoundingBox2d.minY

topRight =
    pointFromPolygon BoundingBox2d.maxX BoundingBox2d.minY

bottomLeft =
    pointFromPolygon BoundingBox2d.minX BoundingBox2d.maxY

bottomRight =
    pointFromPolygon BoundingBox2d.maxX BoundingBox2d.maxY

edgeTop poly =
    curry LineSegment2d (topLeft poly) (topRight poly)

edgeLeft poly =
    curry LineSegment2d (topLeft poly) (bottomLeft poly)

edgeRight poly =
    curry LineSegment2d (topRight poly) (bottomRight poly)

edgeBottom poly =
    curry LineSegment2d (bottomLeft poly) (bottomRight poly)

midLeft =
    LineSegment2d.midpoint << edgeLeft

midRight =
    LineSegment2d.midpoint << edgeRight

midTop =
    LineSegment2d.midpoint << edgeTop

midBottom =
    LineSegment2d.midpoint << edgeBottom


rectangle2d x y w h =
    Polygon2d
        [ Point2d ( x, y )
        , Point2d ( x + w, y )
        , Point2d ( x + w, y + h )
        , Point2d ( x, y + h )
        ]

unitCircle =
    Circle2d
        { centerPoint = Point2d (0, 0)
        , radius = 1
        }


unitSquare =
    rectangle2d -1 -1 2 2


unitTriangle =
    Triangle2d ( Point2d ( -1, -1 ) , Point2d ( -1, 1 ) , Point2d ( 1, -1 ) )
