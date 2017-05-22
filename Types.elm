module Types exposing (..)

import Graph exposing (Graph)
import OpenSolid.Geometry.Types exposing (..)
import Color exposing (Color)
import Draggable


type alias Node =
    Graph.Node Element


type alias Edge =
    Graph.Edge Transformation


type alias Id =
    Graph.NodeId


type DragAction
    = MoveNodeControl Node
    | EdgeChangeEndNode Edge Point2d
    | Pan


type Selectable
    = Node Id
    | Edge Id Id


type alias Element =
    { color : Color
    , opacity : Float
    , shape : Shape
    , controlLocation : Point2d
    }


type Shape
    = Circle
    | Square
    | Triangle


type alias Transformation =
    { translation : Vector2d
    , scale : Float
    , rotation : Float
    }


type alias Model =
    { graph : Graph Element Transformation
    , rootId : Id
    , zoomScale : Float
    , panOffset : Vector2d
    , drag : Draggable.State DragAction
    , dragAction : Maybe DragAction
    , hoverItem : Maybe Selectable
    , selectedItem : Maybe Selectable
    }


type Msg
    -- STAGE VIEW
    = ZoomIn
    | ZoomOut
    -- GRAPH VIEW
    | MouseHover Selectable
    | MouseLeave
    | Select Selectable
    | Deselect
    -- DETAIL
    | Delete
    | ChangeColor Node String
    | ChangeOpacity Node Float
    | ChangeShape Node String
    | ChangeScale Edge Float
    | ChangeRotation Edge Float
    | TranslationX Edge Float
    | TranslationY Edge Float
    -- DRAG
    | StartDragging DragAction
    | StopDragging
    | OnDragBy Vector2d
    | DragMsg (Draggable.Msg DragAction)
    | NoOp
