module Types exposing (..)

import Graph
import OpenSolid.Geometry.Types exposing (..)
import Color exposing (Color)
import Draggable
import Monocle.Lens as Lens exposing (Lens)


-- LENS

nodeLensElement : Lens Node Element
nodeLensElement =
    Lens .label (\l n -> { n | label = l })


elementLensColor : Lens Element Color
elementLensColor =
    Lens .color (\c e -> { e | color = c })


elementLensOpacity : Lens Element Float
elementLensOpacity =
    Lens .opacity (\o e -> { e | opacity = o })


elementLensShape : Lens Element Shape
elementLensShape =
    Lens .shape (\s e -> { e | shape = s })


nodeLensColor : Lens Node Color
nodeLensColor =
    Lens.compose nodeLensElement elementLensColor


nodeLensOpacity : Lens Node Float
nodeLensOpacity =
    Lens.compose nodeLensElement elementLensOpacity


nodeLensShape : Lens Node Shape
nodeLensShape =
    Lens.compose nodeLensElement elementLensShape


edgeLensTransformation : Lens Edge Transformation
edgeLensTransformation =
    Lens .label (\t e -> { e | label = t })


transformationLensScale : Lens Transformation Float
transformationLensScale =
    Lens .scale (\s t -> { t | scale = s })


transformationLensRotation : Lens Transformation Float
transformationLensRotation =
    Lens .rotation (\r t -> { t | rotation = r })


transformationLensTranslation =
    Lens .translation (\tl tf -> { tf | translation = tl })


edgeLensScale : Lens Edge Float
edgeLensScale =
    Lens.compose edgeLensTransformation transformationLensScale


edgeLensRotation : Lens Edge Float
edgeLensRotation =
    Lens.compose edgeLensTransformation transformationLensRotation


edgeLensTranslation =
    Lens.compose edgeLensTransformation transformationLensTranslation


modelLensGraph : Lens Model Graph
modelLensGraph =
    Lens .graph (\g m -> { m | graph = g })


-- ALIASES

type alias Graph =
    Graph.Graph Element Transformation


type alias Node =
    Graph.Node Element


type alias Edge =
    Graph.Edge Transformation


type alias Id =
    Graph.NodeId


-- GRAPH LABELS

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


-- UI STATE

type DragAction
    = MoveNodeControl Node
    | EdgeChangeEndNode Edge Point2d
    | Pan


type Selectable
    = Node Id
    | Edge Id Id


-- MODEL

type alias Model =
    { graph : Graph
    , rootId : Id
    , zoomScale : Float
    , panOffset : Vector2d
    , drag : Draggable.State DragAction
    , dragAction : Maybe DragAction
    , hoverItem : Maybe Selectable
    , selectedItem : Maybe Selectable
    }


-- MSG

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


-- UTILS

shapeFromString : String -> Result String Shape
shapeFromString s =
    case s of
        "Circle" -> Ok Circle
        "Triangle" -> Ok Triangle
        "Square" -> Ok Square
        _ -> Err ("'" ++ s ++ "' is not a valid Shape type")
