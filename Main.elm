module Main exposing (..)

import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes as HtmlAttr
import Html.Events
import Dict exposing (Dict)
import Svg exposing (Svg, svg)
import Svg.Attributes as Attr
import Svg.Events
import Svg.Lazy exposing (lazy)
import Keyboard
import Char
import Json.Decode as Decode exposing (Decoder)

import OpenSolid.Svg as Svg
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.Vector2d as Vector2d
import OpenSolid.LineSegment2d as LineSegment2d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Polygon2d as Polygon2d
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.Frame2d as Frame2d
-- import Transformer2D exposing (Transformation)
import Color.Convert exposing (..)
import Graph as Graph exposing (Graph)
import IntDict
import Draggable
import Draggable.Events as DragEvents
import Keyboard.Extra as KeyEx
import List.Extra as ListEx
import Maybe.Extra as MaybeEx

import Graph.Extra as GraphEx
import OpenSolid.Vector2d.Extra as Vector2dEx
import OpenSolid.Extra exposing (..)
import Types exposing (..)
import View


init : Model
init =
    { rootId = 0
    , graph =
        Graph.fromNodesAndEdges
            [ Graph.Node 0
                { color = Color.rgb 100 0 200
                , opacity = 0.5
                , shape = Triangle
                , controlLocation = Point2d (300,0)
                }
            , Graph.Node 1
                { color = Color.rgb 0 40 60
                , opacity = 0.25
                , shape = Square
                , controlLocation = Point2d (120, 120)
                }
            , Graph.Node 2
                { color = Color.rgb 200 100 0
                , opacity = 0.5
                , shape = Circle
                , controlLocation = Point2d (210, 240)
                }
            ]
            [ Graph.Edge 0 1
                { translation = Vector2d ( 0, 0 )
                , scale = 0.5
                , rotation = 0
                }
            , Graph.Edge 1 0
                { translation = Vector2d ( 1, 0 )
                , scale = 1 / (sqrt 2)
                , rotation = 135
                }
            , Graph.Edge 1 2
                { translation = Vector2d ( -0.25, 0.25 )
                , scale = 0.25
                , rotation = -135
                }
            , Graph.Edge 2 0
                { translation = Vector2d ( -0.25, -0.25 )
                , scale = 0.25
                , rotation = 90
                }
            ]
    , zoomScale = 1
    , panOffset = Vector2d ( View.stageSize / 2, View.stageSize / 2 )
    , drag = Draggable.init
    , dragAction = Nothing
    , hoverItem = Nothing
    , selectedItem = Nothing
    }


-- UPDATE


dragConfig : Draggable.Config DragAction Msg
dragConfig =
    Draggable.customConfig
        [ DragEvents.onDragBy (Vector2d >> OnDragBy)
        , DragEvents.onDragStart StartDragging
        ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ZoomIn ->
            { model | zoomScale = model.zoomScale * 1.05 } ! []

        ZoomOut ->
            { model | zoomScale = model.zoomScale / 1.05 } ! []

        MouseHover graphItem ->
            { model | hoverItem = Just graphItem } ! []

        MouseLeave ->
            { model | hoverItem = Nothing } ! []

        Select graphItem ->
            { model | selectedItem = Just graphItem } ! []

        Deselect ->
            { model | selectedItem = Nothing } ! []

        StartDragging dragAction ->
            let
                newModel =
                    case dragAction of
                        EdgeChangeEndNode edge _ ->
                            let
                                insertTemps =
                                    Graph.insert (newNodeContext model)
                                        >> GraphEx.insertEdge edge
                            in
                                { model | graph = insertTemps model.graph }

                        _ ->
                            model
            in
                { newModel | dragAction = Just dragAction } ! []

        StopDragging ->
            let updatedModel =
                case ( model.dragAction, model.hoverItem ) of
                    ( Just (EdgeChangeEndNode edge endPoint)
                    , Just (Node endNodeID)
                    ) ->
                        let
                            updatedEdge =
                                { edge | to = endNodeID }

                            replaceOldEdge =
                                ListEx.replaceIf (GraphEx.edgeEquals edge) updatedEdge
                        in
                            { model
                                | graph = GraphEx.updateEdges replaceOldEdge model.graph
                                , selectedItem = Just (Edge updatedEdge.from updatedEdge.to)
                            }

                    _ ->
                        model
            in
                { updatedModel | dragAction = Nothing }
                    |> cleanupTempNodesAndEdges
                    |> flip (,) Cmd.none

        OnDragBy vec ->
            case model.dragAction of
                Just (MoveNodeControl node) ->
                    moveNodeControl vec node model ! []

                Just (EdgeChangeEndNode edge endPoint) ->
                    let
                        newEndPoint =
                            endPoint |> Point2d.translateBy vec

                        dragAction =
                            EdgeChangeEndNode edge newEndPoint
                    in
                        { model | dragAction = Just dragAction } ! []

                Just Pan ->
                    { model | panOffset = Vector2d.sum vec model.panOffset } ! []

                _ ->
                    model ! []

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model

        Delete ->
            case model.selectedItem of
                Just (Node nodeId) ->
                    { model | graph = Graph.remove nodeId model.graph } ! []

                _ ->
                    update NoOp model

        ChangeColor node colorStr ->
            let
                stringToColor =
                    Result.withDefault Color.black << hexToColor

                updateLabel ({label} as n) =
                    { n | label = { label | color = (stringToColor colorStr) } }
            in
                { model
                    | graph =
                        GraphEx.updateNode node.id updateLabel model.graph
                } ! []

        ChangeOpacity node opacity ->
            let
                updateLabel ({label} as n) =
                    { n | label = { label | opacity = opacity } }
            in
                { model
                    | graph =
                        GraphEx.updateNode node.id updateLabel model.graph
                } ! []

        ChangeShape node shapeStr ->
            let
                updateLabel ({label} as n) =
                    { n | label = { label | shape = shape } }

                shape =
                    case shapeStr of
                        "Circle" -> Circle
                        "Triangle" -> Triangle
                        _ -> Square
            in
                { model
                    | graph =
                        GraphEx.updateNode node.id updateLabel model.graph
                } ! []

        ChangeScale edge newScale ->
            let
                edgeUpdater ( {label} as e ) =
                    { e | label = { label | scale = newScale } }

                graphUpdater =
                    GraphEx.updateEdge edge.from edge.to edgeUpdater
            in
                { model | graph = graphUpdater model.graph } ! []

        ChangeRotation edge newRotation ->
            let
                edgeUpdater ( {label} as e ) =
                    { e | label = { label | rotation = newRotation } }

                graphUpdater =
                    GraphEx.updateEdge edge.from edge.to edgeUpdater
            in
                { model | graph = graphUpdater model.graph } ! []

        TranslationX edge newX ->
            let
                edgeUpdater ( {label} as e ) =
                    { e | label =
                        { label | translation = (Vector2dEx.setX newX) label.translation }
                    }

                graphUpdater =
                    GraphEx.updateEdge edge.from edge.to edgeUpdater
            in
                { model | graph = graphUpdater model.graph } ! []


        TranslationY edge newY ->
            -- TODO same as TranslationX so refactor
            let
                edgeUpdater ( {label} as e ) =
                    { e | label =
                        { label | translation = (Vector2dEx.setY newY) label.translation }
                    }

                graphUpdater =
                    GraphEx.updateEdge edge.from edge.to edgeUpdater
            in
                { model | graph = graphUpdater model.graph } ! []


        _ ->
            model ! []


newNode model =
    Graph.Node (nextId model.graph) (Element Color.white 0.5 Circle Point2d.origin)

newNodeContext model =
    Graph.NodeContext (newNode model) IntDict.empty IntDict.empty

nextId =
    Graph.nodeIdRange >> Maybe.map Tuple.second >> Maybe.map ((+) 1) >> Maybe.withDefault 0


cleanupTempNodesAndEdges : Model -> Model
cleanupTempNodesAndEdges model =
    let
        removeHangingEdges =
            GraphEx.updateEdges (List.filter (.to >> (/=) -1))

        removeNeighborlessNodes =
            GraphEx.updateNodes
                (List.filter (\n -> GraphEx.neighborCount model.graph n > 0))

        cleanupGraph =
            removeNeighborlessNodes >> removeHangingEdges
    in
        { model | graph = cleanupGraph model.graph }


moveNodeControl : Vector2d -> Node -> Model -> Model
moveNodeControl vec node model =
    let
        updateElement element =
            { element
                | controlLocation =
                    Point2d.translateBy vec element.controlLocation
            }

        updater node =
            { node | label = updateElement node.label }
    in
        { model | graph = GraphEx.updateNode node.id updater model.graph }


-- SUB

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.presses
            (\code ->
                case KeyEx.fromCode code |> Debug.log "key code" of
                    KeyEx.CharZ -> Delete
                    _ -> NoOp
            )
        , Draggable.subscriptions DragMsg model.drag
        ]



-- MAIN


main =
    Html.program
        { init = init ! []
        , update = update
        , subscriptions = subscriptions
        , view = View.root
        }
