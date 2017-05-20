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

import OpenSolid.Svg as Svg
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.Vector2d as Vector2d
import OpenSolid.LineSegment2d as LineSegment2d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Polygon2d as Polygon2d
import OpenSolid.BoundingBox2d as BoundingBox2d
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


type DragAction
    = MoveNodeControl (Graph.Node Element)
    -- | NewEdge (Graph.Node Element) Point2d
    | EdgeChangeEndNode (Graph.Edge Transformation) Point2d
    -- | EdgeChangeStartpoint Graph.NodeId Graph.NodeId
    -- | MoveElement Graph.NodeId
    -- | Pan


-- type Id
--     = ElementSet Graph.NodeId
--     | Element Graph.NodeId (List Graph.NodeId)
--     | NodeControl Graph.NodeId
--     | EdgeControl Graph.NodeId Graph.NodeId


type Selectable
    = StageNode (Graph.Node Element)
    | Node Graph.NodeId
    | Edge Graph.NodeId Graph.NodeId
    | Arrowhead (Graph.Edge Transformation)
    | ArrowTail (Graph.Edge Transformation)
    | Incoming (Graph.Node Element)
    | Outgoing (Graph.Node Element)


type alias Model =
    { graph : Graph Element Transformation
    , rootId : Graph.NodeId
    , zoomScale : Float
    , drag : Draggable.State DragAction
    , dragAction : Maybe DragAction
    , hoverItem : Maybe Selectable
    , selectedItem : Maybe Selectable
    }


type alias Element =
    { color : Color
    , opacity : Float
    , controlLocation : Point2d
    }


type alias Transformation =
    { translation : (Float, Float)
    , scale : Float
    , rotation : Float
    }


init : Model
init =
    { rootId = 0
    , graph =
        Graph.fromNodesAndEdges
            [ Graph.Node 0
                { color = Color.rgb 100 0 200
                , opacity = 0.5
                , controlLocation = Point2d (300,0)
                }
            , Graph.Node 1
                { color = Color.rgb 0 40 60
                , opacity = 0.25
                , controlLocation = Point2d (120, 120)
                }
            , Graph.Node 2
                { color = Color.rgb 200 100 0
                , opacity = 0.75
                , controlLocation = Point2d (210, 240)
                }
            ]
            [ Graph.Edge 0 1
                { translation = ( 0, 0.5 )
                , scale = 0.5
                , rotation = 0
                }
            , Graph.Edge 1 0
                { translation = ( 1, -0.5 )
                , scale = 0.75
                , rotation = 120
                }
            , Graph.Edge 1 2
                { translation = ( 0, -0.75 )
                , scale = 0.5
                , rotation = 0
                }
            , Graph.Edge 2 0
                { translation = ( 0, -0.5 )
                , scale = 0.5
                , rotation = 0
                }
            ]
    , zoomScale = 1
    , drag = Draggable.init
    , dragAction = Nothing
    , hoverItem = Nothing
    , selectedItem = Nothing
    }


type Msg
    = ZoomIn
    | ZoomOut
    | MouseHover Selectable
    | MouseLeave
    | Select Selectable
    | Deselect
    | DragMsg (Draggable.Msg DragAction)
    | OnDragBy Vector2d
    | StartDragging DragAction
    | StopDragging
    | Delete
    | ChangeColor (Graph.Node Element) String
    | ChangeOpacity (Graph.Node Element) Float
    | ChangeScale (Graph.Edge Transformation) Float
    | ChangeRotation (Graph.Edge Transformation) Float
    | TranslationX (Graph.Edge Transformation) Float
    | TranslationY (Graph.Edge Transformation) Float
    | NoOp


dragConfig : Draggable.Config DragAction Msg
dragConfig =
    Draggable.customConfig
        [ DragEvents.onDragBy (Vector2d >> OnDragBy)
        , DragEvents.onDragStart StartDragging
        ]


dragItem dragAction =
    case dragAction of
        MoveNodeControl node ->
            Node node.id

        EdgeChangeEndNode edge _ ->
            Edge edge.from edge.to


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
                { newModel
                    | dragAction = Just dragAction
                    -- , selectedItem = Just (dragItem dragAction)
                } ! []

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
                translationUpdater =
                    Tuple.mapFirst (\_ -> newX)

                edgeUpdater ( {label} as e ) =
                    { e | label =
                        { label | translation = translationUpdater label.translation }
                    }

                graphUpdater =
                    GraphEx.updateEdge edge.from edge.to edgeUpdater
            in
                { model | graph = graphUpdater model.graph } ! []


        TranslationY edge newY ->
            -- TODO same as TranslationX so refactor
            let
                translationUpdater =
                    Tuple.mapSecond (\_ -> newY)

                edgeUpdater ( {label} as e ) =
                    { e | label =
                        { label | translation = translationUpdater label.translation }
                    }

                graphUpdater =
                    GraphEx.updateEdge edge.from edge.to edgeUpdater
            in
                { model | graph = graphUpdater model.graph } ! []


        _ ->
            model ! []


transformationEmpty =
    { translation = (0,0)
    , scale = 0.5
    , rotation = 0
    }

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


moveNodeControl : Vector2d -> Graph.Node Element -> Model -> Model
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


-- getGraphItem : DragAction -> Maybe Graph.NodeId
-- getGraphItem dragAction =
--     case dragAction of
--         MoveNodeControl id ->
--             Just id
--
--         NewEdge startNode _ ->
--             Just (NodeItem startNode)
--
--         _ ->
--             Nothing


-- SUB


subscriptions model =
    Sub.batch
        [ Keyboard.presses
            (\code ->
                case KeyEx.fromCode code |> Debug.log "key code" of
                    KeyEx.Equals -> ZoomIn
                    KeyEx.Minus -> ZoomOut
                    KeyEx.CharZ -> Delete
                    _ -> NoOp
            )
        , Draggable.subscriptions DragMsg model.drag
        ]


-- VIEW


view : Model -> Html Msg
view model =
    Html.div
        [ HtmlAttr.style containerStyle ]
        [ lazy viewStage model
        , viewControls model
        ]


viewControls : Model -> Html Msg
viewControls model =
    Html.section
        [ HtmlAttr.style
            [ "order" => "1"
            , "display" => "grid"
            , "grid-template-rows" => "70% 30%"
            , "grid-template-columns" => "100%"
            ]
        ]
        [ viewDraggableControls model
        -- , Html.hr [ HtmlAttr.style [ "border-top" => "1px solid #ccc" ] ] []
        , viewDetailControls model
        ]


viewDetailControls : Model -> Html Msg
viewDetailControls model =
    Html.section
        [ HtmlAttr.style
            [ "background" => "#ddd"
            , "padding" => "20px"
            ]
        ]
        [ case model.selectedItem of
            Just (Node nodeId) ->
                acceptMaybe (Html.text "") (viewNodeDetail model)
                    <| GraphEx.getNode nodeId model.graph


            Just (Edge from to) ->
                (viewEdgeDetail model |> acceptMaybe (Html.text ""))
                    <| GraphEx.getEdge from to model.graph

            _ ->
                Html.text ""
        ]


acceptMaybe : b -> (a -> b) -> Maybe a -> b
acceptMaybe default func =
    Maybe.map func >> Maybe.withDefault default


viewEdgeDetail model edge =
    Html.div []
        [ Html.label []
            [ Html.text "X: "
            , Html.input
                [ HtmlAttr.type_ "range"
                , HtmlAttr.value (.translation edge.label |> Tuple.first |> toString)
                , HtmlAttr.step "0.05"
                , HtmlAttr.min "-2"
                , HtmlAttr.max "2"
                , Html.Events.onInput (TranslationX edge |> msgFromString)
                ]
                []
            ]
        , Html.br [] []
        , Html.label []
            [ Html.text "Y: "
            , Html.input
                [ HtmlAttr.type_ "range"
                , HtmlAttr.value (.translation edge.label |> Tuple.second |> toString)
                , HtmlAttr.step "0.05"
                , HtmlAttr.min "-2"
                , HtmlAttr.max "2"
                , Html.Events.onInput (TranslationY edge |> msgFromString)
                ]
                []
            ]
        , Html.br [] []
        , Html.label []
            [ Html.text "Scale: "
            , Html.input
                [ HtmlAttr.type_ "range"
                , HtmlAttr.value (.scale edge.label |> toString)
                , HtmlAttr.step "0.01"
                , HtmlAttr.min "0"
                , HtmlAttr.max "0.9"
                , Html.Events.onInput (ChangeScale edge |> msgFromString)
                ]
                []
            ]
        , Html.br [] []
        , Html.label []
            [ Html.text "Rotation: "
            , Html.input
                [ HtmlAttr.type_ "range"
                , HtmlAttr.value (.rotation edge.label |> toString)
                , HtmlAttr.step "5"
                , HtmlAttr.min "0"
                , HtmlAttr.max "360"
                , Html.Events.onInput (ChangeRotation edge |> msgFromString)
                ]
                []
            ]
        ]


msgFromString : (Float -> Msg) -> String -> Msg
msgFromString msgConstructor =
    String.toFloat
        >> Result.map msgConstructor
        >> Result.withDefault NoOp


viewNodeDetail model node =
    Html.div []
        [ Html.fieldset []
            [ Html.label []
                [ Html.text "Color: "
                , Html.input
                    [ HtmlAttr.type_ "color"
                    , HtmlAttr.value (colorToHex node.label.color)
                    , Html.Events.onInput (ChangeColor node)
                    ]
                    []
                ]
            ]
        , Html.fieldset []
            [ Html.label []
                [ Html.text "Opacity: "
                , Html.input
                    [ HtmlAttr.type_ "range"
                    , HtmlAttr.min "0"
                    , HtmlAttr.max "1"
                    , HtmlAttr.step "0.05"
                    , HtmlAttr.value (toString node.label.opacity)
                    , Html.Events.onInput (msgFromString (ChangeOpacity node))
                    ]
                    []
                ]
            ]
        , Html.fieldset []
            [ Html.button
                [ Html.Events.onClick Delete
                ]
                [ Html.text "Delete" ]
            ]
        ]


viewDraggableControls : Model -> Html Msg
viewDraggableControls model =
    let
        nodeViews =
            List.map (viewNodeControl model) (Graph.nodes model.graph)

        edgeViews =
            List.map (viewEdgeControl model) (Graph.edges model.graph)
    in
        svg
            [ Attr.viewBox <| svgViewBoxString rootSize rootSize
            , Svg.Events.onMouseUp StopDragging
            , HtmlAttr.style
                [ "background" => "#eee"
                ]
            -- , Svg.Events.onClick Deselect
            ]
            (nodeViews ++ edgeViews)


newNode model =
    Graph.Node (nextId model.graph) (Element Color.grey 0.5 Point2d.origin)

newNodeContext model =
    Graph.NodeContext (newNode model) IntDict.empty IntDict.empty

nextId =
    Graph.nodeIdRange >> Maybe.map Tuple.second >> Maybe.map ((+) 1) >> Maybe.withDefault 0


incomingPortLocation =
    nodeControlRect >> edgeLeft >> flip LineSegment2d.interpolate 0.2


outgoingPortLocation =
    nodeControlRect >> edgeRight >> flip LineSegment2d.interpolate 0.8


viewEdgeControl : Model -> Graph.Edge Transformation -> Svg Msg
viewEdgeControl model edge =
    let
        isSelected =
            model.selectedItem == Just (Edge edge.from edge.to)

        lineView =
            Svg.lineSegment2d
                [ Attr.stroke "grey"
                , Attr.strokeWidth "1px"
                , Attr.strokeDasharray "5,5"
                ]

        arrowLocation =
            flip LineSegment2d.interpolate 0.75

        arrowView lineSeg =
            LineSegment2d.direction lineSeg
                |> Maybe.map
                    (arrowTriangle <| arrowLocation lineSeg)
                |> Maybe.map
                    ( Svg.triangle2d
                        [ Attr.fill "grey", Attr.stroke "grey"
                        , Attr.cursor "alias"
                        , Draggable.mouseTrigger
                            (EdgeChangeEndNode edge (arrowLocation lineSeg))
                            DragMsg
                        ]
                    )
                |> Maybe.withDefault
                    (Svg.g [] [])

        edgeView lineSeg =
            Svg.g
                [ Svg.Events.onClick <| Select (Edge edge.from edge.to) ]
                [ Svg.lineSegment2d
                    [ Attr.opacity <| if isSelected then "1" else "0"
                    , Attr.strokeWidth "6px"
                    , Attr.stroke "yellow"
                    , Attr.cursor <| if isSelected then "default" else "pointer"
                    ]
                    lineSeg
                , lineView lineSeg
                , if isSelected then arrowView lineSeg else Svg.g [] []
                ]

        getNode =
            flip Graph.get model.graph

        specialDragEndpoint =
            case model.dragAction of
                Just (EdgeChangeEndNode e endpoint) ->
                    if GraphEx.edgeEquals e edge then Just endpoint else Nothing

                _ ->
                    Nothing

        edgeCount =
            Graph.edges model.graph |> List.length --|> Debug.log "edge count"
    in
        case ( getNode edge.from, getNode edge.to, specialDragEndpoint ) of
            ( Just fromCtx, Just toCtx, Nothing ) ->
                edgeView
                    ( LineSegment2d
                        ( outgoingPortLocation fromCtx.node
                        , incomingPortLocation toCtx.node
                        )
                    )

            ( Just fromCtx, _, Just endpoint ) ->
                edgeView
                    ( LineSegment2d
                        ( outgoingPortLocation fromCtx.node
                        , endpoint
                        )
                    )

            _ ->
                Svg.g [] []


arrowTriangle : Point2d -> Direction2d -> Triangle2d
arrowTriangle tip direction =
    let
        back =
            Direction2d.scaleBy (controlSize / 6) (Direction2d.flip direction)

        left =
            Vector2d.perpendicularTo back
                |> Vector2d.scaleBy 0.5

        right =
            left |> Vector2d.rotateBy (degrees 180)
    in
        Triangle2d
            ( tip
            , tip |> Point2d.translateBy (Vector2d.sum back left)
            , tip |> Point2d.translateBy (Vector2d.sum back right)
            )


viewNodeControl : Model -> Graph.Node Element -> Svg Msg
viewNodeControl model node =
    let
        isSelected =
            model.selectedItem == Just (Node node.id)

        isHovering =
            model.hoverItem == Just (Node node.id) && not isSelected

        isBeingDraggedTo =
            case model.dragAction of
                Just (EdgeChangeEndNode _ _) -> isHovering
                _ -> False

        rect =
            nodeControlRect node

        inboundEdgePort =
            Circle2d { radius = controlSize / 10, centerPoint = incomingPortLocation node }

        outboundEdgePort =
            Circle2d { radius = controlSize / 10, centerPoint = outgoingPortLocation node }
    in
    Svg.g
        [ Attr.opacity <| if isSelected then "1" else "0.5"
        ]
        [ Svg.polygon2d
            [ Attr.fill "#ccc"
            , Attr.cursor <| if MaybeEx.isNothing model.dragAction then "move" else ""
            , Attr.stroke "grey"
            , Attr.strokeWidth <| if isHovering || isSelected then "2px" else "0"
            , Draggable.mouseTrigger (MoveNodeControl node) DragMsg
            ]
            rect
        , Svg.circle2d
            [ Attr.fill (colorToHex node.label.color)
            , Attr.opacity <| toString node.label.opacity
            , Attr.cursor <| if MaybeEx.isNothing model.dragAction then "pointer" else ""
            , Svg.Events.onClick (Select (Node node.id))
            , Svg.Events.onMouseOver (MouseHover (Node node.id))
            , Svg.Events.onMouseOut MouseLeave
            ]
            (Circle2d
                { radius = controlSize * 3 / 8, centerPoint = centroid rect }
            )
        , Svg.circle2d
            [ Attr.fill "grey"
            , Attr.stroke "grey"
            , Attr.strokeWidth <| if isBeingDraggedTo then "10px" else "0"
            ]
            inboundEdgePort
        , Svg.circle2d
            [ Attr.fill "grey"
            , Attr.cursor "alias"
            , Draggable.mouseTrigger
                ( EdgeChangeEndNode (Graph.Edge node.id -1 transformationEmpty)
                    (midRight rect)
                )
                DragMsg
            ]
            outboundEdgePort
        ]

controlSize = 80

nodeControlRect node =
    rectangle2d 0 0 controlSize controlSize
        |> Polygon2d.translateBy
            ( Vector2d
                ( Point2d.coordinates node.label.controlLocation)
            )


nodeCardStyle model node =
    let
        shadow =
            if model.hoverId == (Just node.id) then
                "#ccc 0 5px 40px 0"
            else
                "#ccc 0 5px 10px 0"
    in
        [ "height" => "50px"
        , "box-shadow" => shadow
        , "margin" => "50px"
        , "border-radius" => "2px"
        , "background" => "rgba(1 1 1 1)"
        -- , "width" => "25%"
        ]


containerStyle =
    [ "display" => "grid"
    , "grid-template-columns" => "60% 40%"
    , "grid-template-rows" => "100%"
    , "height" => "100%"
    ]


(=>) = (,)


viewStage : Model -> Html Msg
viewStage model =
    svg
        [ Attr.viewBox <| svgViewBoxString rootSize rootSize
        , HtmlAttr.style
            [ "background" => "grey"
            ]
        ]
        [ lazy viewRoot model ]


svgViewBoxString w h =
    "0 0 " ++ (toString w) ++ " " ++ (toString h)


rootSize =
    500


viewRoot model =
    let halfSize = rootSize / 2 in
    lazy (viewElement 1 model) model.rootId
        |> Svg.scaleAbout Point2d.origin (halfSize * 0.75 * model.zoomScale)
        |> Svg.translateBy (Vector2d (halfSize, halfSize))


viewElement : Float -> Model -> Graph.NodeId -> Svg Msg
viewElement cumulativeScale model id =
    case
        ( Graph.get id model.graph
        , cumulativeScale * model.zoomScale * rootSize >= 4
        -- , cumulativeScale * rootSize <= rootSize
        )
        of
        (Just nodeContext, True) ->
            let
                element =
                    nodeContext.node.label

                parent =
                    Svg.circle2d
                        [ Attr.fill (colorToHex element.color)
                        , Attr.opacity (toString element.opacity)
                        -- , Svg.Events.onMouseOver ( MouseHover (StageNode node) )
                        ]
                        unitCircle

                newScale transformation =
                    transformation.scale * cumulativeScale

                viewChild (childId, transformation) =
                    viewElement (newScale transformation) model childId
                        |> Svg.scaleAbout Point2d.origin
                            (clamp 0 1 transformation.scale)
                        |> Svg.rotateAround Point2d.origin
                            (degrees transformation.rotation)
                        |> Svg.translateBy
                            ( transformation.translation
                                |> Vector2d
                                -- |> vector2dFromPolar
                            )

                children =
                    nodeContext.outgoing
                        |> IntDict.toList
                        |> List.map viewChild
            in
                Svg.g [] (parent :: children)

        _ ->
            Svg.g [][]

-- VIEW HELPERS




-- GEOMETRY HELPERS


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


vector2dFromPolar (radius, angle) =
    Vector2d <| fromPolar (radius, degrees angle)





-- MAIN


main =
    Html.program
        { init = init ! []
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
