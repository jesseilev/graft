module Main exposing (..)

import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes as HtmlAttr
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


type DragAction
    = MoveNodeControl Graph.NodeId
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
    | Node (Graph.Node Element)
    | Edge (Graph.Edge Transformation)
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
                , controlLocation = Point2d (0,0)
                }
            , Graph.Node 1
                { color = Color.red
                , controlLocation = Point2d (120, 120)
                }
            , Graph.Node 2
                { color = Color.rgb 200 100 0
                , controlLocation = Point2d (210, 240)
                }
            ]
            [ Graph.Edge 0 1
                { translation = ( 0.5, 120 )
                , scale = 0.5
                , rotation = 0
                }
            , Graph.Edge 1 0
                { translation = ( 1, -90 )
                , scale = 0.75
                , rotation = 0
                }
            , Graph.Edge 1 2
                { translation = ( -1, -60 )
                , scale = 0.5
                , rotation = 0
                }
            , Graph.Edge 2 0
                { translation = ( 0, 0 )
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
    | NoOp


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
            let newModel = { model | dragAction = Just dragAction } in
            case dragAction of
                EdgeChangeEndNode edge _ ->
                    let
                        insertTemps =
                            Graph.insert (newNodeContext model)
                                >> insertEdge edge
                    in
                        { newModel | graph = insertTemps model.graph } ! []

                _ ->
                    newModel ! []

        StopDragging ->
            let updatedModel =
                case ( model.dragAction, model.hoverItem ) of
                    ( Just (EdgeChangeEndNode edge endPoint)
                    , Just (Node endNode)
                    ) ->
                        let
                            updatedEdge =
                                { edge | to = endNode.id }

                            replaceOldEdge =
                                ListEx.replaceIf (edgeEquals edge) updatedEdge
                        in
                            { model | graph = updateEdges replaceOldEdge model.graph }

                    _ ->
                        model
            in
                cleanupTempNodesAndEdges updatedModel ! []

        OnDragBy vec ->
            case model.dragAction of
                Just (MoveNodeControl nodeId) ->
                    moveNodeControl vec nodeId model ! []

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
                Just (Node node) ->
                    { model | graph = Graph.remove node.id model.graph } ! []

                _ ->
                    update NoOp model

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
            updateEdges (List.filter (.to >> (/=) -1))

        removeNeighborlessNodes =
            updateNodes (List.filter (\n -> neighborCount model.graph n > 0))

        cleanupGraph =
            removeNeighborlessNodes >> removeHangingEdges
    in
        { model | graph = cleanupGraph model.graph }


neighborCount graph node =
    let adjCount = IntDict.keys >> List.length in
    Graph.get node.id graph
        |> Maybe.map
            (\{incoming, outgoing} -> adjCount incoming + adjCount outgoing)
        |> Maybe.withDefault 0
        |> Debug.log "neighborcount"


edgeEquals : Graph.Edge e -> Graph.Edge e -> Bool
edgeEquals e1 e2 =
    let
        stuff =
            (e1, e2, e1.from == e2.from && e1.to == e2.to)
    in
    e1.from == e2.from && e1.to == e2.to


updateNodes updater graph =
    Graph.fromNodesAndEdges (updater (Graph.nodes graph)) (Graph.edges graph)

updateEdges : (List (Graph.Edge e) -> List (Graph.Edge e)) -> Graph n e -> Graph n e
updateEdges updater graph =
    Graph.fromNodesAndEdges (Graph.nodes graph) (updater (Graph.edges graph))


insertEdge : Graph.Edge e -> Graph n e -> Graph n e
insertEdge newEdge graph =
    let
        alreadyExists =
            ListEx.find (edgeEquals newEdge) (Graph.edges graph)
                |> MaybeEx.isJust
                |> Debug.log "edge already exists?"

        newEdges =
            if alreadyExists then [] else [ newEdge ]
    in
        Graph.fromNodesAndEdges (Graph.nodes graph)
            (Graph.edges graph ++ newEdges)


moveNodeControl : Vector2d -> Graph.NodeId -> Model -> Model
moveNodeControl vec nodeId model =
    let
        updateElement element =
            { element
                | controlLocation =
                    Point2d.translateBy vec element.controlLocation
            }

        updateNode node =
            { node | label = updateElement node.label }

        updateNodeContext =
            Maybe.map (\ctx -> { ctx | node = updateNode ctx.node })

        updateGraph =
            Graph.update nodeId updateNodeContext
    in
        { model | graph = updateGraph model.graph }


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
    let
        edgeCount = model.graph |> Graph.edges |> List.length |> Debug.log "edge cont"
    in
    Html.div
        [ HtmlAttr.style containerStyle ]
        [ lazy viewStage model
        , viewControls model
        ]


viewControls : Model -> Html Msg
viewControls model =
    let
        nodeViews =
            List.map (viewNodeControl model) (Graph.nodes model.graph)

        edgeViews =
            List.map (viewEdgeControl model) (Graph.edges model.graph)

        newNodeAndNewEdge =
            case model.dragAction of
                Just (EdgeChangeEndNode edge endPoint) ->
                    let
                        startNode =
                            Graph.get edge.from model.graph

                        outgoingPortLocation =
                            .node >> nodeControlRect >> midRight

                        startPoint =
                            Maybe.map outgoingPortLocation startNode
                                |> Maybe.withDefault Point2d.origin
                    in
                        [ --viewLineWithArrow startPoint endPoint []
                        -- viewNodeControl model (newNode model)
                        ]

                _ ->
                    []
    in
        svg
            [ Attr.viewBox <| svgViewBoxString rootSize rootSize
            , HtmlAttr.style controlsStyle
            , Svg.Events.onMouseUp StopDragging
            -- , Svg.Events.onClick Deselect
            ]
            (nodeViews ++ edgeViews)


newNode model =
    Graph.Node (nextId model.graph) (Element Color.black Point2d.origin)

newNodeContext model =
    Graph.NodeContext (newNode model) IntDict.empty IntDict.empty

nextId =
    Graph.nodeIdRange >> Maybe.map Tuple.second >> Maybe.map ((+) 1) >> Maybe.withDefault 0


-- viewLineWithArrow : Point2d -> Point2d -> List (Svg.Attribute Msg) -> Svg Msg
-- viewLineWithArrow fromLocation toLocation attrs =
--     let
--         line =
--             LineSegment2d (fromLocation, toLocation)
--
--         lineView =
--             Svg.lineSegment2d
--                 ( attrs ++ [ Attr.stroke "grey", Attr.strokeDasharray "5,5" ] )
--                 line
--
--         arrowView =
--             LineSegment2d.direction line
--                 |> Maybe.map
--                     (arrowTriangle (LineSegment2d.interpolate line 0.75))
--                 |> Maybe.map
--                     ( Svg.triangle2d
--                         [ Attr.fill "grey", Attr.stroke "#ccc"
--                         , Draggable.mouseTrigger
--                             ( EdgeChangeEndNode)
--                         ]
--                     )
--                 |> Maybe.withDefault
--                     (Svg.g [] [])
--     in
--         Svg.g [] [ lineView, arrowView ]

incomingPortLocation =
    nodeControlRect >> midLeft


outgoingPortLocation =
    nodeControlRect >> midRight


viewEdgeControl : Model -> Graph.Edge Transformation -> Svg Msg
viewEdgeControl model edge =
    let
        lineView =
            Svg.lineSegment2d [ Attr.stroke "grey", Attr.strokeDasharray "5,5" ]

        arrowLocation =
            flip LineSegment2d.interpolate 0.75

        arrowView lineSeg =
            LineSegment2d.direction lineSeg
                |> Maybe.map
                    (arrowTriangle <| arrowLocation lineSeg)
                |> Maybe.map
                    ( Svg.triangle2d
                        [ Attr.fill "grey", Attr.stroke "grey"
                        , Draggable.mouseTrigger
                            (EdgeChangeEndNode edge (arrowLocation lineSeg))
                            DragMsg
                        ]
                    )
                |> Maybe.withDefault
                    (Svg.g [] [])

        edgeView lineSeg =
            Svg.g
                [ Svg.Events.onClick <| Select (Edge edge) ]
                [ lineView lineSeg
                , arrowView lineSeg
                ]

        getNode =
            flip Graph.get model.graph

        specialDragEndpoint =
            case model.dragAction of
                Just (EdgeChangeEndNode e endpoint) ->
                    if edgeEquals e edge then Just endpoint else Nothing

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
            model.selectedItem == Just (Node node)

        isHovering =
            model.hoverItem == Just (Node node) && not isSelected

        rect =
            nodeControlRect node

        inboundEdgePort =
            Circle2d { radius = controlSize / 12, centerPoint = midLeft rect }

        outboundEdgePort =
            Circle2d { radius = controlSize / 12, centerPoint = midRight rect }
    in
    Svg.g
        [ Svg.Events.onClick (Select (Node node))
        , Svg.Events.onMouseOver (MouseHover (Node node))
        , Svg.Events.onMouseOut MouseLeave
        , Attr.cursor <| if isSelected then "move" else ""
        , Draggable.mouseTrigger (MoveNodeControl node.id) DragMsg
        -- , Attr.opacity "0.5"
        ]
        [ Svg.polygon2d
            [ Attr.fill "#ccc"
            , Attr.stroke "#09f"
            , Attr.strokeDasharray <| if isHovering then "5, 5" else ""
            , Attr.strokeWidth <| if isSelected || isHovering then "3px" else "0"
            ]
            rect
        , Svg.circle2d [ Attr.fill "grey"] inboundEdgePort
        , Svg.circle2d
            [ Attr.fill "grey"
            , Draggable.mouseTrigger
                ( EdgeChangeEndNode (Graph.Edge node.id -1 transformationEmpty)
                    (midRight rect)
                )
                DragMsg
            ]
            outboundEdgePort
        , Svg.circle2d
            [ Attr.fill (colorToHex node.label.color)
            , Attr.opacity "0.5"
            ]
            (Circle2d
                { radius = controlSize * 3 / 8, centerPoint = centroid rect }
            )
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


controlsStyle =
    [ "order" => "1"
    -- , "flex-basis" => "75%"
    , "flex-grow" => "1"
    , "padding" => "20px"
    -- , "background" => "red"
    -- , "position" => "absolute"
    -- , "left" => "20px"
    -- , "top" => "20px"
    -- , "width" => "100px"
    ]


containerStyle =
    [ "display" => "flex"
    , "height" => "100%"
    ]


(=>) = (,)


viewStage : Model -> Html Msg
viewStage model =
    svg
        [ Attr.viewBox <| svgViewBoxString rootSize rootSize
        , HtmlAttr.style
            [ "background" => "grey"
            -- , "flex-basis" => "75%"
            , "flex-grow" => "2"
            -- , "position" => "relative"
            -- , "order" => "0"
            -- , "width" => "75%"
            -- , "margin" => "20px"
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
        |> Svg.scaleAbout Point2d.origin (halfSize / 2 * model.zoomScale)
        |> Svg.translateBy (Vector2d (halfSize, halfSize))


viewElement : Float -> Model -> Graph.NodeId -> Svg Msg
viewElement cumulativeScale model id =
    case
        ( Graph.get id model.graph
        , cumulativeScale * model.zoomScale * rootSize >= 10
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
                        , Attr.opacity "0.85"
                        -- , Svg.Events.onMouseOver ( MouseHover (StageNode node) )
                        ]
                        unitCircle

                newScale transformation =
                    transformation.scale * cumulativeScale

                viewChild (childId, transformation) =
                    viewElement (newScale transformation) model childId
                        |> Svg.scaleAbout Point2d.origin
                            (clamp 0 1 transformation.scale)
                        |> Svg.translateBy
                            (transformation.translation |> toVector2d)

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

midLeft =
    pointFromPolygon BoundingBox2d.minX BoundingBox2d.midY

midRight =
    pointFromPolygon BoundingBox2d.maxX BoundingBox2d.midY

midTop =
    pointFromPolygon BoundingBox2d.midX BoundingBox2d.minY

midBottom =
    pointFromPolygon BoundingBox2d.midX BoundingBox2d.maxY


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


toVector2d (radius, angle) =
    Vector2d <| fromPolar (radius, degrees angle)





-- MAIN


main =
    Html.program
        { init = init ! []
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
