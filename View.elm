module View exposing (root, stageSize)

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
import Round as Format

import Graph.Extra as GraphEx
import OpenSolid.Vector2d.Extra as Vector2dEx
import OpenSolid.Extra exposing (..)
import Types exposing (..)




root : Model -> Html Msg
root model =
    Html.div
        [ HtmlAttr.style
            [ "display" => "grid"
            , "grid-template-columns" => "1fr 370px"
            , "height" => "100%"
            , "font-family" => "Sans-serif"
            , "color" => "grey"
            , "background" => "#eee"
            ]
        ]
        [ lazy viewStage model
        , viewControls model
        ]


viewStage : Model -> Html Msg
viewStage model =
    Html.section
        [ HtmlAttr.style
            [ "display" => "grid"
            , "grid-template-rows" => "1fr 50px" -- TODO htf does this work
            , "height" => "100%"
            -- , "justify-items" => "stretch"
            -- , "background" => "grey"
            ]
        ]
        [ svg
            [ Attr.viewBox <| svgViewBoxString stageSize stageSize
            , HtmlAttr.style
                [
                "grid-column" => "1 / 3"
                ,
                "grid-row" => "1 / 3"
                ,
                "background" => "grey"
                ,
                "cursor" =>
                    "move"
                    -- if model.dragAction == Just Pan then "-webkit-grabbing" else "-webkit-grab"
                -- ,
                -- "width" => (Vector2d.xComponent model.panOffset |> toString)
                ,
                "height" => "100%"
                ]
            , Draggable.mouseTrigger Pan DragMsg
            , Svg.Events.onMouseUp StopDragging
            -- , Html.Events.on "click" (Decode.succeed ZoomIn)
                -- ( Decode.map
                --     (ZoomIn << Point2d.translateBy (Vector2d (-stageSize / 2, -stageSize / 2)))
                --     clickPositionDecoder
                -- )
            ]
            [ lazy viewRootElement model
            ]
        , Html.div
            [ HtmlAttr.style
                [ "grid-row" => "2"
                , "grid-column" => "1"
                -- , "background" => "rgba(200, 100, 200, 0.5)"
                , "padding" => "8px"
                , "padding-top" => "0"
                , "display" => "grid"
                , "grid-template-columns" => "30px 30px 1fr"
                , "grid-gap" => "10px"
                , "align-items" => "end"
                , "justify-items" => "stretch"
                ]
            ]
            [ Html.button
                [ Html.Events.onClick ZoomIn
                , HtmlAttr.style
                    [ "font-size" => "20px"
                    , "background" => "rgba(0,0,0,0)"
                    , "color" => "white"
                    , "border" => "2px solid white"
                    , "grid-column" => "1"
                    ]
                ]
                [ Html.text "+" ]
            , Html.button
                [ Html.Events.onClick ZoomOut
                , HtmlAttr.style
                    [ "font-size" => "20px"
                    , "background" => "rgba(0,0,0,0)"
                    , "color" => "white"
                    , "border" => "2px solid white"
                    , "grid-column" => "2"
                    ]
                ]
                [ Html.text "-" ]
            ]
        ]


viewRootElement : Model -> Svg Msg
viewRootElement model =
    let halfSize = stageSize / 2 in
    lazy (viewElement 1 model) model.rootId
        |> Svg.scaleAbout Point2d.origin (halfSize * 0.6 * model.zoomScale)
        |> Svg.translateBy model.panOffset

viewElement : Float -> Model -> Id -> Svg Msg
viewElement cumulativeScale model id =
    case
        ( Graph.get id model.graph
        , cumulativeScale * model.zoomScale * stageSize >= 4
        )
        of
        (Just nodeContext, True) ->
            let
                element =
                    nodeContext.node.label

                parent =
                    shapeView nodeContext.node []

                newScale transformation =
                    transformation.scale * cumulativeScale

                viewChild (childId, transformation) =
                    viewElement (newScale transformation) model childId
                        |> Svg.scaleAbout Point2d.origin
                            (clamp 0 1 transformation.scale)
                        |> Svg.rotateAround Point2d.origin
                            (degrees transformation.rotation)
                        |> Svg.translateBy
                            transformation.translation

                children =
                    nodeContext.outgoing
                        |> IntDict.toList
                        |> List.map viewChild
            in
                Svg.g
                    []
                    (parent :: children)

        _ ->
            Svg.g [][]


viewControls : Model -> Html Msg
viewControls model =
    Html.section
        [ HtmlAttr.style
            [ "display" => "grid"
            , "grid-template-rows" => "65% 35%"
            ]
        ]
        [ viewGraph model, viewDetailsContainer model ]


viewGraph : Model -> Html Msg
viewGraph model =
    let
        nodeViews =
            List.map (viewNode model) (Graph.nodes model.graph)

        edgeViews =
            List.map (viewEdge model) (Graph.edges model.graph)
    in
        Html.section []
            [ svg
                [ Attr.viewBox <| svgViewBoxString stageSize stageSize
                , Svg.Events.onMouseUp StopDragging
                , HtmlAttr.style
                    [
                    "background" => "#eee"
                    ,
                    "height" => "100%"
                    , "width" => "100%"
                    , "cursor" =>
                        case model.dragAction of
                            Just _ -> "-webkit-grabbing"
                            _ -> "default"
                    ]
                -- , Svg.Events.onClick Deselect
                ]
                (nodeViews ++ edgeViews)
            ]


viewNode : Model -> Node -> Svg Msg
viewNode model node =
    let
        isSelected =
            model.selectedItem == Just (Node node.id)

        isHoveringOverShape =
            model.hoverItem == Just (NodeShape node.id)

        isHoveringOverBox =
            model.hoverItem == Just (NodeBox node.id)

        isBeingDraggedTo =
            case model.dragAction of
                Just (EdgeChangeEndNode _ _) ->
                    isHoveringOverBox || isHoveringOverShape

                _ ->
                    False

        box =
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
            , Attr.cursor <|
                if MaybeEx.isNothing model.dragAction then "-webkit-grab" else ""
            , Attr.stroke "grey"
            , Attr.strokeWidth <|
                if isHoveringOverShape || isBeingDraggedTo || isSelected then "2px" else "0"
            , Draggable.mouseTrigger (MoveNodeControl node) DragMsg
            , Svg.Events.onMouseOver (StartHover (NodeBox node.id))
            , Svg.Events.onMouseOut StopHover
            ]
            box
        , shapeView node
            [ Attr.cursor <| if MaybeEx.isNothing model.dragAction then "pointer" else ""
            , Svg.Events.onClick (Select (Node node.id))
            , Svg.Events.onMouseOver (StartHover (NodeShape node.id))
            , Svg.Events.onMouseOut StopHover
            ]
                |> Svg.scaleAbout Point2d.origin (controlSize * 0.25)
                |> Svg.placeIn (Frame2d.at (centroid box))
        , Svg.circle2d
            [ Attr.fill "grey"
            , Attr.stroke "grey"
            , Attr.strokeWidth <| if isBeingDraggedTo then "20px" else "0"
            ]
            inboundEdgePort
        , Svg.circle2d
            [ Attr.fill "grey"
            , Attr.stroke "grey"
            , Attr.strokeWidth
                <| if model.hoverItem == Just (OutgoingPort node.id) then "20px" else "0"
            , Attr.cursor "-webkit-grab"
            , Draggable.mouseTrigger
                ( EdgeChangeEndNode (Graph.Edge node.id -1 transformationDefault)
                    (midRight box)
                )
                DragMsg
            , Svg.Events.onMouseOver (StartHover (OutgoingPort node.id))
            , Svg.Events.onMouseOut StopHover
            ]
            outboundEdgePort
        ]


viewEdge : Model -> Edge -> Svg Msg
viewEdge model edge =
    let
        isSelected =
            model.selectedItem == Just (Edge edge.from edge.to)

        lineView =
            Svg.lineSegment2d
                [ Attr.stroke "grey" --<| if isSelected then "yellow" else "grey"
                , Attr.strokeWidth <| if isSelected then "2px" else "1px"
                , Attr.strokeDasharray <| if isSelected then "" else "5,5"
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
                        , Attr.cursor "-webkit-grab"
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
                    [ Attr.opacity <| if isSelected then "0" else "0"
                    , Attr.strokeWidth "10px"
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

        chopALittleOffBothEnds : LineSegment2d -> LineSegment2d
        chopALittleOffBothEnds lineSeg =
            LineSegment2d.scaleAbout (LineSegment2d.midpoint lineSeg) 0.95 lineSeg

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
                            |> chopALittleOffBothEnds
                    )

            ( Just fromCtx, _, Just endpoint ) ->
                edgeView
                    ( LineSegment2d
                        ( outgoingPortLocation fromCtx.node
                        , endpoint
                        )
                            |> chopALittleOffBothEnds
                    )

            _ ->
                Svg.g [] []


viewDetailsContainer : Model -> Html Msg
viewDetailsContainer model =
    Html.section
        [ HtmlAttr.style
            [
            "background" => "linear-gradient(to left, #ccc, #eee)"
            ,
            "padding" => "30px"
            -- , "border-top" => "1px solid #ccc"
            ]
        ]
        [ case model.selectedItem of
            Just (Node nodeId) ->
                acceptMaybe (Html.text "") viewNodeDetail
                    <| GraphEx.getNode nodeId model.graph


            Just (Edge from to) ->
                (viewEdgeDetail model |> acceptMaybe (Html.text ""))
                    <| GraphEx.getEdge from to model.graph

            _ ->
                Html.text ""
        ]


viewEdgeDetail : Model -> Edge -> Svg Msg
viewEdgeDetail model edge =
    let
        translationX =
            .translation edge.label |> Vector2d.xComponent

        translationY =
            .translation edge.label |> Vector2d.yComponent
    in
    Html.div
        [ HtmlAttr.style
            [ "display" => "grid"
            , "grid-template-columns" => "50% 50%"
            , "grid-gap" => "10px"
            ]
        ]
        [ Html.div []
            [ fieldsetView ("X: " ++ (Format.round 2 translationX))
                <| sliderView translationX -2 2 0.05
                    [ Html.Events.onInput (TranslationX edge |> floatMsgFromString) ] []
            , fieldsetView ("Y: " ++ (Format.round 2 translationY))
                <| sliderView translationY -2 2 0.05
                    [ Html.Events.onInput (TranslationY edge |> floatMsgFromString) ] []
            ]
        , Html.div []
            [ fieldsetView ("Scale: " ++ (.scale edge.label |> Format.round 2))
                <| sliderView (.scale edge.label) 0 0.9 0.01
                    [ Html.Events.onInput (ChangeScale edge |> floatMsgFromString) ] []
            , fieldsetView ("Rotation: " ++ (.rotation edge.label |> toString))
                <| sliderView (.rotation edge.label) 0 360 5
                    [ Html.Events.onInput (ChangeRotation edge |> floatMsgFromString) ] []
            ]
        ]


viewNodeDetail : Node -> Html Msg
viewNodeDetail ({label} as node) =
    let
        shapeChooser shape =
            svg
                [ Attr.viewBox "-1.25 -1.25 2.5 2.5"
                , HtmlAttr.style
                    [ "height" => "50px"
                    , "margin-right" => "8px"
                    ]
                , Svg.Events.onClick <| ChangeShape node (toString shape)
                -- , Attr.opacity <| if label.shape == shape then "1" else "0.25"
                ]
                [ shapeView { node | label = { label | shape = shape, opacity = 1 } }
                    [ Attr.stroke
                        <| if label.shape == shape then (colorToHex label.color) else "#aaa"
                    ,
                    Attr.strokeWidth "0.1" --<| if label.shape == shape then "0.15" else "0.1"
                    , Attr.fill
                        <| if label.shape == shape then (colorToHex label.color) else "rgba(0,0,0,0)"
                    , Attr.cursor "pointer"
                    ]
                ]
    in
    Html.div []
        [ fieldsetView ""
            <| Html.div []
                ( List.map shapeChooser
                    [ Square, Triangle, Circle, HalfWedge, QuarterWedge ]
                )
        , fieldsetView ""
            <| Html.div
                [ HtmlAttr.style
                    [ "display" => "grid"
                    , "grid-template-columns" => "25% 75%"
                    , "grid-gap" => "10px"
                    , "align-items" => "center"
                    ]
                ]
                [ Html.input
                    [ HtmlAttr.type_ "color"
                    , HtmlAttr.value (colorToHex node.label.color)
                    , Html.Events.onInput (ChangeColor node)
                    , HtmlAttr.style [ "width" => "100%" ]
                    ]
                    []
                , sliderView node.label.opacity 0 1 0.01
                    [ Html.Events.onInput (floatMsgFromString (ChangeOpacity node))
                    , HtmlAttr.style
                        [ "background" =>
                            ( "linear-gradient(to right, rgba(0,0,0,0), "
                                ++ colorToCssRgba node.label.color
                            )
                        ]
                    ]
                    []
                ]
        , Html.hr
            [ HtmlAttr.style [ "border-top" => "1px solid #ccc", "border-bottom" => "0", "margin-bottom" => "20px"]]
            []
        , fieldsetView ""
            <| Html.button
                [ Html.Events.onClick Delete
                , HtmlAttr.style
                    [ "height" => "40px"
                    , "width" => "40px"
                    , "background" => "rgba(0,0,0,0)"
                    , "color" => "red"
                    , "font-family" => "Sans-serif"
                    , "font-size" => "20px"
                    , "float" => "right"
                    -- , "padding" => "0 10px 0 10px"
                    , "border" => "2px solid red"
                    , "opacity" => "0.5"
                    ]
                ]
                [ Html.text "×" ]
        ]



-- REUSABLE VIEWS


shapeView : Node -> List (Svg.Attribute Msg) -> Svg Msg
shapeView node attrs =
    unitShapeSvg node.label.shape <|
        [ Attr.fill (colorToHex node.label.color)
        , Attr.opacity (toString node.label.opacity)
        -- , Svg.Events.onMouseOver ( StartHover (StageNode node) )
        ]
        ++ attrs


sliderView :
    Float -> Float -> Float -> Float -> List (Svg.Attribute Msg)
    -> List (Html Msg) -> Html Msg
sliderView value min max step attrs =
    Html.input <|
        [ HtmlAttr.type_ "range"
        , HtmlAttr.value (toString value)
        , HtmlAttr.min (toString min)
        , HtmlAttr.max (toString max)
        , HtmlAttr.step (toString step)
        , HtmlAttr.style
            [ "-webkit-appearance" => "none"
            , "height" => "2px"
            , "background" => "#aaa"
            ]
        ]
        ++ attrs


fieldsetView : String -> Html Msg -> Html Msg
fieldsetView labelText child =
    Html.fieldset
        [ HtmlAttr.style
            [ "padding" => "0.5 em"
            , "padding-left" => "0"
            , "border-width" => "0"
            , "margin-top" => "0.5em"
            ]
        ]
        [ Html.label [] [ Html.text labelText ]
        , child
        ]


unitShapeSvg : Shape -> List (Svg.Attribute Msg) -> Svg Msg
unitShapeSvg shape =
    case shape of
        Circle ->
            flip Svg.circle2d unitCircle

        Square ->
            flip Svg.polygon2d unitSquare

        Triangle ->
            flip Svg.triangle2d unitTriangle

        HalfWedge ->
            flip Svg.polygon2d (arcToPolygon unitHalfWedge)

        QuarterWedge ->
            flip Svg.polygon2d (arcToPolygon unitQuarterWedge)


-- VIEW HELPERS


incomingPortLocation : Node -> Point2d
incomingPortLocation =
    nodeControlRect >> edgeLeft >> flip LineSegment2d.interpolate 0.2


outgoingPortLocation : Node -> Point2d
outgoingPortLocation =
    nodeControlRect >> edgeRight >> flip LineSegment2d.interpolate 0.8


(=>) = (,)


floatMsgFromString : (Float -> Msg) -> String -> Msg
floatMsgFromString msgConstructor =
    String.toFloat
        >> Result.map msgConstructor
        >> Result.withDefault NoOp


acceptMaybe : b -> (a -> b) -> Maybe a -> b
acceptMaybe default func =
    Maybe.map func >> Maybe.withDefault default


svgViewBoxString w h =
    "0 0 " ++ (toString w) ++ " " ++ (toString h)


nodeControlRect : Node -> Polygon2d
nodeControlRect {label} =
    uncurry rectangle2d
        (Point2d.coordinates label.controlLocation) controlSize controlSize


arrowTriangle : Point2d -> Direction2d -> Triangle2d
arrowTriangle tip direction =
    let
        back =
            Direction2d.scaleBy (controlSize / 4) (Direction2d.flip direction)

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


clickPositionDecoder : Decoder Point2d
clickPositionDecoder =
    Decode.map2 (\x y -> Point2d ( toFloat x, toFloat y ))
        (Decode.field "offsetX" Decode.int)
        (Decode.field "offsetY" Decode.int)


-- MODEL HELPERS


transformationDefault =
    { translation = Vector2d.zero
    , scale = 0.5
    , rotation = 0
    }


-- VIEW CONFIG

stageSize =
    500

controlSize =
    80
