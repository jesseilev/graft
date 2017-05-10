module Graph.Extra exposing (..)

import Graph exposing (Graph)
import IntDict
import List.Extra as ListEx
import Maybe.Extra as MaybeEx


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


updateNode id updater =
    updateNodes (List.map (\n -> if n.id == id then updater n else n))


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
