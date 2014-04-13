namespace UGP.Core

open Microsoft.FSharp.Collections
open System
open System.Collections.Generic

[<Sealed>]
type Chart(edgeSets : Map<int, Edge list>) =

    interface System.IEquatable<Chart> with
        member x.Equals another =
            x.Edges = another.Edges

    new() =
        Chart(Map.empty)

    member val Edges = edgeSets with get, set

    member x.subChart(f , t) =
        Chart(Map.filter (fun k _ -> k >= f && k <= t) x.Edges)

    member x.headChart(t) =
        Chart(Map.filter (fun k _ -> k <= t) x.Edges)

    member x.tailChart(f) =
        Chart(Map.filter (fun k _ -> k >= f) x.Edges)

    member x.getEdges(index) =
        x.Edges.[index]

    member x.contains(edge) =
        Map.exists (fun k edges -> List.exists (fun e -> e = edge) edges) x.Edges

    member x.indexOf(edge) =
        Map.tryPick (fun i edges -> 
            if (Seq.exists (fun e -> e = edge) edges) then
                Some(i)
            else
                None)

    member x.containsEdges(index) =
        x.Edges.ContainsKey(index)

    member x.countEdges() = 
        Map.fold (fun state _ edges -> state + List.length edges) 0 x.Edges

    member x.addEdge(index, edge) =
        if (index < 0) then
            invalidArg "index" "less than 0"
        else
            match Map.tryFind(index) x.Edges with
                | Some(edges) ->
                            if (not (List.exists (fun e -> e = edge) edges)) then
                                x.Edges <- Map.add index (edge :: edges) x.Edges
                                true
                            else
                                false
                | None ->
                        x.Edges <- Map.add index [edge] x.Edges
                        true

    override x.Equals(another) = 
        match another with
            | :? Chart as chart ->
                (x :> IEquatable<_>).Equals chart
            | _ -> false

    override x.GetHashCode() =
        37 * (1 + hash(x.Edges))