namespace UGP.Core

open Microsoft.FSharp.Collections
open System
open System.Collections.Generic

[<Sealed>]
type Chart(edgeSets : Map<int, Edge seq>) =

    interface System.IEquatable<Chart> with
        member x.Equals another =
            x.EdgeSets = another.EdgeSets

    new() =
        Chart(Map.empty)

    member val EdgeSets = edgeSets with get, set

    member x.subChart(f , t) =
        Chart(Map.filter (fun k _ -> k >= f && k <= t) x.EdgeSets)

    member x.headChart(t) =
        Chart(Map.filter (fun k _ -> k <= t) x.EdgeSets)

    member x.tailChart(f) =
        Chart(Map.filter (fun k _ -> k >= f) x.EdgeSets)

    member x.getEdges(index) =
        x.EdgeSets.[index]

    member x.contains(edge) =
        Map.exists (fun k (v : Edge seq) -> Seq.exists (fun e -> e = edge) v) x.EdgeSets

    member x.indexOf(edge) =
        Map.tryPick (fun i edges -> 
            if (Seq.exists (fun e -> e = edge) edges) then
                Some(i)
            else
                None)

    member x.containsEdges(index) =
        x.EdgeSets.ContainsKey(index)

    member x.countEdges() = 
        Map.fold (fun state _ edges -> state + Seq.length edges) 0 x.EdgeSets

    member x.addEdge(index, edge) =
        if (index < 0) then
            invalidArg "index" "less than 0"
        else
            match Map.tryFind(index) x.EdgeSets with
                | Some(edges) ->
                            if (not (Seq.exists (fun e -> e = edge) edges)) then
                                let edgeList = List.ofSeq edges
                                x.EdgeSets <- Map.add index (Seq.ofList (edge :: edgeList)) x.EdgeSets
                                true
                            else
                                false
                | None ->
                        x.EdgeSets <- Map.add index (Seq.singleton(edge)) x.EdgeSets
                        true

    override x.Equals(another) = 
        match another with
            | :? Chart as chart ->
                (x :> IEquatable<_>).Equals chart
            | _ -> false

    override x.GetHashCode() =
        37 * (1 + hash(x.EdgeSets))