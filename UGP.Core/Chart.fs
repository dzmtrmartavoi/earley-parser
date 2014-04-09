namespace UGP.Core

open Microsoft.FSharp.Collections
open System
open System.Collections.Generic

[<Sealed>]
type Chart(edgeSets : Map<int, Set<Edge>>) =

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
            |> Set.toSeq

    member x.contains(edge) =
        match (Map.tryPick (fun k (v : Set<Edge>) -> 
            if (v.Contains(edge)) then Some(k) else None) x.EdgeSets) with
                | Some(_) -> true
                | None -> false

    member x.indexOf(edge) =
        (Map.tryPick (fun k (v : Set<Edge>) -> 
            if (v.Contains(edge)) then Some(k) else None) x.EdgeSets)

    member x.containsEdges(index) =
        x.EdgeSets.ContainsKey(index)

    member x.countEdges() = 
        Map.fold (fun state _ (set : Set<Edge>) -> state + set.Count) 0 x.EdgeSets

    member x.addEdge(index, edge) =
        if (index < 0) then
            invalidArg "index" "less than 0"
        else
            match Map.tryFind(index) x.EdgeSets with
                | Some(v) ->
                            if (not (v.Contains(edge))) then
                                x.EdgeSets <- Map.add index (v.Add(edge)) x.EdgeSets
                                true
                            else
                                false
                | None ->
                        x.EdgeSets <- Map.add index (Set.singleton(edge)) x.EdgeSets
                        true

    override x.Equals(another) = 
        match another with
            | :? Chart as chart ->
                (x :> IEquatable<_>).Equals chart
            | _ -> false

    override x.GetHashCode() =
        37 * (1 + hash(x.EdgeSets))