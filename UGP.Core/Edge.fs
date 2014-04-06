namespace UGP.Core

    open System.Text

    [<Sealed>]
    type Edge(rule, origin, bases : Set<Edge>) =
        do 
            if(origin < 0) then
                invalidArg "origin" "should be equal or more than 0"

        interface System.IEquatable<Edge> with
            member x.Equals edge =
                x.DottedRule = edge.DottedRule
                    && x.Origin = edge.Origin
                    && x.Bases = edge.Bases

        interface System.IComparable<Edge> with
            member x.CompareTo another =
                x.Origin.CompareTo another.Origin

        interface System.IComparable with
            member x.CompareTo another =
                match another with
                    | :? Edge as edge  -> (x :> System.IComparable<_>).CompareTo edge
                    | _ -> invalidArg "another" "is not an Edge"

        member x.DottedRule with get() : DottedRule = rule
    
        member x.Origin with get() = origin
    
        member x.Bases with get() = bases

        member x.IsPassive with get() = match x.DottedRule.ActiveCategory with
                                        | None ->  true
                                        | _ -> false

        new(rule, origin) = Edge(rule, origin, Set.empty)
        new(rule) = Edge(rule, 0, Set.empty)

        override x.Equals(another) =
            match another with
                | :? Edge as edge ->
                    (x :> System.IEquatable<_>).Equals edge
                | _ -> false

        override x.GetHashCode() =
            x.DottedRule.GetHashCode() * x.Origin

        override x.ToString() =
            let builder = StringBuilder()
            builder
                .Append(origin)
                .Append('[')
                .Append(rule)
                .Append(']').ToString()

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Edge =
        let predictFor(rule, origin) =
            Edge(DottedRule(rule), origin)
    
        let scan(edge : Edge, token) =
            match edge.DottedRule.ActiveCategory with
                | None -> invalidArg "edge" "edge is passive"
                | Some(c) -> match c.Type with
                                | NonTerminal -> invalidArg "edge" "edge's active category is nonterminal"
                                | Terminal -> if (c.Name <> token) then
                                                invalidArg "token" "token incompatible with edge"
                                              else
                                                Edge(DottedRule.advanceDot edge.DottedRule, edge.Origin, edge.Bases.Add edge)

        let complete(toComplete : Edge, basis : Edge) =
            if (toComplete.IsPassive) then
                invalidArg "toComplete" "edge is pasive"
            else
                if (not basis.IsPassive) then
                    invalidArg "basis" "is not passive"
                else
                    if (basis.DottedRule.Position = 0
                        || toComplete.DottedRule.ActiveCategory.IsNone
                        || basis.DottedRule.Left <> toComplete.DottedRule.ActiveCategory.Value) then
                        invalidArg "basis" "toComplete is not completed by basis"
                    else
                        let newBasis = toComplete.Bases.Add basis
                        Edge(DottedRule.advanceDot toComplete.DottedRule, toComplete.Origin, newBasis)