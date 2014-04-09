namespace UGP.Core
    
    open System.Text

    type Parse(seed, chart : Chart, tokens : string seq) =

        let mutable parseTrees = Seq.empty

//        new(seed, chart) =
//            Parse(seed, chart, Seq.empty)

        member x.Seed with get() = seed
        member x.Chart with get() = chart
        member x.Tokens with get() = tokens
        member x.ParseTrees
            with get() =
                if (Seq.isEmpty parseTrees) then
                    parseTrees <- x.getParseTreeFor(Category.start, 0, Seq.length x.Tokens)
                    parseTrees
                else
                    parseTrees

        member x.getCompletedEdges(category, origin, index) =
            match chart.EdgeSets.TryFind(index) with
                | Some(v) ->
                    Set.filter (fun (e : Edge) ->
                        e.Origin = origin
                        && e.IsPassive
                        && e.DottedRule.Left = category) v
                | None -> Set.empty

        member x.getStatus() =
            if (x.getCompletedEdges(Category.start, 0, Seq.length tokens).IsEmpty) then
                Status.Reject
            else
                Status.Accept

        member x.getParseTreeFor(category, origin, index) =
            x.getCompletedEdges(category, origin, index)
            |> Seq.map (fun e -> ParseTree.create(e, None))

        member x.getParseTreeFor(edge : Edge) =
            if (chart.contains(edge)) then
                Some(ParseTree.create(edge, None))
            else
                None

        override x.Equals(another) =
            match another with
                | :? Parse as parse -> 
                    x.Tokens = parse.Tokens
                    && x.Seed = parse.Seed
                    && x.Chart = parse.Chart
                | _ -> false

        override x.GetHashCode() =
            38 * x.Tokens.GetHashCode() * x.Seed.GetHashCode() * x.Chart.GetHashCode()

        override x.ToString() =
            let status = x.getStatus()
            let builder = StringBuilder()
                                        .Append(status)
                                        .Append(": ")
                                        .Append(x.Seed.ToString())
                                        .Append(" -> ")
                                        .Append(x.Tokens)
                                        |> fun b -> if (status = Accept) then
                                                        b.Append(' ')
                                                         .Append('(')
                                                         .Append(Seq.length x.ParseTrees)
                                                         .Append(')')
                                                    else
                                                        b
            builder.ToString()