namespace UGP.Core
    
    type EarleyParser(grammar : Grammar) =

        static let rec predictForEdge(grammar : Grammar, chart : Chart, edge: Edge, index) =
            let active = edge.DottedRule.ActiveCategory
            match active with
                | Some(c) when grammar.HasRule(c) ->
                    grammar.GetRules(c)
                        |> Seq.iter (fun r ->
                                            let newEdge = Edge.predictFor(r, index)
                                            if (chart.addEdge(index, newEdge)) then
                                                predictForEdge(grammar, chart, newEdge, index))
                | _ -> ()
        
        static let rec completeForEdge(chart : Chart, edge : Edge, index) =
            let eo = edge.Origin
            if (edge.IsPassive
                && chart.containsEdges(eo)) then
                chart.getEdges(eo)
                    |> Seq.filter (fun oEdge -> not oEdge.IsPassive)
                    |> Seq.filter (fun oEdge -> oEdge.DottedRule.ActiveCategory.Value = edge.DottedRule.Left)
                    |> Seq.iter (fun oEdge ->
                                            let newEdge = Edge.complete(oEdge, edge)
                                            if (chart.addEdge(index, newEdge)) then
                                                completeForEdge(chart, newEdge, index))
        
        member x.Grammar with get() = grammar

        member x.predict(grammar : Grammar, chart : Chart, index) =
            if (chart.containsEdges(index)) then
                chart.getEdges(index)
                    |> Seq.iter (fun e -> predictForEdge(grammar, chart, e, index))

        member x.scan(chart : Chart, index, token) =
            if (chart.containsEdges(index)) then
                chart.getEdges(index)
                    |> Seq.filter (fun e -> not e.IsPassive)
                    |> Seq.iter (fun e ->
                                        let rule = e.DottedRule
                                        if (rule.ActiveCategory.Value.Type = Terminal
                                            && (rule.ActiveCategory.Value.Name = token)) then
                                            let newEdge = Edge.scan(e, token)
                                            let successor = index + 1
                                            chart.addEdge(successor, newEdge)
                                                |> ignore)

        member x.complete (chart : Chart, index) =
            if (chart.containsEdges(index)) then
                chart.getEdges(index)
                    |> Seq.iter (fun e -> completeForEdge(chart, e, index))

        member x.parse(tokens : string seq, seed) =
            let seedEdge = Edge(DottedRule.startRule(seed), 0)
            let chart = Chart()
            chart.addEdge(0, seedEdge)
                |> ignore
            tokens
                |> Seq.iteri (fun i token ->
                                        x.predict(x.Grammar, chart, i)
                                        x.scan(chart, i + 1, token)
                                        x.complete(chart, i + 1)
                                        x.predict(x.Grammar, chart, i + 1))
            let parse = Parse(seed, chart, tokens)
            parse

        member x.recognize(tokens : string seq, seed) =
            x.parse(tokens, seed)
             .getStatus()

        
