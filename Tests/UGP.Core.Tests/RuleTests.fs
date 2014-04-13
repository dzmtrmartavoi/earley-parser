namespace UGP.Core.Tests

    open System
    open UGP.Core
    open Xunit
    open FsUnit.Xunit

    module RuleTests =
        
        [<Fact>]
        let ``right side of rule should not be empty``() =
            let c = Category("X", NonTerminal)
            (fun () -> Rule(c, Seq.empty)
                       |> ignore)
            |> should throw typeof<ArgumentException>

        [<Fact>]
        let ``at the left side of rule should be a nonterminal``() =
            let cLeft = Category("x", Terminal)
            let cRight = Category("y", Terminal)
            (fun () -> Rule(cLeft, [cRight])
                       |> ignore)
            |> should throw typeof<ArgumentException>

        [<Fact>]
        let ``rule constructor test``() =
            let cLeft = Category("X", NonTerminal)
            let cRight = Category("x", Terminal)
            Rule(cLeft, Seq.singleton(cRight))

        [<Fact>]
        let ``Left member test``() =
            let cLeft = Category("X", NonTerminal)
            let cRight = Category("x", Terminal)
            let rule = Rule(cLeft, [cRight])
            rule.Left |> should equal cLeft

        [<Fact>]
        let ``Right member test``() =
            let cLeft = Category("X", NonTerminal)
            let cRight = Category("x", Terminal)
            let rule = Rule(cLeft, [cRight])
            List.ofSeq rule.Right = List.ofSeq [cRight] |> should be True

        [<Fact>]
        let ``should be a preterminal``() =
            let cLeft = Category("A", NonTerminal)
            let cRight1 = Category("X", NonTerminal)
            let cRight2 = Category("a", Terminal)
            let rule = Rule(cLeft, [cRight1; cRight2])
            rule.isPreterminal() |> should be True

        [<Fact>]
        let ``should be a single preterminal``() =
            let cLeft = Category("A", NonTerminal)
            let cRight = Category("a", Terminal)
            let rule = Rule(cLeft, [cRight])
            rule.isSinglePreterminal() |> should be True

        [<Fact>]
        let ``equality test1``() =
            let cLeft = Category("A", NonTerminal)
            let cRight = Category("b", Terminal)
            let rule = Rule(cLeft, [cRight])

            let cLeft2 = Category("A", NonTerminal)
            let cRight2 = Category("a", Terminal)
            let rule2 = Rule(cLeft, [cRight])
            rule = rule2 |> should be True

        [<Fact>]
        let ``equality test2``() =
            let cLeft = Category("A", NonTerminal)
            let cRight = Category("x", Terminal)
            let rule = Rule(cLeft, [cRight])

            let cLeft2 = Category("A", NonTerminal)
            let cRight2 = Category("a", Terminal)
            let rule2 = Rule(cLeft, [cRight2])
            rule = rule2 |> should be False