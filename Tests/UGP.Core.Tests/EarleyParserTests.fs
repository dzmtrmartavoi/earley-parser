namespace UGP.Core.Tests

    open UGP.Core
    open Xunit
    open FsUnit.Xunit

    // Grammar (BNF form):
    // S -> NP VP
    // NP -> DET N
    // VP -> left
    // DET -> a
    // DET -> the
    // N -> boy
    // N -> girl

    // Parse sample: the boy left

    module EarleyParserTests =

        let S = Category("S", NonTerminal)
        let NP = Category("NP", NonTerminal)
        let VP = Category("VP", NonTerminal)
        let DET = Category("DET", NonTerminal)
        let N = Category("N", NonTerminal)

        let left = Category("left", Terminal)
        let the = Category("the", Terminal)
        let girl = Category("girl", Terminal)
        let boy = Category("boy", Terminal)
        let a = Category("a", Terminal)

        let rule1 = Rule(S, [NP; VP])
        let rule2 = Rule(NP, [DET; N])
        let rule3 = Rule(VP, [left])
        let rule4 = Rule(DET, [a])
        let rule5 = Rule(DET, [the])
        let rule6 = Rule(N, [boy])
        let rule7 = Rule(N, [girl])

        let testGrammar = Grammar("TestGrammar", [rule1;
                                                 rule2;
                                                 rule3;
                                                 rule4;
                                                 rule5;
                                                 rule6;
                                                 rule7;])

        let testGrammarParser = EarleyParser(testGrammar)

        [<Fact>]
        let ``recoznize test1``() =
            let tokens = 
                [the.Name;
                boy.Name;
                left.Name]

            testGrammarParser.recognize(tokens, S) |> should equal Accept

        [<Fact>]
        let ``recoznize test2``() =
            let tokens = 
                [the.Name;
                the.Name;
                left.Name]

            testGrammarParser.recognize(tokens, S) |> should equal Reject