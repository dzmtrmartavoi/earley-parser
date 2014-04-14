namespace UGP.Drawing.Runner

    open UGP.Core
    open UGP.Drawing

    open System
    open System.Globalization
    open System.Windows

    module Runner =
        
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


        [<EntryPoint>]
        [<STAThread>]
        let main argv = 
            let tokens = 
                [the.Name;
                boy.Name;
                left.Name]

            ParseTreeView.show(Seq.exactlyOne (testGrammarParser.parse(tokens, S).ParseTrees))
            0
