namespace UGP.Core.Tests

    open System
    open UGP.Core
    open Xunit
    open FsUnit.Xunit

    module CategoryTests =
        
        [<Fact>]
        let ``name should not be empty``() =
            (fun () -> Category(String.Empty, CategoryType.Terminal)) |> should throw typeof<ArgumentException>

        [<Fact>]
        let ``Type member test``() =
            let c = Category("x", CategoryType.Terminal)
            c.Type |> should equal Terminal

        [<Fact>]
        let ``Name member test``() =
            let c = Category("x", CategoryType.Terminal)
            c.Name |> should equal "x"
        
        [<Fact>]
        let ``equality test1``() =
            let c1 = Category("x", CategoryType.Terminal)
            let c2 = Category("x", CategoryType.Terminal)
            c1 = c2 |> should be True

        [<Fact>]
        let ``equality test2``() =
            let c1 = Category("x", CategoryType.Terminal)
            let c2 = Category("y", CategoryType.Terminal)
            c1 = c2 |> should be False

        [<Fact>]
        let ``equality test3``() =
            let c1 = Category("x", CategoryType.Terminal)
            let c2 = Category("x", CategoryType.NonTerminal)
            c1 = c2 |> should be False

        [<Fact>]
        let ``equality test4``() =
            let c1 = Category("x", CategoryType.Terminal)
            let c2 = Category("y", CategoryType.NonTerminal)
            c1 = c2 |> should be False

        [<Fact>]
        let ``ToString test``() =
            let c = Category("x", CategoryType.Terminal)
            c.ToString() |> should equal "x"