namespace UGP.Core

    open System.Text

    type Rule(left : Category, right : Category seq) =
        do
            match left.Type with
                | CategoryType.Terminal -> invalidArg "left" "category is a terminal"
                | _ -> ()

            if (Seq.length right = 0) then
                invalidArg "right" "haven't categories"

        new(left : Category, right : Category) =
            Rule(left, Seq.singleton right)

        member x.Left with get() = left
        member x.Right with get() = right

        //Test whether the rule is a pre-terminal production rule.
        member x.IsPreterminal() = 
            x.Right |> Seq.exists (fun c -> match c.Type with
                                                | CategoryType.Terminal -> true
                                                | _ -> false)

        //Test whether the rule has only one terminal token at the right side
        member x.IsSinglePreterminal() =
            (Seq.length x.Right = 1) && ((Seq.head x.Right).Type = CategoryType.Terminal)

        //Test whether the rule equals to another, with the same left and the right side
        override x.Equals(obj) =
            match obj with
                | :? Rule as rule -> 
                    x.Left = rule.Left
                    && Seq.forall (fun (one, another) -> one = another) (Seq.zip x.Right rule.Right)
                | _ -> false

        override x.GetHashCode() =
            x.Left.GetHashCode()

        //Return string representation of the production
        override x.ToString() =
            let builder = StringBuilder()
                            .Append(x.Left)
                            .Append(" -> ")
            x.Right 
            |> Seq.fold (fun (builder : StringBuilder) c -> builder.Append(c).Append(' ')) builder 
            |> (fun builder -> builder.ToString())