namespace UGP.Core

    open System.Text

//     A parse tree that represents the derivation of a string based on the rules in a grammar.
//     Parse trees recursively contain x.Children - other parse trees, so they can be iterated through to
//     find the entire derivation of a category. A parse tree can also be 
//     traversed upward by getting x.Parent for each successive parse
//     tree until it returns None.
    [<Sealed>]
    type ParseTree(category : Category, parent : ParseTree option, children : ParseTree seq) =

        interface System.IEquatable<ParseTree> with
            member x.Equals tree =
                x.Category = tree.Category
                && match (x.Parent, tree.Parent) with
                        | Some(one), Some(another) -> one.Category = another.Category
                        | None, None -> true
                        | _ -> false
                && (Seq.length x.Children) = (Seq.length tree.Children)
                && Seq.forall2 ((fun f s -> f.Category = s.Category) : ParseTree -> ParseTree -> bool) x.Children tree.Children

        member x.Category with get() = category
        member x.Parent with get() = parent
        member val Children = children with get, set

//        Returns true if equals(category) and one-level nearest nodes.
        override x.Equals(another) =
            match another with
                | :? ParseTree as another -> 
                    (x :> System.IEquatable<_>).Equals another
                | _ -> false

//        Returns hash code based on the category and one-level nearest nodes.
        override x.GetHashCode() =
            x.Category.GetHashCode() * match x.Parent with
                                            | Some(parent) -> parent.Category.GetHashCode()
                                            | None -> 1
                                    *  Seq.fold (fun hash child -> hash * (child : ParseTree).Category.GetHashCode()) 1 x.Children

//       Gets a string representation of this parse tree.
        override x.ToString() =
            let builder = StringBuilder()
                                        .Append('[')
                                        .Append(x.Category)
            x.Children
            |> Seq.fold (fun (seed : StringBuilder) child -> seed.Append(child)) builder
            |> fun x -> x.ToString()
    
        new(category, parent) = ParseTree(category, parent, Seq.empty)
        new(category) = ParseTree(category, None, Seq.empty)

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module ParseTree =
        let rec create(edge : Edge, parent : ParseTree option) =
            let (e, parentTree) = 
                if (edge.DottedRule.Left = Category.start) then
                    (Seq.exactlyOne edge.Bases, None)
                else
                    (edge, if (parent.IsSome
                               && parent.Value.Category = Category.start) then
                                None 
                           else Some(parent.Value))

            if (e.IsPassive) then
                let newTree = ParseTree(e.DottedRule.Left, parentTree, Seq.empty)
                newTree.Children <- Seq.fold (fun childs basis -> create(basis, Some(newTree))::childs) [ ] edge.Bases
                newTree
            else
                ParseTree(edge.DottedRule.ActiveCategory.Value, parentTree, Seq.empty)