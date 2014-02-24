namespace UGP.Core

type ParseTree(category : Category, parent : ParseTree option, children : ParseTree seq option) =
    member x.Category with get() = category
    member x.Parent with get() = parent
    member x.Children with get() = children

//  Returns true if equals(category) and one-level nearest nodes.
    override x.Equals(another) =
        match another with
            | :? ParseTree as tree -> 
                                    (x.Category = tree.Category)
                                    && (match (x.Parent, tree.Parent) with
                                            | Some(one), Some(another) -> one.Category = another.Category
                                            | None, None -> true
                                            | _ -> false)
                                    && (match (x.Children, tree.Children) with
                                            | Some(one), Some(another) when (Seq.length one) = (Seq.length another) -> Seq.forall2 ((fun f s -> f.Category = s.Category) : ParseTree -> ParseTree -> bool) one another
                                            | None, None -> true
                                            | _ -> false)
            | _ -> false

//  Returns hash code based on the category and one-level nearest nodes.
    override x.GetHashCode() =
        x.Category.GetHashCode() * (match x.Parent with
                                        | Some(parent) -> parent.Category.GetHashCode()
                                        | None -> 1)
                                * (match x.Children with
                                        | Some(children) -> children |> Seq.fold (fun hash child -> hash * child.Category.GetHashCode()) 1
                                        | None -> 1)

    new(category, parent) = ParseTree(category, parent, None)
    new(category) = ParseTree(category, None, None)