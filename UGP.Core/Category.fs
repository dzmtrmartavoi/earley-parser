namespace UGP.Core

    type CategoryType =
        | Terminal
        | NonTerminal

    [<Sealed>]
    type Category(name : string, categoryType : CategoryType) = 

        interface System.IEquatable<Category> with
            member x.Equals category =
                x.Name = category.Name 
                && x.Type = category.Type

        interface System.IComparable<Category> with
            member x.CompareTo(another) =
                x.Name.CompareTo another.Name

        interface System.IComparable with
            member x.CompareTo(another) =
                match another with
                    | :? Category as anotherCat  -> 
                        (x :> System.IComparable<_>).CompareTo anotherCat
                    | _ -> invalidArg "another" "is not a Category"

        member x.Type with get() = categoryType
        member x.Name with get() = name

        override x.Equals(obj) =
            match obj with
                | :? Category as another ->
                    (x :> System.IEquatable<_>).Equals another
                | _ -> false

        override x.GetHashCode() =
            x.Name.GetHashCode() * x.Type.GetHashCode()

        override x.ToString() =
            x.Name

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Category =
        let start = Category("START", NonTerminal)