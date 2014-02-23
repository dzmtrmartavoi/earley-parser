namespace UGP.Core

open System

type CategoryType =
    | Terminal
    | NonTerminal

type Category(name : string, categoryType : CategoryType) = 
    member x.Type with get() = categoryType
    member x.Name with get() = name

    override x.Equals(obj) =
        match obj with
            | :? Category as another -> (x.Name = another.Name) && (x.Type = another.Type) 
            | _ -> false

    override x.GetHashCode() =
        x.Name.GetHashCode() * x.Type.GetHashCode()

    override x.ToString() =
        x.Name

    interface IComparable<Category> with
        member x.CompareTo(another) =
            x.Name.CompareTo another.Name

    interface IComparable with
        member x.CompareTo(another) =
            match another with
                | :? Category as anotherCat  -> (x :> IComparable<_>).CompareTo anotherCat
                | _ -> invalidArg "another" "is not a Category"