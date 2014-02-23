namespace UGP.Core

open System
open System.Collections
open System.Collections.Generic
open System.Text

type Grammar(name, startToken) =
    let mutable rules : Map<Category, Rule list> = Map.empty

    member x.Name with get() = name

    member x.StartCategoryToken with get() = startToken

    member x.GetStartCategory() =
        Category(x.StartCategoryToken, CategoryType.NonTerminal)

    //Adds a production rule to grammar
    member x.AddRule(rule : Rule) =
        match rules.TryFind rule.Left with
            | Some(existingRules) -> 
                rules <- rules
                         |> Map.remove rule.Left 
                         |> Map.add rule.Left (rule :: existingRules) 
            | None -> 
                rules <- Map.add rule.Left [ rule ] rules

    //Test whether grammar has the specified rule.
    member x.HasRule(category) =
        rules.ContainsKey(category)

    member x.GetRules(category) = 
        rules.[category]

    member x.GetRules() =
        rules |> Map.fold (fun rs c r -> r @ rs) List.empty

    //Get single preterminal rule with the specified left category, produced the given string token
    member x.GetSinglePreterminal(left, token) : Rule option =
        if (x.HasRule(left)) then
            x.GetRules(left) |> List.pick (fun rule -> 
                                                        if ((Seq.head rule.Right).Name = token) then Some(Some(rule))
                                                        else None)
        else None

    //Returns string represenatation of this grammar
    override x.ToString() =
        let builder = StringBuilder().Append("[")
        x.GetRules() 
        |> List.fold (fun (builder : StringBuilder) rule -> builder.Append(rule).AppendLine(", ")) builder
        |> (fun builder -> builder.Append("]").ToString())