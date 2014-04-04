namespace UGP.Core

open System.Text

//Extension of the Rule type that maintains the dot position within the rule.
//Dotted rules are used by EarleyParser to keep track of how far within a rule processing has succeeded.
type DottedRule(rule : Rule, pos) =
    inherit Rule(rule.Left, rule.Right)

    member x.Position with get() = pos
    member x.ActiveCategory with get() = if (x.Position < Seq.length x.Right) then Some(Seq.nth x.Position x.Right) else None

    override x.Equals(another) =
        match another with
            | :? DottedRule as dottedRule -> base.Equals dottedRule && (x.Position = dottedRule.Position)
            | _ -> false

    override x.GetHashCode() =
        x.Position.GetHashCode() * base.GetHashCode()

    override x.ToString() =
        let builder = StringBuilder()
                        .Append(x.Left)
                        .Append(" -> ")
        x.Right 
        |> Seq.fold (fun (builder : StringBuilder) c -> builder.Append(c)) builder 
        |> (fun builder -> builder.ToString())

    //Creates a dotted rule based on the specified rule with biased position.
    static member advanceDot(dottedRule : DottedRule) =
        DottedRule(dottedRule, dottedRule.Position + 1)


    //Creates a new dotted rule for the given rule, with the dot position at the beginning of the relu's right side.
    new(rule) = DottedRule(rule, 0)

    
