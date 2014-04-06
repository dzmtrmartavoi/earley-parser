namespace UGP.Core

//    Reflects the status completed by the Early parser.
    type Status =
        | Reject //String are rejected after parsing
        | Accept //Means that string is a valid string of the given grammar
        | Error //Error occured during parsing