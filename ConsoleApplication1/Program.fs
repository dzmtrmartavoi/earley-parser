open ClassLibrary2;

[<EntryPoint>]
let main argv = 
    let c2 = ClassLibrary2.Class2(ClassLibrary1.Class1.GetApp())
    0