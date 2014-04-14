namespace UGP.Drawing

    open UGP.Core

    open System
    open System.Globalization
    open System.Windows
    open System.Windows.Controls
    open System.Windows.Shapes
    open System.Windows.Media
    open System.Windows.Input

    type ParseTreeView(tree : ParseTree) as x =
        inherit ContentControl()  

        let mutable canvas = Canvas()

        do
            x.Content <- canvas

        member x.Tree with get() = tree
            

        override x.OnRender(dc) =
            base.OnRender(dc)

            let scale = 160.0
            let nodeWidth = 75.0
            let nodeHeight = 30.0

            let renderNode text x y =
                let node = TextBlock()
                node.Text <- text
                node.HorizontalAlignment <- HorizontalAlignment.Center
                node.VerticalAlignment <- VerticalAlignment.Center
                node.Foreground <- Brushes.DarkSlateBlue

                let nodeBorder = Border();
                nodeBorder.BorderThickness <- Thickness(1.0)
                nodeBorder.BorderBrush <- Brushes.DarkSlateBlue
                nodeBorder.Width <- nodeWidth
                nodeBorder.Height <- nodeHeight
                nodeBorder.Child <- node

                canvas.Children.Add(nodeBorder) |> ignore
                let xPos = x
                let yPos = y
                Canvas.SetLeft(nodeBorder, x)
                Canvas.SetTop(nodeBorder, y)

            let renderLine x1 y1 x2 y2 =
                let line = Line()
                line.Stroke <- Brushes.DarkSeaGreen
                line.StrokeThickness <- 2.0
                line.X1 <- x1
                line.X2 <- x2
                line.Y1 <- y1
                line.Y2 <- y2
                
                canvas.Children.Add(line) |> ignore

            let rec drawTree (tree : ParseTree) x y scale =
                let (subtreesXWidth, _, subtreesXY) = Seq.fold (fun (x, y, realSubtreesXY) child ->
                                                                                                    let (subtreeWidth, realSubtreeX, realSubtreeY) = drawTree child x y scale
                                                                                                    (subtreeWidth, y, (realSubtreeX, realSubtreeY)::realSubtreesXY))
                                                                                                    (x, y + 1.0, List.empty) tree.Children
                let realXLevel = (x + (subtreesXWidth - x) / 2.0)
                let realYLevel = y
                renderNode (tree.Category.ToString()) (realXLevel * scale) (realYLevel * scale)
                List.iter (fun (cx, cy) -> 
                                            renderLine (realXLevel * scale + nodeWidth / 2.0) (realYLevel * scale + nodeHeight) (cx * scale + nodeWidth / 2.0) (cy * scale)) subtreesXY
                (subtreesXWidth + 1.0, realXLevel, realYLevel)
                    
            drawTree x.Tree 0.0 0.0 scale |> ignore

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module ParseTreeView =
        
        let show(tree) = 
            let view = ParseTreeView(tree)
            let w = Window()
            w.Content <- view
            w.Title <- tree.ToString()
            let app = Application()
            app.Run(w) |> ignore  
