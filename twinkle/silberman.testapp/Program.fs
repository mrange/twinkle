open silberman

// TODO: 
// 1. Use units to avoid mixing up floats representing different concepts
// 2. Make sure SharpDX isn't implicitly exported in the interface

open Fundamental

open Logical
open Logical.Events
open Logical.Properties

open SharpDX

module MyGame =

    open Visual

    type GameElement() =
        inherit Foundation.Element()

        let stroke      = SolidBrush Color.Black
        let fill        = SolidBrush Color.LightBlue   
        let transform   = 
            Matrix3x2.Identity*Matrix3x2.Scaling(100.F)*Matrix3x2.Translation(200.F, 200.F)
    
        override x.OnMeasureContent a = 
                    Measurement.Fill



        override x.OnRenderContent  (o : Placement)
                                    (i : Placement) =
                    let s = stroke      |> Animated.Brush.Opaque
                    let f = fill        |> Animated.Brush.Opaque
                    let t = transform   |> Animated.Constant
                    let sw= 0.02F       |> Animated.Constant
                    VisualTree.Geometry (EquilateralTriangle, s, f, t, sw)

    let Game (pvs : Foundation.PropertyValue list) = Logical.CreateElement<GameElement> pvs

[<EntryPoint>]
let main argv = 
    let body = 
        Stack 
            [
                Orientation.Value FromTop
            ]
            [
                Label 
                    [ 
                        Margin  .Value <| Thickness.Uniform 4.F
                        Text    .Value "Hi there!" 
                    ]
                MyGame.Game
                    [ 
                    ]
//                TextButton "Click me!"
//                    [ 
//                    ]
//                    >>+ Clicked.Handler (fun e v -> true)

                Label 
                    [ 
                        Margin      .Value <| Thickness.Uniform 4.F
                        Text        .Value "Hello there!" 
                    ]
            ]

    let app = App.Show "Test app" 1600 1200 body

    Async.StartImmediate app

    0
