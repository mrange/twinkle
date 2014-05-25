open twinkle
open silberman

open Fundamental
open Logical
open Elements
open Elements.Events
open Elements.Properties

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
                TwinkleGame.Game
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
