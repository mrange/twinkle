open twinkle
open silberman

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
                        Text    .Value "Click blocks to rotate them"
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
                        Text        .Value "Make sure all adjacent facets have the same color"
                    ]
            ]

    let app = App.Show "Test app" 1024 1024 body

    Async.StartImmediate app

    0
