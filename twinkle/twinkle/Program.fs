open twinkle
open silberman

open Elements
open Elements.Events
open Elements.Properties

[<EntryPoint>]
let main argv =
    let label (text : string) =
            Label
                [
                    Text    .Value      text
                    Foreground.Value    <| (SolidColor <| "#FFF".ToColorDescriptor ())
                ]

    let body =
        Stack
            [
                Orientation .Value  <| FromTop
                Margin  .Value      <| Thickness.Uniform 4.F
            ]
            [
                label "Click blocks to rotate them"
                TwinkleGame.Game
                    [
                        Bounds  .Value  <| BoundingBox.CenterMin
                    ]
//                TextButton "Click me!"
//                    [
//                    ]
//                    >>+ Clicked.Handler (fun e v -> true)

                label "Make sure all adjacent facets have the same color"
                TextButton "Start New Game!" []
            ]

    let background = "#190066".ToColorDescriptor ()
    let app = App.Show "Test app" 1024 1024 background body

    Async.StartImmediate app

    0
