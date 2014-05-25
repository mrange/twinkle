open silberman

open SharpDX

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

    let app = App.Show "Test app" 1600 1200 (ColorDescriptor.Color Color.White) body

    Async.StartImmediate app

    0
