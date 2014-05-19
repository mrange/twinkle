namespace twinkle

open silberman

open System
open System.Collections.Generic

open Fundamental
open Logical
open Logical.Events
open Logical.Properties

open SharpDX

module TwinkleGame = 

    let Facets = 4

    [<Measure>]
    type Columns

    [<Measure>]
    type Rows

    [<Measure>]
    type X

    [<Measure>]
    type Y

    type TwinkleColor = 
        | Red 
        | Orange
        | Yellow
        | Green
        | Blue
        | Indigo
        | Violet

    type Direction = 
        | Left
        | Up
        | Right
        | Down

    let DirectionToInt (d : Direction) = 
        match d with
        | Left  -> 0
        | Up    -> 1
        | Right -> 2
        | Down  -> 3

    let IntToDirection (i : int) = 
        match (i % Facets) with
        | 0 -> Left
        | 1 -> Up
        | 2 -> Right
        | 3 -> Down
        | _ -> Left

    type Cell = 
        {
            Colors                  : TwinkleColor[]
            mutable Direction       : Direction
            mutable VisualRotation  : AnimatedFloat
        }

        member x.GetColor (d : Direction) : TwinkleColor = 
            let dir = ((DirectionToInt d) + (DirectionToInt x.Direction)) % Facets
            x.Colors.[dir]

        member x.SetColor (d : Direction) (c : TwinkleColor) = 
            let dir = ((DirectionToInt d) + (DirectionToInt x.Direction)) % Facets
            x.Colors.[dir] <- c

        member x.Rotate (i : int) = 
            let dir = (DirectionToInt x.Direction) + i
            x.Direction <- IntToDirection dir
   
        static member New () = 
                        { 
                            Colors          = [| for i in 0..3 -> Red |]
                            Direction       = Left
                            VisualRotation  = 0.F |> Animated.Constant 
                        }

    type Board =
        {
            Cells   : Cell[][]
            Columns : int<Columns>
            Rows    : int<Rows>
        }
        static member New c r = 
                        { 
                            Cells   = [| for i in 0..(int <| r - 1<Rows>) -> [| for i in 0..(int <| c - 1<Columns>) -> Cell.New () |] |]
                            Columns = c
                            Rows    = r 
                        }

    let Visit 
        (isolated   : Cell->int<X>->int<Y>->Direction->unit                                 ) 
        (adjacent   : Cell->int<X>->int<Y>->Direction->Cell->int<X>->int<Y>->Direction->unit) 
        (b          : Board                                                                 ) 
        = 
        let lasty = int <| b.Rows - 1<Rows>
        let lastx = int <| b.Columns - 1<Columns>
        for y in 0..lasty do
            let dy = y * 1<Y>
            for x in 0..lastx do
                let dx = x * 1<X>
                let cell = b.Cells.[y].[x]

                if x = 0 then
                    isolated cell dx dy Left

                if x < lastx then
                    let ocell = b.Cells.[y].[x + 1]
                    adjacent cell dx dy Right ocell (dx + 1<X>) dy Left
                else
                    isolated cell dx dy Right

                if y = 0 then
                    isolated cell dx dy Up

                if y < lasty then
                    let ocell = b.Cells.[y + 1].[x]
                    adjacent cell dx dy Down ocell dx (dy + 1<Y>) Up
                else
                    isolated cell dx dy Down
                
            
    let CreateBoard (random : Random) c r =
        let color () = 
            let i = random.Next (0, 7)
            match i with
            | 0 -> Red 
            | 1 -> Orange
            | 2 -> Yellow
            | 3 -> Green
            | 4 -> Blue
            | 5 -> Indigo
            | 6 -> Violet
            | _ -> Red
        let b = Board.New c r
        b |> Visit 
                (fun c _ _ d -> c.SetColor d <| color ()) 
                (fun c _ _ d oc _ _ od -> 
                    let cl = color ()
                    c.SetColor d cl
                    oc.SetColor od cl
                )
        b

    let ShakeBoard (random : Random) (b : Board) =
        b |> Visit 
                (fun c _ _ d -> c.Rotate <| random.Next(0, Facets))
                (fun c _ _ d oc _ _ od -> 
                    c.Rotate    <| random.Next(0, Facets)
                    oc.Rotate   <| random.Next(0, Facets)
                )
        b

    let CheckWinCondition (b : Board) = 
        let mismatches = ref 0
        b |> Visit 
                (fun c _ _ d -> ())
                (fun c _ _ d oc _ _ od -> 
                    let cl  = c.GetColor d
                    let ocl = oc.GetColor od
                    if cl <> ocl then
                        mismatches := !mismatches + 1
                )
        !mismatches = 0

    module Elements = 
        open Visual

        type GameElement() =
            inherit Foundation.Element()

            let stroke      = SolidBrush Color.Black
            let fills       = 
                [
                    Red     , SolidBrush Color.Red   
                    Orange  , SolidBrush Color.Orange
                    Yellow  , SolidBrush Color.Yellow
                    Green   , SolidBrush Color.Green 
                    Blue    , SolidBrush Color.Blue  
                    Indigo  , SolidBrush Color.Indigo
                    Violet  , SolidBrush Color.Violet
                ] |> Map.ofList

            let createVisualFacet (c : Cell) (x : int<X>) (y : int<Y>) (direction : Direction) =
                let s = stroke                      |> Animated.Brush.Opaque
                let f = fills.[c.GetColor direction]|> Animated.Brush.Opaque
                let r = float32 Math.PI * 2.0F / 4.0F + float32 Math.PI * 2.0F * (float32 <| DirectionToInt direction) / (float32 Facets)
                let t = 
                    Matrix3x2.Rotation r                *
                    Matrix3x2.Scaling 100.F             *
                    Matrix3x2.Translation (200.F + 100.F * float32 x, 200.F + 100.F * float32 y)
                    |> Animated.Constant
                let sw= 0.02F       |> Animated.Constant
                VisualTree.Geometry (Triangle45x45x90, s, f, t, sw)

            let transform (state : ApplicationState)    = 
                let time    = state.CurrentTime
//                Matrix3x2.Rotation time             *
//                Matrix3x2.Scaling 100.F             *
//                Matrix3x2.Translation (4.F, 4.F)
                Matrix3x2.Identity

            let random  = Random 19740531

            let board   = CreateBoard random 5<Columns> 5<Rows> // |> ShakeBoard random

            let vt      =
                let facets = List<VisualTree>()
                board |> Visit 
                            (fun c x y d -> facets.Add <| createVisualFacet c x y d)
                            (fun c x y d oc ox oy od -> 
                                facets.Add <| createVisualFacet c x y d
                                facets.Add <| createVisualFacet oc ox oy od
                            )
                VisualTree.Group <| facets.ToArray ()
                    
    
            override x.OnMeasureContent a = 
                        Measurement.Fill



            override x.OnRenderContent  (o : Placement)
                                        (i : Placement) =
                        VisualTree.Transform (transform,vt)

    let Game (pvs : Foundation.PropertyValue list) = Logical.CreateElement<Elements.GameElement> pvs


                                                        