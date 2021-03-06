﻿namespace silberman
open silberman.Internal

open SharpDX

open System
open System.Collections.Generic
open System.Threading

//    [<Measure>]
//    type Columns

type BrushKey               = int
type TextFormatKey          = int
type GeometryKey            = int
type TransformedGeometryKey = int
type Time                   = float32

type Point                  = float32*float32

[<Flags>]
type MouseButtonStates =
    | Empty     = 0x000
    | Left      = 0x001
    | Middle    = 0x002
    | Right     = 0x004

[<NoEquality>]
[<NoComparison>]
type MouseState =
    {
        ButtonState : MouseButtonStates
        Coordinate  : Vector2
    }
    static member New bs c = {ButtonState = bs; Coordinate = c}
    static member Zero = MouseState.New MouseButtonStates.Empty <| Vector2()

    member x.Transform (t : Matrix3x2) = MouseState.New x.ButtonState <| t.TransformPoint x.Coordinate

[<NoEquality>]
[<NoComparison>]
type ApplicationState =
    {
        CurrentTime     : Time
        CurrentMouse    : MouseState
    }
    static member New ct cm = {CurrentTime = ct; CurrentMouse = cm}

    member x.Transform (t : Matrix3x2) = ApplicationState.New x.CurrentTime <| x.CurrentMouse.Transform t


[<StructuralEquality>]
[<StructuralComparison>]
type ColorDescriptor =
    {
        Alpha   : float32
        Red     : float32
        Green   : float32
        Blue    : float32
    }
    // TODO: This type shouldn't export SharpDX types
    static member ARGB  a r g b     = {Alpha = a; Red = r; Green = g; Blue = b}
    static member RGB   r g b       = ColorDescriptor.ARGB 1.F r g b
    static member Color (c : Color) = let toFloat (b : byte) = (float32 b) / 255.0F
                                      ColorDescriptor.ARGB (toFloat c.A) (toFloat c.R) (toFloat c.G) (toFloat c.B)
    static member Fault             = ColorDescriptor.Color Color.Red
    member x.ToColor4               = Color4(x.Red, x.Green, x.Blue, x.Alpha)
    member x.ToColor3               = Color3(x.Red, x.Green, x.Blue)

    member x.Transparency a         = ColorDescriptor.ARGB a x.Red x.Green x.Blue

type BrushExtendMode    =
        | ClampBrush
        | WrapBrush
        | MirrorBrush

type GradientStop       =
        {
            Position: float32
            Color   : ColorDescriptor
        }
        static member New p c =
            {
                Position    = p
                Color       = c
            }

[<StructuralEquality>]
[<StructuralComparison>]
type BrushDescriptor    =
    | Transparent
    | SolidColor        of ColorDescriptor
    | LinearGradient    of Start:   Point   * End:      Point   * Mode: BrushExtendMode * Stops: GradientStop list
    | RadialGradient    of Center:  Point   * Offset:   Point   * Radius: Point * Mode: BrushExtendMode * Stops: GradientStop list

[<StructuralEquality>]
[<StructuralComparison>]
type TextFormatDescriptor   =
    {
        FontFamily  : string
        FontSize    : float32
    }
    static member New ff fs = {FontFamily = ff; FontSize = fs}

[<StructuralEquality>]
[<StructuralComparison>]
type PathSegment     =
//    | Arc               of
    | Bezier            of Control0:Point   * Control1:Point    * EndPoint:  Point
    | QuadraticBezier   of Control: Point   * EndPoint:Point
    | Line              of Point:   Point

[<StructuralEquality>]
[<StructuralComparison>]
type PathFigure         =
    {
        Start       : Point
        Filled      : bool
        Closed      : bool
        Segments    : PathSegment list
    }

[<StructuralEquality>]
[<StructuralComparison>]
type GeometryDescriptor     =
    | PathGeometry      of PathFigure list
    | PolygonGeometry   of Point list

type AnimationEase      = Time->Time->float32->float32->ApplicationState->float32

type AnimatedFloat      = ApplicationState->float32
type AnimatedVector2    = ApplicationState->Vector2
type AnimatedRectangleF = ApplicationState->RectangleF
type AnimatedBrush      = ApplicationState->BrushKey*float32
type AnimatedMatrix     = ApplicationState->Matrix3x2

type ThickessUnit = float32

[<StructuralEquality>]
[<StructuralComparison>]
type Thickness =
    {
        Left    : ThickessUnit
        Top     : ThickessUnit
        Right   : ThickessUnit
        Bottom  : ThickessUnit
    }

    static member New left top right bottom     = {Left = Clamp left; Top = Clamp top; Right = Clamp right; Bottom = Clamp bottom}
    static member Zero                          = Thickness.New 0.F 0.F 0.F 0.F
    static member Uniform w                     = Thickness.New w w w w

    member x.IsZero with get () = x = Thickness.Zero

    static member ( + ) (l : Thickness, r : Thickness) =
                        Thickness.New
                            (l.Left      + r.Left   )
                            (l.Top       + r.Top    )
                            (l.Right     + r.Right  )
                            (l.Bottom    + r.Bottom )

    static member ( ~- ) (t : Thickness) =
                        Thickness.New
                            -t.Left
                            -t.Top
                            -t.Right
                            -t.Bottom

[<StructuralEquality>]
[<StructuralComparison>]
type MeasurementUnit =
    | FixedMeasurement  of float32
    | Fill

    static member Zero = FixedMeasurement 0.F

    static member ( + ) (l : MeasurementUnit, r : float32) =
        match l with
        | FixedMeasurement  v   -> MeasurementUnit.Clamp <| FixedMeasurement (v + r)
        | Fill                  -> Fill
    static member ( - ) (l : MeasurementUnit, r : float32) = l + (-r)

    static member ( + ) (l : MeasurementUnit, r : MeasurementUnit) =
        match l,r with
        | FixedMeasurement l, FixedMeasurement r    -> MeasurementUnit.Clamp <| FixedMeasurement (l + r)
        | _                 , _                     -> Fill

    static member ( - ) (l : MeasurementUnit, r : MeasurementUnit) =
        match l,r with
        | FixedMeasurement l, FixedMeasurement r    -> MeasurementUnit.Clamp <| FixedMeasurement (l - r)
        | FixedMeasurement _, Fill                  -> MeasurementUnit.Zero
        | _                 , _                     -> Fill

    member x.Union (o : MeasurementUnit) =
        match x,o with
        | Fill              , _                     -> Fill
        | _                 , Fill                  -> Fill
        | FixedMeasurement l, FixedMeasurement r    -> MeasurementUnit.Clamp <| FixedMeasurement (max l r)

    static member FromFloat32 (v : float32) =
        match v with
        | IsPositiveInfinity    -> Fill
        | IsPositive            -> FixedMeasurement v
        | _                     -> FixedMeasurement 0.F

    static member Clamp (x : MeasurementUnit) =
            match x with
            | Fill              -> Fill
            | FixedMeasurement m->
                match m with
                | IsPositiveInfinity    -> Fill
                | IsPositive            -> FixedMeasurement m
                | _                     -> FixedMeasurement 0.F


[<StructuralEquality>]
[<StructuralComparison>]
type Measurement =
    {
        Width   : MeasurementUnit
        Height  : MeasurementUnit
    }
    static member New (width : MeasurementUnit) (height : MeasurementUnit) = {Width = MeasurementUnit.Clamp width; Height = MeasurementUnit.Clamp height}
    static member Zero = Measurement.New MeasurementUnit.Zero MeasurementUnit.Zero
    static member Fill = Measurement.New Fill Fill

    static member FromSize2 (sz : Size2F) = Measurement.New (MeasurementUnit.FromFloat32 sz.Width) (MeasurementUnit.FromFloat32 sz.Height)

    static member ( + ) (l : Measurement, r : Thickness) =
                        Measurement.New
                            (l.Width    + (r.Left + r.Right ))
                            (l.Height   + (r.Top  + r.Bottom))

    static member ( - ) (l : Measurement, r : Thickness) =
                        Measurement.New
                            (l.Width    - (r.Left + r.Right ))
                            (l.Height   - (r.Top  + r.Bottom))

    member x.Union (o : Measurement) =
        Measurement.New
            (x.Width.Union  o.Width )
            (x.Height.Union o.Height)


[<StructuralEquality>]
[<StructuralComparison>]
type AvailableUnit =
    | Unbound
    | Bound     of float32

    static member Zero = Bound 0.F

    static member ( + ) (l : AvailableUnit, r : float32) =
        match l with
        | Unbound   -> Unbound
        | Bound v   -> AvailableUnit.Clamp <| Bound (v + r)
    static member ( - ) (l : AvailableUnit, r : float32) = l + (-r)

    static member ( + ) (l : AvailableUnit, r : MeasurementUnit) =
        match l,r with
        | Bound l, FixedMeasurement r -> AvailableUnit.Clamp <| Bound (l + r)
        | _      , _                  -> Unbound

    static member ( - ) (l : AvailableUnit, r : MeasurementUnit) =
        match l,r with
        | Bound l, FixedMeasurement r -> AvailableUnit.Clamp <| Bound (l - r)
        | Bound _, Fill               -> AvailableUnit.Zero
        | _      , _                  -> Unbound

    member x.Max (o : AvailableUnit) =
        match x,o with
        | Bound xx  , Bound yy  -> AvailableUnit.Clamp <| Bound (max xx yy)
        | _         , _         -> Unbound

    member x.IsMeasurementValid (measurement : MeasurementUnit) =
            match x,measurement with
            | Unbound   , _                 -> true
            | Bound _   , Fill              -> true
            | Bound b   , FixedMeasurement v-> b >= v

    member x.ToFloat32 =
            match x with
            | Unbound   -> Single.PositiveInfinity
            | Bound b   -> b

    static member Clamp (x : AvailableUnit) =
            match x with
            | Unbound       -> Unbound
            | Bound m       ->
                match m with
                | IsPositiveInfinity    -> Unbound
                | IsPositive            -> Bound m
                | _                     -> Bound 0.F

[<StructuralEquality>]
[<StructuralComparison>]
type Available =
    {
        Width   : AvailableUnit
        Height  : AvailableUnit
    }
    static member New (width : AvailableUnit) (height : AvailableUnit) = {Width = AvailableUnit.Clamp width; Height = AvailableUnit.Clamp height}
    static member Unbound = Available.New AvailableUnit.Unbound AvailableUnit.Unbound
    static member Zero = Available.New AvailableUnit.Zero AvailableUnit.Zero

    member x.IsZero with get ()     = x = Available.Zero

    static member ( + ) (l : Available, r : Thickness) =
                        Available.New
                            (l.Width    + (r.Left + r.Right ))
                            (l.Height   + (r.Top  + r.Bottom))

    static member ( - ) (l : Available, r : Thickness) = l + (-r)

    member x.IsMeasurementValid (m : Measurement)   = x.Width.IsMeasurementValid m.Width && x.Height.IsMeasurementValid m.Height

    member x.ToSize2F = Size2F(x.Width.ToFloat32, x.Height.ToFloat32)

type PlacementUnit = float32

[<StructuralEquality>]
[<StructuralComparison>]
type Placement =
    {
        X       : PlacementUnit
        Y       : PlacementUnit
        Width   : PlacementUnit
        Height  : PlacementUnit
    }
    static member New x y width height = {X = x; Y = y; Width = Clamp width; Height = Clamp height}
    static member Zero = Placement.New 0.F 0.F 0.F 0.F

    member x.IsZero with get ()     = x = Placement.Zero


    member x.ToRectangleF = RectangleF(x.X, x.Y, x.Width, x.Height)

    static member ( + ) (l : Placement, r : Thickness) =
                        Placement.New
                            (l.X - r.Left                               )
                            (l.Y - r.Top                                )
                            (Clamp <| l.Width     + r.Left+ r.Right   )
                            (Clamp <| l.Height    + r.Top + r.Bottom  )

    static member ( - ) (l : Placement, r : Thickness) =
                        Placement.New
                            (l.X + r.Left                               )
                            (l.Y + r.Top                                )
                            (Clamp <| l.Width     - r.Left- r.Right   )
                            (Clamp <| l.Height    - r.Top - r.Bottom  )


[<StructuralEquality>]
[<StructuralComparison>]
type PositionUnit =
    | MinPos
    | CenterPos
    | MaxPos
    static member Clamp (x : PositionUnit) = x

[<StructuralEquality>]
[<StructuralComparison>]
type SizeUnit =
    | MinSize
    | MaxSize
    | FixedSize of float32
    static member Clamp (x : SizeUnit) =
            match x with
            | MinSize       -> MinSize
            | MaxSize       -> MaxSize
            | FixedSize m   ->
                match m with
                | IsPositiveInfinity    -> MaxSize
                | IsPositive            -> FixedSize m
                | _                     -> FixedSize 0.F

[<StructuralEquality>]
[<StructuralComparison>]
type BoundingBox =
    {
        X       : PositionUnit
        Y       : PositionUnit
        Width   : SizeUnit
        Height  : SizeUnit
    }
    static member New (x : PositionUnit) (y : PositionUnit) (width : SizeUnit) (height : SizeUnit) = {X = PositionUnit.Clamp x; Y = PositionUnit.Clamp y; Width = SizeUnit.Clamp width; Height = SizeUnit.Clamp height}
    static member MinMin    = BoundingBox.New MinPos MinPos MinSize MinSize
    static member MinMax    = BoundingBox.New MinPos MinPos MaxSize MaxSize
    static member CenterMin = BoundingBox.New CenterPos CenterPos MinSize MinSize
    static member CenterMax = BoundingBox.New CenterPos CenterPos MaxSize MaxSize
    static member MaxMin    = BoundingBox.New MaxPos MaxPos MinSize MinSize
    static member MaxMax    = BoundingBox.New MaxPos MaxPos MaxSize MaxSize

    member x.AdjustMeasurement (a : Available) (m : Measurement) =
                let ww = x.AdjustMeasurementUnit a.Width  x.X x.Width  m.Width
                let hh = x.AdjustMeasurementUnit a.Height x.Y x.Height m.Height
                Measurement.New ww hh

    member private x.AdjustMeasurementUnit asize bpos bsize msize =
                        match asize,bsize,msize with
                        | Unbound   , MinSize       , FixedMeasurement _ -> msize
                        | Bound b   , MinSize       , FixedMeasurement m -> FixedMeasurement <| min b m
                        | Unbound   , FixedSize s   , _                  -> FixedMeasurement s
                        | Bound b   , FixedSize s   , _                  -> FixedMeasurement <| min b s
                        | _         , _             , _                  -> Fill

    member x.AdjustPlacement (m : Measurement) (p : Placement) =
                let xx,ww = x.AdjustPlacementUnit m.Width  x.X x.Width  p.X p.Width
                let yy,hh = x.AdjustPlacementUnit m.Height x.Y x.Height p.Y p.Height

                Placement.New xx yy ww hh

    member private x.AdjustPlacementUnit msize bpos bsize ppos psize =
                        match msize,bpos,bsize with
                        | FixedMeasurement m, MinPos        , MinSize    -> ppos,min m psize
                        | FixedMeasurement m, MaxPos        , MinSize    -> let size = min m psize
                                                                            ppos + psize - size,size
                        | FixedMeasurement m, CenterPos     , MinSize    -> let size = min m psize
                                                                            ppos + (psize - size) / 2.0F,size
                        | _                 , MinPos        , FixedSize s-> ppos,min s psize
                        | _                 , MaxPos        , FixedSize s-> let size = min s psize
                                                                            ppos + psize - size,size
                        | _                 , CenterPos     , FixedSize s-> let size = min s psize
                                                                            ppos + (psize - size) / 2.0F,size
                        | _                 , _             , _          -> ppos,psize


type BlockingQueue<'T>() =
    let safe    = obj()
    let queue   = Queue<'T>()

    member x.Enqueue (v : 'T) =
        Monitor.Enter safe
        try
            queue.Enqueue v
            Monitor.Pulse safe
        finally
            Monitor.Exit safe

    member x.EnqueueValues (vs : 'T array) =
        Monitor.Enter safe
        try
            for v in vs do
                queue.Enqueue v
            Monitor.Pulse safe
        finally
            Monitor.Exit safe

    member x.TryDequeue (timeOut : int) (ct : CancellationToken) : 'T array =

        let now = CurrentTimeInMs ()
        let waitUntil = now + (max 0L <| int64 timeOut)

        Monitor.Enter safe
        try
            let mutable result = [||]
            let mutable cont = true
            while cont do
                if queue.Count > 0 then
                    result <-   [|
                                    while queue.Count > 0 do
                                        yield queue.Dequeue ()
                                |]
                    cont <- false
                else
                    let waitFor = int32 <| waitUntil - CurrentTimeInMs ()
                    if waitFor > 0 then
                        cont <- Monitor.Wait(safe,waitFor)
                    else
                        cont <- false


            result
        finally
            Monitor.Exit safe


    member x.AsyncDequeue (timeOut : int) : Async<'T array> =
        let dequeue ct =    Async.FromContinuations <| fun (cont, econt, ccont) ->
                                try
                                    let d = x.TryDequeue timeOut ct
                                    if not ct.IsCancellationRequested then
                                        cont d
                                    else
                                        ccont <| OperationCanceledException ()
                                with
                                    | e -> econt e
        async.Bind(Async.CancellationToken,dequeue)

[<AutoOpen>]
module FundamentalAutoOpen =

    let InvalidId                           = 0
    let CurrentTime () : Time               = (float32 GlobalClock.ElapsedMilliseconds) / 1000.F

    let internal AsTransparentBrush         = BrushDescriptor.Transparent
    let internal AsSolidBrush (c : Color)   = BrushDescriptor.SolidColor <| ColorDescriptor.Color c

    let AsVector ((x,y) : Point)            = Vector2 (x,y)

    let ( .*. ) (l : AnimatedMatrix) (r : AnimatedMatrix) : AnimatedMatrix =
        fun time -> let left    = l time
                    let right   = r time
                    Matrix3x2.Multiply (left, right)

    type MouseButtonStates with

        member x.Union      (o : MouseButtonStates) = x ||| o
        member x.Intersect  (o : MouseButtonStates) = x &&& o
        member x.Difference (o : MouseButtonStates) = x &&& ~~~o

    type String with

        member x.ToColorDescriptor () =
            let a,r,g,b = ParseColor x
            if a >= 0.F && r >= 0.F && g >= 0.F && b >= 0.F then
                ColorDescriptor.ARGB a r g b
            else
                ColorDescriptor.Fault
