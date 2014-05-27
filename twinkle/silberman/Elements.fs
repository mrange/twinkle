namespace silberman
open silberman.Internal
open silberman.Visual

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Diagnostics
open System.Threading

open SharpDX

open Foundation

module public Elements =

    module Internal =

        let NoAction                (e : Element) (ov : 'T) (nv : 'T) = e.NoAction                ()
        let InvalidateMeasurement   (e : Element) (ov : 'T) (nv : 'T) = e.InvalidateMeasurement   ()
        let InvalidatePlacement     (e : Element) (ov : 'T) (nv : 'T) = e.InvalidatePlacement     ()
        let InvalidateVisual        (e : Element) (ov : 'T) (nv : 'T) = e.InvalidateVisual        ()
        let InvalidateTextFormatKey (e : Element) (ov : 'T) (nv : 'T) = e.InvalidateTextFormatKey ()
        let InvalidateBrushKey bk   (e : Element) (ov : 'T) (nv : 'T) = e.InvalidateBrushKey bk

        // type Element = Logical.Foundation.Element

        type [<AbstractClass>] ContainerElement() =
            inherit Element()

            static let Persistent id valueChanged value = Property.Persistent<ContainerElement, _>  id valueChanged value

            static let padding                  = Persistent "Padding"         InvalidateMeasurement    <| Value Thickness.Zero

            static member Padding               = padding

            override x.OnGetEffectiveMargin ()  = x.Get Element.Margin + x.Get ContainerElement.Padding

        type [<AbstractClass>] DecoratorElement() =
            inherit ContainerElement()

            static let InvalidateChild (e : Element) (ov : Element option) (nv : Element option) =
                        match ov with
                        | None          -> ()
                        | Some child    -> child.ClearParent () // Invalidates old parent

                        match nv with
                        | None          -> ()
                        | Some child    -> child.SetParent e    // Invalidates parent

            static let Persistent id valueChanged value = Property.Persistent<DecoratorElement, _>  id valueChanged value

            static let child        = Persistent     "Child"        InvalidateChild <| Value (None : Element option)

            let mutable cachedChildren : Element array option = None

            static member Child     = child

            override x.OnChildren () =
                        let child = x.Get DecoratorElement.Child
                        match cachedChildren, child with
                        | Some c    , _         -> c
                        | None      , Some c    ->
                            let children = [|c|]
                            cachedChildren <- Some children
                            children
                        | None      , None      ->
                            let children = [||]
                            cachedChildren <- Some children
                            children

            override x.OnMeasureContent a   =
                        let child = x.Get DecoratorElement.Child
                        match child with
                        | None      -> Measurement.Zero
                        | Some c    -> c.MeasureElement a

            override x.OnPlaceElement o i   =
                        let child = x.Get DecoratorElement.Child
                        match child with
                        | None      -> ()
                        | Some c    -> c.PlaceElement i

        type [<AbstractClass>] LayoutElement() =
            inherit ContainerElement()

            static do
                Element.Bounds.Override<LayoutElement> (Some <| Value BoundingBox.MinMax) None
                

            let children    = SortedDictionary<int, Element>()

            let mutable cachedChildren = None

            override x.OnChildren ()  = match cachedChildren with
                                        | Some c    ->  c
                                        | None      ->  let c = children |> Seq.map (fun kv -> kv.Value) |> Seq.toArray
                                                        cachedChildren <- Some c
                                                        c


            member x.SetChild i le    = children.[i] <- le
                                        le.SetParent x  // Invalidates parent
                                        cachedChildren <- None
                                        x

            member x.RemoveChild i    = let c = children.Find i
                                        match c with
                                        | None      -> ()
                                        | Some le    ->
                                            ignore <| children.Remove i
                                            le.ClearParent ()   // Invalidates old parent
                                            cachedChildren <- None
                                        x

        type DocumentElement(ctx : ElementContext) as x=
            inherit DecoratorElement()

            static do
                Element.Bounds.Override<DocumentElement> (Some <| Value BoundingBox.MinMax) None

            do
                x.Set Element.ElementContext <| Some ctx



        type StackElement() =
            inherit LayoutElement()

            let AccumulateMeasurement   (orientation : StackOrientation) (measurement : Measurement) (other : Measurement) =
                match orientation with
                | FromLeft | FromRight  -> Measurement.New (measurement.Width + other.Width) (max measurement.Height other.Height)
                | FromTop  | FromBottom -> Measurement.New (max measurement.Width other.Width) (measurement.Height + other.Height)

            let SubtractAvailable       (orientation : StackOrientation) (available : Available) (other : Measurement) =
                match orientation with
                | FromLeft | FromRight  -> Available.New (available.Width - other.Width) available.Height
                | FromTop  | FromBottom -> Available.New available.Width (available.Height - other.Height)

            let Intersect (f : float32) (m : MeasurementUnit) =
                match m with
                | FixedMeasurement m    -> Clamp <| min f m
                | Fill                  -> f

            let Subtract (f : float32) (m : MeasurementUnit) =
                match m with
                | FixedMeasurement m    -> Clamp <| f - m
                | Fill                  -> 0.F

            let AccumulateAndIntersectPlacement   (orientation : StackOrientation) (placement : Placement) (other : Measurement) =
                match orientation with
                | FromLeft  -> let intersected  = Placement.New placement.X placement.Y (Intersect placement.Width other.Width) placement.Height
                               let adjusted     = Placement.New (placement.X + intersected.Width) placement.Y (Clamp <| placement.Width - intersected.Width) placement.Height
                               adjusted,intersected
                | FromRight -> let adjusted     = Placement.New placement.X placement.Y (Subtract placement.Width other.Width) placement.Height
                               let intersected  = Placement.New (placement.X + adjusted.Width) placement.Y (Clamp <| placement.Width - adjusted.Width) placement.Height
                               adjusted,intersected
                | FromTop   -> let intersected  = Placement.New placement.X placement.Y placement.Width (Intersect placement.Height other.Height)
                               let adjusted     = Placement.New placement.X (placement.Y + intersected.Height) placement.Width (Clamp <| placement.Height - intersected.Height)
                               adjusted,intersected
                | FromBottom-> let adjusted     = Placement.New placement.X placement.Y placement.Width (Subtract placement.Height other.Height)
                               let intersected  = Placement.New placement.X (placement.Y + adjusted.Height) placement.Width (Clamp <| placement.Height - adjusted.Height)
                               adjusted,intersected

            static let Persistent id valueChanged value = Property.Persistent<StackElement, _>  id valueChanged value

            static let orientation      = Persistent "Orientation"     InvalidateMeasurement    <| Value StackOrientation.FromTop

            static member Orientation   = orientation

            override x.OnMeasureContent a   =
                        let orientation = x.Get StackElement.Orientation

                        let mutable measurement = Measurement.Zero
                        let mutable remaining   = a

                        let children = x.Children
                        for c in children do
                            let cm = c.MeasureElement a

                            measurement <- AccumulateMeasurement orientation measurement cm
//                            remaining   <- SubtractAvailable orientation remaining cm

                        measurement

            override x.OnPlaceElement o i   =
                        let orientation = x.Get StackElement.Orientation

                        let mutable placement = i

                        let children = x.Children
                        for c in children do
                            let cachedMeasurement = c.Get Element.Measurement
                            match cachedMeasurement with
                            | None      -> ()
                            | Some m    ->
                                let a,i = AccumulateAndIntersectPlacement orientation placement m
                                c.PlaceElement i
                                placement <- a

                        ()

        [<AbstractClass>]
        type TextElement() =
            inherit Element()

            static let Persistent id valueChanged value = Property.Persistent<TextElement, _> id valueChanged value

            static let text             = Persistent     "Text"        InvalidateMeasurement  <| Value ""

            static member Text          = text

        type LabelElement() =
            inherit TextElement()

            override x.OnMeasureContent a =
                        let context = x.Context
                        match context with
                        | None          -> Debug.Assert false; Measurement.Fill
                        | Some context  ->
                            let text = x.Get TextElement.Text
                            if text.Length = 0 then Measurement.Zero
                            else
                                let key     = x.Get Element.TextFormatKey
                                let size    = context.MeasureText key a.ToSize2F text
                                Measurement.FromSize2 size


            override x.OnRenderContent (o : Placement)
                                       (i : Placement) =
                            let text = x.Get TextElement.Text
                            if text = "" then VisualTree.NoVisual
                            else
                                let foregroundKey           = x.Get Element.ForegroundKey
                                let key                     = x.Get Element.TextFormatKey
                                let layoutRect = i.ToRectangleF |> Animated.Constant
                                VisualTree.Text (text, key, layoutRect, foregroundKey |> Animated.Brush.Opaque)

        type ButtonState =
            | Normal
            | Highlighted
            | Pressed

        type ButtonElement() =
            inherit DecoratorElement()

            static let Persistent   id valueChanged value   = Property.Persistent<ButtonElement, _> id valueChanged value
            static let Routed       id sample               = Event.Routed<ButtonElement, _> id sample
            static let Brush                                = Element.BrushCreator Persistent Persistent

            static let buttonState              = Persistent    "ButtonState"       InvalidateVisual        <| Value ButtonState.Normal

            static let borderThickness          = Persistent    "BorderThickness"   InvalidateMeasurement   <| Value 3.0F

            static let highlight, highlightKey  = Brush         "Highlight"         <| AsSolidBrush Color.Purple
            static let pressed, pressedKey      = Brush         "Pressed"           <| AsSolidBrush Color.LightBlue
            static let border, borderKey        = Brush         "Border"            <| AsSolidBrush Color.White

            static let clicked                  = Routed        "Clicked"          ()

            static do
                Element.Foreground.Override<ButtonElement> (Some <| Value (AsSolidBrush Color.White)) None
                Element.Background.Override<ButtonElement> (Some <| Value (SolidColor ("#555".ToColorDescriptor()))) None
                DecoratorElement.Padding.Override<ButtonElement> (Some <| Value (Thickness.New 16.F 4.F 16.F 4.F)) None

            static member ButtonState       = buttonState

            static member BorderThickness   = borderThickness

            static member Highlight         = highlight
            static member HighlightKey      = highlightKey
            static member Pressed           = pressed
            static member PressedKey        = pressedKey
            static member Border            = border
            static member BorderKey         = borderKey

            static member Clicked           = clicked

            override x.OnGetEffectiveMargin ()  = x.Get Element.Margin + (Thickness.Uniform <| x.Get ButtonElement.BorderThickness) + x.Get ContainerElement.Padding

            override x.OnRenderContent (o : Placement)
                                       (i : Placement) =
                            let rect            = (o + x.Get Element.Margin).ToRectangleF

                            let state           = x.Get ButtonElement.ButtonState
                            let borderThickness = x.Get ButtonElement.BorderThickness
                            let borderKey       = x.Get ButtonElement.BorderKey
                            let backgroundKey   = x.Get ButtonElement.BackgroundKey
                            let highlightKey    = x.Get ButtonElement.HighlightKey
                            let pressedKey      = x.Get ButtonElement.PressedKey

                            let backgroundKey =
                                match state with
                                | Normal        -> backgroundKey
                                | Highlighted   -> highlightKey
                                | Pressed       -> pressedKey

                            let background (a : ApplicationState) =
                                if rect.Contains a.CurrentMouse.Coordinate then
                                    highlightKey, 1.F
                                else
                                    backgroundKey, 1.F
                                    

                            VisualTree.Rectangle (
                                borderKey       |> Animated.Brush.Opaque,
                                background                              ,
                                rect            |> Animated.Constant    ,
                                borderThickness |> Animated.Constant
                                )

    open Internal

    module Properties =

        let Bounds          = Element.Bounds
        let IsVisible       = Element.IsVisible
        let Margin          = Element.Margin
        let FontFamily      = Element.FontFamily
        let FontSize        = Element.FontSize
        let Background      = Element.Background
        let Foreground      = Element.Foreground

        let Padding         = ContainerElement.Padding

        let Child           = DecoratorElement.Child

        let Orientation     = StackElement.Orientation

        let Text            = TextElement.Text

    module Events =

        let Attached        = Element.Attached
        let Detached        = Element.Detached
        let Clicked         = ButtonElement.Clicked

    let SomeElement (e : #Element) = Some (e :> Element)

    let ( >>+ ) (e : #Element) (el : EventListener<'T>) =
                    ignore <| el.SetListener e
                    e
    let ( >>- ) (e : #Element) (el : EventListener<'T>) =
                    ignore <| el.ClearListener e
                    e

    let CreateElement<'T when 'T :> Element and 'T : (new: unit -> 'T)> (pvs : PropertyValue list) =
        let element = new 'T()
        element.AssignFromPropertyValues pvs
        element

    let CreateContainer<'T when 'T :> LayoutElement and 'T : (new: unit -> 'T)> (pvs : PropertyValue list) (children : Element list) =
        let layout = CreateElement<'T> pvs
        let mutable i = 0
        for child in children do
            ignore <| layout.SetChild i child
            i <- i + 1
        layout

    let Label (pvs : PropertyValue list) : LabelElement  = CreateElement<LabelElement> pvs

    let Stack (pvs : PropertyValue list) (children : Element list)  = CreateContainer<StackElement> pvs children

    let Button (pvs : PropertyValue list) = CreateElement<ButtonElement> pvs

    let TextButton text (pvs : PropertyValue list) =
        let label               = Label 
                                    [
                                        Properties.Text         .Value text 
                                        Properties.Foreground   .Value      (SolidColor <| "#FFF".ToColorDescriptor ())
                                    ]
        let pv : PropertyValue  = upcast Properties.Child.Value (SomeElement <| label)
        Button <| pv::pvs