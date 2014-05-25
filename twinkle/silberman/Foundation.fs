namespace silberman
open silberman.Internal
open silberman.Visual

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Diagnostics
open System.Threading

open SharpDX

[<StructuralEquality>]
[<StructuralComparison>]
type LayoutRotation =
    | D0
    | D90
    | D180
    | D270

[<StructuralEquality>]
[<StructuralComparison>]
type LayoutTransform =
    {
        Rotation : LayoutRotation
        Scaling  : float32
    }

type StackOrientation =
    | FromLeft
    | FromRight
    | FromTop
    | FromBottom

module Foundation =
    [<ReferenceEquality>]
    type ElementContext =
        {
            CreateBrush                 : BrushDescriptor       -> BrushKey
            CreateTextFormat            : TextFormatDescriptor  -> TextFormatKey
            CreateGeometry              : GeometryDescriptor    -> GeometryKey
            CreateTransformedGeometry   : GeometryKey           -> Matrix3x2 -> TransformedGeometryKey
            MeasureText                 : TextFormatKey         -> Size2F -> string -> Size2F
        }
        static member New cb ctf cg ctg mt =
                        {
                            CreateBrush                 = cb
                            CreateTextFormat            = ctf
                            CreateGeometry              = cg
                            CreateTransformedGeometry   = ctg
                            MeasureText                 = mt
                        }

    type PropertyType =
            | Computed
            | Persistent
            | Empty

    [<NoEquality>]
    [<NoComparison>]
    type PropertyDefaultValue<'T> =
        | Value         of 'T
        | ValueCreator  of (Element -> 'T)
    and PropertyValueChanged<'T>    = Element -> 'T -> 'T -> unit
    and ComputePropertyValue<'T>    = Element -> 'T
    and EventHandler<'TEventValue>   = Element -> 'TEventValue -> bool
    and [<AbstractClass>] Member(id : string, declaringType : Type) =
        inherit obj()


        static let globalId = ref 1000
        static let CreateInternalId () : int =
                        let id = Interlocked.Increment globalId
                        id

        let internalId = CreateInternalId()

        interface IEquatable<Member> with
            member x.Equals o = internalId = o.InternalId

        member x.InternalId     = internalId

        member x.Id             = id
        member x.DeclaringType  = declaringType

        member x.IsMemberOf (t : Type) = declaringType.IsAssignableFrom t

        member x.ValidateMember (t : Type) =
            if not <| x.IsMemberOf t then
                failwithf "%s %s.%s is not a member %s" (x.GetType().Name) x.DeclaringType.Name x.Id t.Name

        override x.ToString () = sprintf "%s:%s.%s (%d)" (x.GetType().Name) x.DeclaringType.Name x.Id internalId

        override x.Equals o =
                    match o with
                    | :? Member as m    -> internalId = m.InternalId
                    | _                 -> false

        override  x.GetHashCode () = internalId.GetHashCode ()

    and [<AbstractClass>] Event(id : string, eventValueType : Type, declaringType : Type) =
        inherit Member(id, declaringType)

        member x.EventValueType = eventValueType

        static member Routed<'TDeclaring, 'TEventValue> id  (sample : 'TEventValue) = Event<'TEventValue>(id,typeof<'TDeclaring>)
        static member Empty = Event.Routed<Event, obj> "<EMPTY>"

    and [<Sealed>] Event<'TEventValue>(id : string, declaringType : Type) =
        inherit Event(id, typeof<'TEventValue>, declaringType)

        member x.Handler (eh : EventHandler<'TEventValue>) = EventListener<'TEventValue>(x, eh)

    and [<AbstractClass>] EventListener(e : Event) =

        abstract OnSetListener  : Element -> unit
        abstract OnClearListener: Element -> unit

        member x.SetListener (ee : Element) =
                    if e.IsMemberOf <| ee.GetType () then
                        x.OnSetListener ee
                        true
                    else false

        member x.ClearListener (ee : Element) =
                    if e.IsMemberOf <| ee.GetType () then
                        x.OnClearListener ee
                        true
                    else false

        member x.Event  = e

    and [<AbstractClass>] Property(id : string, ``type`` : Type, declaringType : Type) =
        inherit Member(id, declaringType)

        static let __NoAction              (le : Element) (ov : 'T) (nv : 'T) = le.NoAction                ()

        static let empty        = EmptyProperty()

        member x.Type           = ``type``
        member x.IsEmpty        = x.Equals (empty)

        abstract OnPropertyType : unit -> PropertyType
        member x.PropertyType   = x.OnPropertyType ()

        static member Value v = fun () -> v

        static member Persistent<'TDeclaring, 'T when 'T : equality> id valueChanged valueCreator = PersistentProperty<'T>(id,typeof<'TDeclaring>,valueCreator,valueChanged)
        static member Computed<'TDeclaring, 'T>   id computeValue = ComputedProperty<'T>(id,typeof<'TDeclaring>,computeValue)

        static member Empty = empty

    and [<Sealed>] EmptyProperty() =
        inherit Property("<EMPTY>", typeof<obj>, typeof<Property>)

        override x.OnPropertyType ()    = Empty

    and [<AbstractClass>] Property<'T>(id : string, declaringType : Type) =
        inherit Property(id, typeof<'T>, declaringType)

    and [<Sealed>] PersistentProperty<'T when 'T : equality>(id : string, declaringType : Type, defaultValue : PropertyDefaultValue<'T>, valueChanged : PropertyValueChanged<'T>)=
        inherit Property<'T>(id, declaringType)

        static let overrideDefaultValue = TypeDictionary<PropertyDefaultValue<'T>>()
        static let overrideValueChanged = TypeDictionary<PropertyValueChanged<'T>>()

        override x.OnPropertyType ()    = Computed

        member x.DefaultValue (e : Element)             =
                    let dv = (overrideDefaultValue.TryFind <| e.GetType()) <??> defaultValue
                    match dv with
                    | Value         v -> v      , true
                    | ValueCreator  vc-> vc e   , false

        member x.ValueChanged e oldValue newValue      =
                    let vc = (overrideValueChanged.TryFind <| e.GetType()) <??> valueChanged
                    vc e oldValue newValue


        member x.Value (v : 'T) = PropertyValue<'T>(x, v)

        member x.Override<'TOverride> (defaultValue : PropertyDefaultValue<'T> option) (valueChanged : PropertyValueChanged<'T> option) =
                    let overrideType = typeof<'TOverride>
                    x.ValidateMember overrideType
                    match defaultValue with
                    | Some defaultValue -> overrideDefaultValue.Replace overrideType defaultValue
                    | None              -> ()
                    match valueChanged with
                    | Some valueChanged -> overrideValueChanged.Replace overrideType valueChanged
                    | None              -> ()
                    ()

    and PersistentPropertyCreator<'T when 'T : equality> = string -> PropertyValueChanged<'T> -> PropertyDefaultValue<'T> -> PersistentProperty<'T>

    and [<AbstractClass>] PropertyValue(p : Property) =

        abstract OnAssignValueTo : Element -> bool

        member x.AssignValueTo (e : Element) =
                    if p.IsMemberOf <| e.GetType () then
                        x.OnAssignValueTo e
                    else false

        member x.Property   = p

    and PropertyValue<'T when 'T : equality>(p : PersistentProperty<'T>, v : 'T)=
        inherit PropertyValue(p)

        member x.Value      = v

        override x.OnAssignValueTo (e : Element) =
                    e.Set p v
                    true
    and [<Sealed>] ComputedProperty<'T>(id : string, declaringType : Type, computeValue : ComputePropertyValue<'T>) =
        inherit Property<'T>(id, declaringType)

        static let overrideCompute  = TypeDictionary<ComputePropertyValue<'T>>()

        override x.OnPropertyType ()= Persistent

        member x.ComputeValue (e : Element) =
                    let cv = (overrideCompute.TryFind <| e.GetType()) <??> computeValue
                    cv e

        member x.Override<'TOverride> (computeValue : ComputePropertyValue<'T> option) =
                    let overrideType = typeof<'TOverride>
                    x.ValidateMember overrideType
                    match computeValue with
                    | Some computeValue -> overrideCompute.Replace overrideType computeValue
                    | None              -> ()
                    ()

    and [<AbstractClass>] PropertyBag() as this =

        let element : Element   = downcast this
        let properties          = Dictionary<Property, obj>()
        let eventHandlers       = Dictionary<Event, obj>()

        let mutable parent : Element option = None

        member x.Parent
            with get ()         = parent

        member internal x.SetParentNoTrigger p = parent <- p

        member x.Root
            with get ()         =
                    match parent with
                    | None        -> x
                    | Some parent -> parent.Root

        member private x.ValidateProperty (lp :Property<'T>) =
            lp.ValidateMember <| x.GetType()

        member private x.ValidateEvent (e :Event<'T>) =
            e.ValidateMember <| x.GetType()

        member private x.TryGet (lp :Property<'T>)  : 'T option =
                let v = properties.Find lp
                match v with
                | None      -> None
                | Some v    ->
                    let tv = v.As<'T> ()
                    match tv with
                    | None      -> Debug.Assert false; None
                    | Some tv   -> Some tv


        member x.Get    (lp : ComputedProperty<'T>)  : 'T =
                x.ValidateProperty lp
                lp.ComputeValue element

        member x.Get<'T when 'T : equality> (lp : PersistentProperty<'T>)  : 'T =
                x.ValidateProperty lp
                let v = x.TryGet lp
                match v with
                | Some v    -> v
                | None      ->
                    ignore <| properties.Remove lp  // Shouldn't be necessary but if the TryGet assert fails this is required to clear local value
                    let dv,shared = lp.DefaultValue element
                    if not shared then
                        properties.Add(lp,dv)
                    dv  // No ValueChanged on initializing the default value

        member x.Get    (lp : Property<'T>)           : 'T =
                match lp.PropertyType with
                | Computed      -> x.Get (lp :?> ComputedProperty<'T>)
                | Persistent    -> x.Get (lp :?> PersistentProperty<'T>)
                | Empty         -> DefaultOf<_>

        member x.Set<'T when 'T : equality> (lp : PersistentProperty<'T>) (v : 'T)  : unit =
                x.ValidateProperty lp
                let pv = x.Get lp
                if pv = v then ()
                else
                    properties.[lp] <- v
                    lp.ValueChanged element pv v

        member x.Clear  (lp : PersistentProperty<'T>)           : unit =
                x.ValidateProperty lp
                let v = x.TryGet lp
                ignore <| properties.Remove lp  // Shouldn't be necessary but if the TryGet assert fails this is required to clear local value
                match v with
                | None      -> ()
                | Some v   ->
                    // Property value found, reset to default value and raise ValueChanged
                    let dv,shared = lp.DefaultValue element
                    if not shared then
                        properties.Add(lp,dv)
                    lp.ValueChanged element v dv

        member x.AssignFromPropertyValues (pvs : PropertyValue list) =
            for pv in pvs do
                ignore <| pv.AssignValueTo element

        member internal x.RaiseEventImpl (e : Event<'TEventValue>) (v : 'TEventValue) =
                x.ValidateEvent e
                let event = eventHandlers.Find e
                match event,parent with
                | None      , None          ->  false
                | None      , Some parent   ->  parent.RaiseEventImpl e v
                | Some eh   , None          ->  let eh : EventHandler<'TEventValue> = downcast eh
                                                eh element v
                | Some eh   , Some parent   ->  let eh : EventHandler<'TEventValue> = downcast eh
                                                let handled = eh element v
                                                if handled then true
                                                else parent.RaiseEventImpl e v

        member x.RaiseEvent (e : Event<'TEventValue>) (v : 'TEventValue) =
                x.ValidateEvent e
                x.RaiseEventImpl e v

        member x.ClearEventHandler (e : Event<'TEventValue>) =
                ignore <| eventHandlers.Remove e

        member x.SetEventHandler (e : Event<'TEventValue>) (eh : EventHandler<'TEventValue>) =
                eventHandlers.[e] <- eh

    and [<AbstractClass>] Element() =

        inherit PropertyBag()

        static let children = [||]

        static let __NoAction                   (le : Element) (ov : 'T) (nv : 'T) = le.NoAction                ()
        static let __InvalidateMeasurement      (le : Element) (ov : 'T) (nv : 'T) = le.InvalidateMeasurement   ()
        static let __InvalidatePlacement        (le : Element) (ov : 'T) (nv : 'T) = le.InvalidatePlacement     ()
        static let __InvalidateVisual           (le : Element) (ov : 'T) (nv : 'T) = le.InvalidateVisual        ()
        static let __InvalidateTextFormatKey    (le : Element) (ov : 'T) (nv : 'T) = le.InvalidateTextFormatKey ()
        static let __InvalidateBrushKey      bk (le : Element) (ov : 'T) (nv : 'T) = le.InvalidateBrushKey bk

        static let brushCreator
            (brushCreator       : PersistentPropertyCreator<BrushDescriptor>)
            (keyCreator         : PersistentPropertyCreator<BrushKey>       )
            (id                 : string                                    )
            (brushDescriptor    : BrushDescriptor                           ) =
            let rec brush   = brushCreator id         (__InvalidateBrushKey brushKey)   <| Value brushDescriptor
            and brushKey    = keyCreator (id+"Key")   __InvalidateVisual
                                <| ValueCreator
                                    (fun e ->
                                        let context         = e.Context
                                        let brushDescriptor = e.Get brush
                                        match context with
                                        | Some c    -> c.CreateBrush brushDescriptor
                                        | _         -> InvalidId
                                    )
            brush, brushKey

        static let Persistent id valueChanged value = Property.Persistent<Element, _>   id valueChanged value
        static let Computed   id computeValue       = Property.Computed<Element, _>     id computeValue
        static let Routed     id sample             = Event.Routed<Element, _>          id sample
        static let Brush                            = brushCreator Persistent Persistent

        static let elementContext               = Persistent "ElementContext"  __NoAction               <| Value (None : ElementContext option)

        static let measurement                  = Persistent "Measurement"     __NoAction               <| Value (None : Measurement option)
        static let placement                    = Persistent "Placement"       __NoAction               <| Value (None : Placement option)
        static let visual                       = Persistent "Visual"          __NoAction               <| Value (None : VisualTree option)

        static let bounds                       = Persistent "Bounds"          __InvalidateMeasurement  <| Value Bounds.MinMin
        static let isVisible                    = Persistent "IsVisible"       __InvalidateMeasurement  <| Value true

        static let margin                       = Persistent "Margin"          __InvalidateMeasurement  <| Value Thickness.Zero

        static let fontFamily                   = Persistent "FontFamily"      __InvalidateTextFormatKey<| Value "Calibri"
        static let fontSize                     = Persistent "FontSize"        __InvalidateTextFormatKey<| Value 24.F
        static let textFormatKey                = Persistent "TextFormatKey"   __InvalidateMeasurement  <| ValueCreator
                                                                                                            (fun e ->
                                                                                                                let context     = e.Context
                                                                                                                let fontFamily  = e.Get fontFamily
                                                                                                                let fontSize    = e.Get fontSize
                                                                                                                match context with
                                                                                                                | Some c    -> c.CreateTextFormat <| TextFormatDescriptor.New fontFamily fontSize
                                                                                                                | _         -> InvalidId
                                                                                                            )

        static let background, backgroundKey    = Brush "Background" BrushDescriptor.Transparent
        static let foreground, foregroundKey    = Brush "Foreground" <| AsSolidBrush Color.Black


        static let attached                     = Routed        "Attached"      ()
        static let detached                     = Routed        "Detached"      ()

        abstract OnChildren     : unit -> Element array

        abstract OnMeasureContent                   : Available -> Measurement

        abstract OnGetEffectiveMargin               : unit -> Thickness

        abstract OnPlaceElement                     : Placement -> Placement -> unit

        abstract OnRenderContent                    : Placement -> Placement -> VisualTree
        abstract OnRenderOverlay                    : Placement -> Placement -> VisualTree

        abstract OnRenderChild                      : Placement -> Placement -> Element -> VisualTree

        member x.Children       = x.OnChildren ()

        member x.Context        =
                    let root = x.Root
                    root.Get elementContext

        member internal x.SetParent p =
                            let parent = x.Parent
                            match parent with
                            | None      -> ()
                            | Some pp   -> failwith "Element is already a member of a logical tree"
                            x.SetParentNoTrigger <| Some p
                            match parent with
                            | None      -> ()
                            | Some pp   -> pp.InvalidateMeasurement ()
                            ignore <| x.RaiseEvent attached ()

        member internal x.ClearParent () =
                            let parent = x.Parent
                            match parent with
                            | None      -> ()
                            | Some pp   -> pp.InvalidateMeasurement ()
                            x.SetParentNoTrigger None
                            ignore <| x.RaiseEvent detached ()

        static member BrushCreator          = brushCreator

        static member ElementContext        = elementContext

        static member Measurement           = measurement
        static member Placement             = placement
        static member Visual                = visual

        static member Bounds                = bounds
        static member IsVisible             = isVisible

        static member Margin                = margin

        static member FontFamily            = fontFamily
        static member FontSize              = fontSize

        static member Background            = background
        static member BackgroundKey         = backgroundKey

        static member Foreground            = foreground
        static member ForegroundKey         = foregroundKey

        static member TextFormatKey         = textFormatKey

        static member Attached              = attached
        static member Detached              = detached

        default x.OnChildren () = children
        default x.OnMeasureContent m                = Measurement.Fill
        default x.OnGetEffectiveMargin ()           = x.Get Element.Margin
        default x.OnPlaceElement    (o : Placement)
                                    (i : Placement)
                                                    = ()
        default x.OnRenderContent   (o : Placement)
                                    (i : Placement)
                                                    = VisualTree.NoVisual
        default x.OnRenderOverlay   (o : Placement)
                                    (i : Placement)
                                                    = VisualTree.NoVisual
        default x.OnRenderChild     (o : Placement)
                                    (i : Placement)
                                    (e : Element)
                                                    = e.Render ()

        member x.NoAction               () = ()
        member x.InvalidateMeasurement  () =
            let m = x.Get Element.Measurement
            match m with
            | None      ->  ()
            | Some _    ->  x.Clear Element.Measurement
                            x.Clear Element.Placement
                            x.Clear Element.Visual
                            match x.Parent with
                            | Some p -> p.InvalidateMeasurement ()
                            | None   -> ()
        member x.InvalidatePlacement    () =
            let p = x.Get Element.Placement
            match p with
            | None      ->  ()
            | Some _    ->  x.Clear Element.Placement
                            x.Clear Element.Visual
                            match x.Parent with
                            | Some p -> p.InvalidatePlacement ()
                            | None   -> ()
        member x.InvalidateVisual       () =
            let v = x.Get Element.Visual
            match v with
            | None      ->  ()
            | Some _    ->  x.Clear Element.Visual
                            match x.Parent with
                            | Some p -> p.InvalidateVisual ()
                            | None   -> ()

        member x.InvalidateTextFormatKey () =
            x.Clear textFormatKey

        member x.InvalidateBrushKey (bk : PersistentProperty<BrushKey>) =
            x.Clear bk

        member x.EffectiveMargin                    = x.OnGetEffectiveMargin ()

        member x.MeasureElement (a  : Available)    =
                    let cachedMeasure = x.Get Element.Measurement
                    match cachedMeasure with
                    | Some m when a.IsMeasurementValid m  -> m
                    | _                                   ->
                        let box = x.EffectiveMargin
                        let bounds = x.Get Element.Bounds
                        let innerMeasure = x.OnMeasureContent<| a - box
                        let finalMeasure = bounds.AdjustMeasurement a (innerMeasure + box)
                        x.Set Element.Measurement <| Some finalMeasure
                        x.Set Element.Placement None
                        x.Set Element.Visual None
                        finalMeasure

        member x.PlaceElement   (p : Placement)     =
                    let cachedPlacement = x.Get Element.Placement
                    match cachedPlacement with
                    | Some cp when cp = p -> ()
                    | _                   ->
                        let cachedMeasure = x.Get Element.Measurement
                        match cachedMeasure with
                        | None            -> ()
                        | Some cm         ->
                            let box = x.EffectiveMargin
                            let bounds = x.Get Element.Bounds
                            let finalPlacement = bounds.AdjustPlacement cm p
                            x.OnPlaceElement finalPlacement <| finalPlacement - box
                            x.Set Element.Placement <| Some finalPlacement
                            x.Set Element.Visual None


        member x.Render ()                          =
                    let cachedVisual = x.Get Element.Visual
                    match cachedVisual with
                    | Some v    -> v
                    | None      ->
                        let box = x.EffectiveMargin
                        let p = x.Get Element.Placement
                        match p with
                        | None                      -> NoVisual
                        | Some p when p.IsZero      -> NoVisual
                        | Some outer                ->

                            let inner = outer - box

                            let visualContent = x.OnRenderContent outer inner

                            // A bit of trickery to avoid allocation and shuffling of extra arrays
                            let children = x.Children
                            let visualChildren = Array.create (children.Length + 2) VisualTree.NoVisual

                            for i in 0..children.Length-1 do
                                visualChildren.[i + 1] <- x.OnRenderChild outer inner children.[i]

                            let visualOverlay = x.OnRenderOverlay outer inner

                            visualChildren.[0] <- visualContent
                            visualChildren.[visualChildren.Length - 1] <- visualOverlay

                            let visual =
                                match visualContent, children.Length, visualOverlay with
                                | VisualTree.NoVisual , 0     , VisualTree.NoVisual   -> VisualTree.NoVisual
                                | _                   , 0     , VisualTree.NoVisual   -> visualContent
                                | VisualTree.NoVisual , 0     , _                     -> visualOverlay
                                | VisualTree.NoVisual , 1     , VisualTree.NoVisual   -> visualChildren.[1] // The first visual child is located @ 1
                                | _                   , _     , _                     -> VisualTree.Group visualChildren

                            x.Set Element.Visual <| Some visual
                            visual

    // Has to be below Element class, F# compiler gets confused otherwise
    and EventListener<'TEventValue>(e : Event<'TEventValue>, handler : EventHandler<'TEventValue>)=
        inherit EventListener(e)

        member x.Handler : EventHandler<'TEventValue> = handler

        override x.OnSetListener (ee : Element) =
                    ee.SetEventHandler e handler
                    ()

        override x.OnClearListener (ee : Element) =
                    ee.ClearEventHandler e
                    ()

