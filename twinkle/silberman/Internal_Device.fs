﻿namespace silberman.Internal

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Diagnostics
open System.Linq
open System.Threading

open SharpDX
open SharpDX.Direct2D1

open silberman

module internal Device =

    type DirectWrite () =
        let dwFactory           = new DirectWrite.Factory (DirectWrite.FactoryType.Shared)

        let textFormats         = Dictionary<TextFormatKey, DirectWrite.TextFormat>()

        member x.CreateTextFormat (tfd : TextFormatDescriptor) : DirectWrite.TextFormat =
            new DirectWrite.TextFormat(dwFactory, tfd.FontFamily, tfd.FontSize)

        member x.GetTextFormatKey (key : TextFormatKey) (tfd : TextFormatDescriptor) : TextFormatKey =
            if textFormats.ContainsKey key then key
            else
                let tf = x.CreateTextFormat tfd
                textFormats.Add(key, tf)
                key

        member x.EstimateTextSize (key : TextFormatKey) (sz : Size2F) (text : string) =
            let tf = textFormats.Find key
            match tf with
            | Some tf ->
                use tl = new DirectWrite.TextLayout(dwFactory, text, tf, sz.Width, sz.Height)
                let m = tl.Metrics
                Size2F (m.Width, m.Height)
            | _ -> Size2F ()

        interface IDisposable with
            member x.Dispose() =
                let tfc = textFormats.ToArray ()
                textFormats.Clear()
                for kv in tfc do
                    TryDispose kv.Value

    type SharedResources() =
        let key                             = ref InvalidId
        let brushes                         = ConcurrentDictionary<BrushKey                 , BrushDescriptor           * Direct2D1.Brush ref               >()
        let textFormats                     = ConcurrentDictionary<TextFormatKey            , TextFormatDescriptor      * DirectWrite.TextFormat ref        >()
        let geometries                      = ConcurrentDictionary<GeometryKey              , GeometryDescriptor        * Direct2D1.Geometry ref            >()
        let transformedGeometries           = ConcurrentDictionary<TransformedGeometryKey   , (GeometryKey* Matrix3x2)  * Direct2D1.TransformedGeometry ref >()

        let brushDescriptors                = ConcurrentDictionary<BrushDescriptor          , BrushKey                  >()
        let textFormatDescriptors           = ConcurrentDictionary<TextFormatDescriptor     , TextFormatKey             >()
        let geometryDescriptors             = ConcurrentDictionary<GeometryDescriptor       , GeometryKey               >()
        let transformedGeometryDescriptors  = ConcurrentDictionary<GeometryKey * Matrix3x2  , TransformedGeometryKey    >()

        let generateKey () = Interlocked.Increment key

        let getKey
            (resources  : ConcurrentDictionary<int, 'D*'V ref>  )
            (keys       : ConcurrentDictionary<'D, int>         )
            (d          : 'D                                    )
            : int =

            match keys.Find d with
            | Some key  ->  key
            | _         ->
                let rec insertKey () =
                    let key = generateKey ()
                    let r   = keys.TryAdd (d, key)
                    if r then
                        let ir = resources.TryAdd (key, (d, ref null))
                        if ir then key
                        else failwith "Failed to insert new key into resource dictionary, as a new key is unique this is shouldn't happen"
                    else insertKey ()
                insertKey ()

        let freeResources (resources  : ConcurrentDictionary<int, 'D*'V ref>  ) =
            for kv in resources do
                let d,r = kv.Value
                let resource = !r
                r := null
                TryDispose resource

        interface IDisposable with
            member x.Dispose () =
                freeResources brushes
                freeResources textFormats
                freeResources geometries
                freeResources transformedGeometries

        member x.Device_Brushes                 = brushes
        member x.Device_TextFormats             = textFormats
        member x.Device_Geometries              = geometries
        member x.Device_TransformedGeometries   = transformedGeometries

        member x.GetBrush                 (bd : BrushDescriptor)        : BrushKey      =
                getKey brushes brushDescriptors bd

        member x.GetTextFormat            (tfd : TextFormatDescriptor)  : TextFormatKey =
                getKey textFormats textFormatDescriptors tfd

        member x.GetGeometry              (gd : GeometryDescriptor)     : GeometryKey   =
                getKey geometries geometryDescriptors gd

        member x.GetTransformedGeometry   (gk : GeometryKey) (t : Matrix3x2) : GeometryKey =
                let d = gk,t
                getKey transformedGeometries transformedGeometryDescriptors d

    [<AbstractClass>]
    type GenericDevice() =
        abstract member GetBrush                : BrushKey*float32*float32  -> Direct2D1.Brush
        abstract member GetTextFormat           : TextFormatKey             -> DirectWrite.TextFormat
        abstract member GetGeometry             : GeometryKey               -> Direct2D1.Geometry
        abstract member GetTransformedGeometry  : TransformedGeometryKey    -> Direct2D1.TransformedGeometry


    type WindowedDevice(form : Windows.RenderForm, sharedResources : SharedResources) =

        inherit GenericDevice()

        let GetDeviceAndSwapChain (form : Windows.RenderForm) =
            let clientSize          = form.ClientSize
            let width               = clientSize.Width
            let height              = clientSize.Height

            let mutable desc        = DXGI.SwapChainDescription()
            desc.BufferCount        <- 2
            desc.ModeDescription    <- SharpDX.DXGI.ModeDescription(
                                        width                           ,
                                        height                          ,
                                        DXGI.Rational(60, 1)            ,
                                        DXGI.Format.R8G8B8A8_UNorm
                                        )
            desc.IsWindowed         <- Bool true
            desc.OutputHandle       <- form.Handle
            desc.SampleDescription  <- DXGI.SampleDescription(1,0)
            desc.SwapEffect         <- DXGI.SwapEffect.Sequential
            desc.Usage              <- DXGI.Usage.RenderTargetOutput

            let device              = RefOf<Direct3D11.Device>
            let swapChain           = RefOf<DXGI.SwapChain>

            let featureLevels       =
                [|
                    Direct3D.FeatureLevel.Level_11_0
                    Direct3D.FeatureLevel.Level_10_1
                    Direct3D.FeatureLevel.Level_10_0
                    Direct3D.FeatureLevel.Level_9_3
                    Direct3D.FeatureLevel.Level_9_2
                    Direct3D.FeatureLevel.Level_9_1
                |]

            Direct3D11.Device.CreateWithSwapChain(
                Direct3D.DriverType.Hardware                ,
                Direct3D11.DeviceCreationFlags.BgraSupport  ,
                featureLevels                               ,
                desc                                        ,
                device                                      , swapChain
                )

            !device, !swapChain

        let clientSize          = form.ClientSize
        let width               = float32 clientSize.Width
        let height              = float32 clientSize.Height

        let directWrite         = new DirectWrite()
        let d2dFactory          = new Direct2D1.Factory(Direct2D1.FactoryType.SingleThreaded)
        let device, swapChain   = GetDeviceAndSwapChain form
        let factory             = swapChain.GetParent<DXGI.Factory>()

        let associateWithWindow = factory.MakeWindowAssociation(form.Handle, DXGI.WindowAssociationFlags.IgnoreAll)

        let backBuffer          = Direct3D11.Texture2D.FromSwapChain<Direct3D11.Texture2D>(swapChain, 0)
        let surface             = backBuffer.QueryInterface<SharpDX.DXGI.Surface>();
        let d2dRenderTarget     = new Direct2D1.RenderTarget(
                                    d2dFactory                          ,
                                    surface                             ,
                                    Direct2D1.RenderTargetProperties(
                                        Direct2D1.PixelFormat(
                                            DXGI.Format.Unknown         ,
                                            Direct2D1.AlphaMode.Premultiplied
                                            )
                                        )
                                    )

        let DrawPathGeometry (painter : GeometrySink->unit) : Direct2D1.Geometry =
            let pg = new PathGeometry(d2dFactory)
            use sink = pg.Open ()
            painter sink
            sink.Close ()
            upcast pg

        let DrawPathGeometryFromPoints (points : Point list) : Direct2D1.Geometry =
            DrawPathGeometry <| fun sink ->
                if points.Length > 1 then
                    let points  = points |> List.toArray
                    let x,y     = points.[0]
                    let xx = new ArcSegment()
                    sink.BeginFigure (Vector2 (x,y), FigureBegin.Filled)
                    for i in 1..(points.Length - 1) do
                        let x,y = points.[i]
                        sink.AddLine (Vector2 (x,y))
                    sink.EndFigure FigureEnd.Closed

        let (|Lines|_|) (segments : PathSegment list) =
            let lines               = ResizeArray<Vector2> ()
            let mutable remaining   = segments
            while
                not remaining.IsEmpty
                &&  match remaining.Head with
                    | Line p     -> lines.Add <| AsVector p
                                    true
                    | _          -> false
                do
                ()

            if lines.Count > 0 then Some (lines.ToArray (), remaining)
            else None

        let (|Beziers|_|) (segments : PathSegment list) =
            let beziers             = ResizeArray<BezierSegment> ()
            let mutable remaining   = segments
            while
                not remaining.IsEmpty
                &&  match remaining.Head with
                    | Bezier (c0, c1, e)  ->
                        let mutable bs  = BezierSegment()
                        bs.Point1       <- AsVector c0
                        bs.Point2       <- AsVector c1
                        bs.Point3       <- AsVector e
                        beziers.Add bs
                        true
                    | _          -> false
                do
                ()

            if beziers.Count > 0 then Some (beziers.ToArray (), remaining)
            else None

        let (|QuadraticBeziers|_|) (segments : PathSegment list) =
            let qbeziers            = ResizeArray<QuadraticBezierSegment> ()
            let mutable remaining   = segments
            while
                not remaining.IsEmpty
                &&  match remaining.Head with
                    | QuadraticBezier (c, e)  ->
                        let mutable qbs = QuadraticBezierSegment()
                        qbs.Point1      <- AsVector c
                        qbs.Point2      <- AsVector e
                        qbeziers.Add qbs
                        true
                    | _          -> false
                do
                ()

            if qbeziers.Count > 0 then Some (qbeziers.ToArray (), remaining)
            else None

        let DrawPathGeometryFromFigures (figures : PathFigure list) : Direct2D1.Geometry =
            DrawPathGeometry <| fun sink ->
                for figure in figures do
                    let x,y = figure.Start
                    sink.BeginFigure
                        (
                            Vector2 (x,y),
                            if figure.Filled then FigureBegin.Filled
                            else FigureBegin.Hollow
                        )

                    let rec drawSegments segments =
                        match segments with
                        | Lines (ls, remaining)             ->  sink.AddLines ls
                                                                drawSegments remaining
                        | Beziers (bs, remaining)           ->  sink.AddBeziers bs
                                                                drawSegments remaining
                        | QuadraticBeziers (qbs, remaining)  -> sink.AddQuadraticBeziers qbs
                                                                drawSegments remaining

                    sink.EndFigure
                        (
                            if figure.Closed then FigureEnd.Closed
                            else FigureEnd.Closed
                        )

        let CreateGradientStopCollection (m : BrushExtendMode) (stops : GradientStop list) =
                let gstops          =
                    stops
                    |> List.map (fun s ->
                                    let mutable stop = GradientStop()
                                    stop.Color      <- s.Color.ToColor4
                                    stop.Position   <- s.Position
                                    stop
                                )
                    |> List.toArray
                let mode =
                    match m with
                    | ClampBrush    -> Direct2D1.ExtendMode.Clamp
                    | WrapBrush     -> Direct2D1.ExtendMode.Wrap
                    | MirrorBrush   -> Direct2D1.ExtendMode.Mirror

                new Direct2D1.GradientStopCollection(d2dRenderTarget, gstops, mode)


        let CreateBrush (bd : BrushDescriptor) : Direct2D1.Brush =
                match bd with
                | Transparent                       -> null
                | SolidColor c                      -> upcast new Direct2D1.SolidColorBrush(d2dRenderTarget, c.ToColor4)
                | LinearGradient (s, e, m, stops)   ->
                    let mutable props   = Direct2D1.LinearGradientBrushProperties()
                    props.StartPoint    <- AsVector s
                    props.EndPoint      <- AsVector e
                    use collection      = CreateGradientStopCollection m stops
                    upcast new Direct2D1.LinearGradientBrush(d2dRenderTarget, props, collection)
                | RadialGradient (c, o, (x,y), m, stops)   ->
                    let mutable props   = Direct2D1.RadialGradientBrushProperties()
                    props.Center                <- AsVector c
                    props.GradientOriginOffset  <- AsVector o
                    props.RadiusX               <- x
                    props.RadiusY               <- y
                    use collection      = CreateGradientStopCollection m stops
                    upcast new Direct2D1.RadialGradientBrush(d2dRenderTarget, props, collection)

        let CreateGeometry (gd : GeometryDescriptor) : Direct2D1.Geometry =
                match gd with
                | PathGeometry      figures -> DrawPathGeometryFromFigures figures
                | PolygonGeometry   points  -> DrawPathGeometryFromPoints points

        override x.GetBrush (key : BrushKey, opacity : float32, globalOpacity : float32) : Direct2D1.Brush =
            if opacity > 0.F && globalOpacity > 0.F then
                let find = sharedResources.Device_Brushes.Find key
                match find with
                | Some (_, brush) when !brush <> null -> 
                    (!brush).Opacity <- opacity * globalOpacity
                    !brush
                | Some (brushDescriptor, brush) ->
                    brush := CreateBrush brushDescriptor
                    (!brush).Opacity <- opacity * globalOpacity
                    !brush
                | _ -> null

            else null

        member x.SharedResources    = sharedResources

        member x.DirectWrite        = directWrite

        override x.GetTextFormat (key : TextFormatKey) : DirectWrite.TextFormat =
            let find = sharedResources.Device_TextFormats.Find key
            match find with
            | Some (_, textFormat) when !textFormat <> null -> !textFormat
            | Some (textFormatDescriptor, textFormat) ->
                textFormat := directWrite.CreateTextFormat textFormatDescriptor
                !textFormat
            | _ -> null

        override x.GetGeometry (key : GeometryKey) : Direct2D1.Geometry =
            let find = sharedResources.Device_Geometries.Find key
            match find with
            | Some (_, geometry) when !geometry <> null -> !geometry
            | Some (geometryDescriptor, geometry) ->
                geometry := CreateGeometry geometryDescriptor
                !geometry
            | _ -> null


        override x.GetTransformedGeometry (key : TransformedGeometryKey) : Direct2D1.TransformedGeometry =
            let find = sharedResources.Device_TransformedGeometries.Find key
            match find with
            | Some (_, geometry) when !geometry <> null -> !geometry
            | Some ((geometryKey, t), geometry) ->
                let g     = x.GetGeometry geometryKey
                geometry := new TransformedGeometry(d2dFactory, g, t)
                !geometry
            | _ -> null

        member x.Width              = width
        member x.Height             = height

        member x.Draw (a : Direct2D1.RenderTarget->unit) =
            d2dRenderTarget.BeginDraw()
            try
                a d2dRenderTarget
            finally
                d2dRenderTarget.EndDraw()
                swapChain.Present (1, DXGI.PresentFlags.None)


        interface IDisposable with
            member x.Dispose() =

                TryDispose sharedResources
                TryDispose d2dRenderTarget
                TryDispose surface
                TryDispose backBuffer
                TryDispose factory
                TryDispose swapChain
                TryDispose device
                TryDispose d2dFactory
                TryDispose directWrite




