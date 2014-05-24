namespace silberman

open System
open System.Collections.Generic
open System.Diagnostics
open System.Linq

open SharpDX
open SharpDX.Direct2D1

open Fundamental

module public Device = 
    
    type internal DirectWrite () = 
        let dwFactory           = new DirectWrite.Factory (DirectWrite.FactoryType.Shared)

        let textFormatCache     = Dictionary<TextFormatDescriptor, DirectWrite.TextFormat>()

        let CreateTextFormat (tfd : TextFormatDescriptor) =
                new DirectWrite.TextFormat(dwFactory, tfd.FontFamily, tfd.FontSize)

        member x.GetTextFormat (tfd : TextFormatDescriptor) : DirectWrite.TextFormat = textFormatCache.GetOrAdd(tfd, CreateTextFormat)

        member x.EstimateTextSize (tfd : TextFormatDescriptor) (sz : Size2F) (text : string) = 
            let tf = x.GetTextFormat tfd
            use tl = new DirectWrite.TextLayout(dwFactory, text, tf, sz.Width, sz.Height)
            let m = tl.Metrics
            Size2F (m.Width, m.Height)

        interface IDisposable with
            member x.Dispose() =
                let tfc = textFormatCache.ToArray ()
                textFormatCache.Clear()
                for kv in tfc do
                    TryDispose kv.Value 
        
    [<AbstractClass>]
    type internal GenericDevice() =
        abstract member GetBrush                : BrushDescriptor*float32       -> Direct2D1.Brush
        abstract member GetTextFormat           : TextFormatDescriptor          -> DirectWrite.TextFormat
        abstract member GetGeometry             : GeometryDescriptor            -> Direct2D1.Geometry
        abstract member GetTransformedGeometry  : GeometryDescriptor*Matrix3x2  -> Direct2D1.Geometry
        

    type internal WindowedDevice(form : Windows.RenderForm) = 

        inherit GenericDevice()

        let GetDeviceAndSwapChain (form : Windows.RenderForm) =
            let width               = form.ClientSize.Width
            let height              = form.ClientSize.Height

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

        let width               = float32 form.ClientSize.Width
        let height              = float32 form.ClientSize.Height

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

        let DrawPathGeometryFromVertices (vertices : (float32*float32) array) : Direct2D1.Geometry =
            DrawPathGeometry <| fun sink ->
                if vertices.Length > 1 then
                    Debug.Assert (vertices.Length > 2)
                    let x,y     = vertices.[0]
                    sink.BeginFigure (Vector2 (x,y), FigureBegin.Filled)
                    for i in 1..(vertices.Length - 1) do
                        let x,y = vertices.[i]
                        sink.AddLine (Vector2 (x,y))
                    sink.EndFigure FigureEnd.Closed


        let Solid (c : ColorDescriptor) =  new Direct2D1.SolidColorBrush(d2dRenderTarget, c.ToColor4)                                    

        let CreateBrush (bd : BrushDescriptor) : Direct2D1.Brush = 
                match bd with
                | Transparent       -> null
                | SolidColor c      -> upcast Solid c
            
        let brushCache = Dictionary<BrushDescriptor, Direct2D1.Brush>()

        let geometryCache = 
                [
                    EquilateralTriangle     , DrawPathGeometryFromVertices 
                                                [|
                                                     +0.0F, (sqrt 3.0F) / 2.0F - 1.0F / (2.0F * sqrt 3.0F)
                                                     +0.5F, - 1.0F / (2.0F * sqrt 3.0F)
                                                     -0.5F, - 1.0F / (2.0F * sqrt 3.0F)
                                                |]
                    Triangle45x45x90        , DrawPathGeometryFromVertices 
                                                [|
                                                     +0.0F, +0.0F
                                                     +0.5F, +0.5F
                                                     -0.5F, +0.5F
                                                |]
                    UnitSquare              , DrawPathGeometryFromVertices 
                                                [|
                                                     +0.5F, +0.5F
                                                     +0.5F, -0.5F
                                                     -0.5F, -0.5F
                                                     -0.5F, +0.5F
                                                |]
                ] |> ToDictionary

        let transformedGeometryCache = Dictionary<GeometryDescriptor*Matrix3x2, Direct2D1.Geometry>()

        let CreateTransformedGeometry (shape : GeometryDescriptor, transform : Matrix3x2) : Direct2D1.Geometry = 
                upcast new TransformedGeometry(d2dFactory, geometryCache.[shape], transform)

        override x.GetBrush (bd : BrushDescriptor, opacity : float32) : Direct2D1.Brush =
            if opacity > 0.F then 
                let b = brushCache.GetOrAdd(bd, CreateBrush)
                b.Opacity <- opacity
                b
            else null

        member x.DirectWrite = directWrite

        override x.GetTextFormat (tfd : TextFormatDescriptor) : DirectWrite.TextFormat =
            x.DirectWrite.GetTextFormat tfd

        override x.GetGeometry (shape : GeometryDescriptor) : Direct2D1.Geometry =
            geometryCache.[shape]
        
        override x.GetTransformedGeometry (shape : GeometryDescriptor, transform : Matrix3x2) : Direct2D1.Geometry =
            let g = transformedGeometryCache.GetOrAdd((shape, transform), CreateTransformedGeometry)
            g
        
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
                let bc = brushCache.ToArray()
                brushCache.Clear()
                for kv in bc do
                    TryDispose kv.Value 
                TryDispose d2dRenderTarget
                TryDispose surface
                TryDispose backBuffer
                TryDispose factory
                TryDispose swapChain
                TryDispose device
                TryDispose d2dFactory
                TryDispose directWrite
            



