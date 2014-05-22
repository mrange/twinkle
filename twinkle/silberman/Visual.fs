namespace silberman

open System
open System.Diagnostics

open SharpDX

open Fundamental

module public Visual =

    // Compute pixel scale (approximation)
    let inline private getLocalLength (transform : Matrix3x2, x : float32, y : float32)  = 
            let p = Vector2(x,y)
            let t = transform.TransformPoint p
            t.Length()

    [<ReferenceEquality>]
    [<NoComparison>]
    type VisualTree =
        | NoVisual
        | Rectangle of  Stroke      : AnimatedBrush         *
                        Fill        : AnimatedBrush         *
                        Rect        : AnimatedRectangleF    *
                        StrokeWidth : AnimatedFloat
        | Line      of  Point0      : AnimatedVector2       *
                        Point1      : AnimatedVector2       *
                        Brush       : AnimatedBrush         *
                        StrokeWidth : AnimatedFloat
        | Geometry  of  Shape       : ShapeDescriptor       *
                        Stroke      : AnimatedBrush         *
                        Fill        : AnimatedBrush         *
                        Transform   : AnimatedMatrix        *
                        StrokeWidth : AnimatedFloat
        | Text      of  Text        : string                *
                        TextFormat  : TextFormatDescriptor  *
                        LayoutRect  : AnimatedRectangleF    *
                        Foreground  : AnimatedBrush
        | Transform of  Transform   : AnimatedMatrix        *
                        Reverse     : AnimatedMatrix        *
                        Child       : VisualTree
        | Group     of  Children    : VisualTree array
        | Fork      of  Left        : VisualTree            *
                        Right       : VisualTree
        | State     of  State       : obj                   *
                        Child       : VisualTree
         
    let rec HasVisuals (vt : VisualTree) =
        match vt with
        | NoVisual          -> false
        | Rectangle _       -> true
        | Line _            -> true
        | Geometry _        -> true
        | Text _            -> true
        | Transform (t,r,c) -> HasVisuals c
        | Group cs          -> (cs |> Array.tryFindIndex (fun c -> HasVisuals c)).IsSome
        | Fork (l,r)        -> (HasVisuals l) && (HasVisuals r)
        | State (_,c)       -> HasVisuals c

    // TODO: Use non-curry function for performance 
    // TODO: Add support for geometry realizations
    // TODO: Add support for caching of subtrees
    
    let rec RenderTreeImpl 
        (state      : ApplicationState                              ) 
        (rt         : Direct2D1.RenderTarget                        ) 
        (tfc        : TextFormatDescriptor->DirectWrite.TextFormat  ) 
        (bc         : BrushDescriptor*float32->Direct2D1.Brush      ) 
        (gc         : ShapeDescriptor->Direct2D1.Geometry           ) 
        (transform  : Matrix3x2                                     ) 
        (pixelScale : float32                                       )  
        (vt         : VisualTree                                    ) 
        = 
        match vt with 
        | NoVisual   -> ()
        | Rectangle (s,f,r,sw) ->
                let rect        = r state
                let strokeWidth = sw state
                let fill        = f state
                let stroke      = s state
          
                let bfill       = bc fill
                let bstroke     = bc stroke
          
                if bfill <> null then rt.FillRectangle (rect, bfill)
                if bstroke <> null && strokeWidth > 0.F then rt.DrawRectangle (rect, bstroke, strokeWidth)
        | Line (p0,p1,b,sw) ->
                let point0      = p0 state
                let point1      = p1 state
                let stroke      = b state
                let strokeWidth = sw state
          
                let bstroke     = bc stroke
          
                if bstroke <> null && strokeWidth >= 0.F then rt.DrawLine (point0, point1, bstroke, strokeWidth)
        | Geometry (shape,s,f,t,sw) ->
                let trans       = t state
                let strokeWidth = sw state
                let fill        = f state
                let stroke      = s state

                let gshape      = gc shape
          
                let bfill       = bc fill
                let bstroke     = bc stroke
            
                let fullTransform   = trans * transform

                rt.Transform        <- fullTransform

                if bfill <> null then rt.FillGeometry (gshape, bfill)
                if bstroke <> null && strokeWidth > 0.F then rt.DrawGeometry (gshape, bstroke, strokeWidth)

                rt.Transform        <- transform
        | Text (t,tf,lr,fg) ->
                let layoutRect  = lr state
                let foreground  = fg state
                let textFormat  = tfc tf
          
                let bforeground = bc foreground
          
                if bforeground <> null then rt.DrawText(t, textFormat, layoutRect, bforeground)
        | Transform (t,r,c) ->
                let trans           = t state
                let rtrans          = r state

                Debug.Assert (IsIdentity <| trans.Multiply rtrans)

                let s               = state.Transform rtrans

                let fullTransform   = trans * transform
                let pixelScale      = getLocalLength (fullTransform,1.F,1.F)
          
                rt.Transform <- fullTransform

                RenderTreeImpl s rt tfc bc gc fullTransform pixelScale c
          
                rt.Transform <- transform                 
        | Group (cs) ->
                for branch in cs do
                    RenderTreeImpl state rt tfc bc gc transform pixelScale branch 
        | Fork (l,r) ->
                RenderTreeImpl state rt tfc bc gc transform pixelScale l 
                RenderTreeImpl state rt tfc bc gc transform pixelScale r
        | State (_,c) ->
                RenderTreeImpl state rt tfc bc gc transform pixelScale c

    let RenderTree 
        (state      : ApplicationState                              ) 
        (rt         : Direct2D1.RenderTarget                        ) 
        (tfc        : TextFormatDescriptor->DirectWrite.TextFormat  ) 
        (bc         : BrushDescriptor*float32->Direct2D1.Brush      ) 
        (gc         : ShapeDescriptor->Direct2D1.Geometry           ) 
        (vt         : VisualTree                                    ) 
        = 
        RenderTreeImpl state rt tfc bc gc Matrix3x2.Identity 1.0F vt



