namespace silberman.Internal

open SharpDX

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Diagnostics
open System.Threading
open System.Windows.Forms

[<AutoOpen>]
module internal UtilsAutoOpen =

    let private globalId= ref 1

    let GenerateId ()   = Interlocked.Increment globalId

    let GlobalClock     = let sw = new Stopwatch ()
                          sw.Start ()
                          sw

    let CurrentTimeInMs () = GlobalClock.ElapsedMilliseconds

    let inline (|IsNaN|IsPositiveInfinity|IsNegativeInfinity|IsNegative|IsPositive|) (v : float32) =
        if      Single.IsNaN                v then IsNaN
        elif    Single.IsPositiveInfinity   v then IsPositiveInfinity
        elif    Single.IsNegativeInfinity   v then IsNegativeInfinity
        elif    v < 0.F                       then IsNegative
        else                                       IsPositive

    let inline DefaultOf<'T> = Unchecked.defaultof<'T>
    let inline RefOf<'T> = ref DefaultOf<'T>

    let inline Zero<'T when 'T : struct> = DefaultOf<'T>

    let inline Clamp (v : float32) =
        match v with
        | IsPositiveInfinity    -> v
        | IsPositive            -> v
        | _                     -> 0.F

    let IsNear a b = (abs <| a - b) < 0.0001F

    let IsIdentity (m : Matrix3x2) =
        IsNear 1.0F m.M11   &&
        IsNear 0.0F m.M12   &&
        IsNear 0.0F m.M21   &&
        IsNear 1.0F m.M22   &&
        IsNear 0.0F m.M31   &&
        IsNear 0.0F m.M32

    let ParseHex (defaultTo : int) (ch : char) =
        match ch with 
        | _ when ch >= '0' && ch <= '9' -> int ch - int '0'
        | _ when ch >= 'a' && ch <= 'f' -> int ch - int 'a' + 10
        | _ when ch >= 'A' && ch <= 'F' -> int ch - int 'A' + 10
        | _                             -> defaultTo

    let ParseShortColorComponent (mostSignificant : char) =
        let ms = ParseHex -15 mostSignificant
        (float32 ms) / 15.F

    let ParseLongColorComponent (mostSignificant: char) (leastSignificant : char) =
        let ms = ParseHex -255  mostSignificant
        let ls = ParseHex -255  leastSignificant
        (float32 <| ms * 16 + ls) / 255.F

    let ParseColor (s : string) =
            if s.Length < 1 || s.[0] <> '#' then 1.F,-1.F,-1.F,1.F
            else
                match s.Length with
                | 4 -> 
                    let r = ParseShortColorComponent s.[1]
                    let g = ParseShortColorComponent s.[2]
                    let b = ParseShortColorComponent s.[3]
                    1.F,r,g,b
                | 5 -> 
                    let a = ParseShortColorComponent s.[1]
                    let r = ParseShortColorComponent s.[2]
                    let g = ParseShortColorComponent s.[3]
                    let b = ParseShortColorComponent s.[4]
                    a,r,g,b
                | 7 -> 
                    let r = ParseLongColorComponent s.[1] s.[2]
                    let g = ParseLongColorComponent s.[3] s.[4]
                    let b = ParseLongColorComponent s.[5] s.[6]
                    1.F,r,g,b
                | 9 -> 
                    let a = ParseLongColorComponent s.[1] s.[2]
                    let r = ParseLongColorComponent s.[3] s.[4]
                    let g = ParseLongColorComponent s.[5] s.[6]
                    let b = ParseLongColorComponent s.[7] s.[8]
                    a,r,g,b
                | _ -> -1.F,-1.F,-1.F,1.F


    let Log             (message  : string)= printfn "Information : %s" message
    let LogWarning      (message  : string)= printfn "Warning     : %s" message
    let LogError        (message  : string)= printfn "Error       : %s" message
    let LogException    (exn      : exn)   = printfn "Exception   : %s" exn.Message

    let Deg2Rad = float32 Math.PI/180.F
    let Rad2Deg = 1.F / Deg2Rad

    let Vector2 x y = Vector2(x,y)

    let Normalize (v : Vector2) = v.Normalize(); v

    let TryDispose (disposable : IDisposable) =
        try
            if disposable <> null then disposable.Dispose ()
        with
        | exn -> printfn "Caught exception: %A" exn

    let TryDisposeSequence (s : seq<#IDisposable>) =
        for v in s do
            TryDispose v

    let TryRun (action : unit -> unit) =
        try
            action()
        with
        | exn -> printfn "Caught exception: %A" exn


    type Disposable(action : unit->unit) =
        interface IDisposable with
            member x.Dispose() = TryRun action

    let OnExit action : IDisposable = upcast new Disposable(action)

    let AsNullable value = Nullable<_>(value)

    let CompareAndExchange<'T when 'T : not struct> (f : 'T -> 'T) (valueReference : 'T ref) =
        while let value = !valueReference in not (Object.ReferenceEquals (Interlocked.CompareExchange (valueReference, f value, value), value)) do
            ()

    let DispatchAction (control : Control) (action : unit->unit) =
        let a = Action action
        ignore <| control.BeginInvoke(a)

    let DefaultTo (optional : 'T option) (defaultValue : 'T) =
        match optional with
        | Some v    -> v
        | _         -> defaultValue

    let CastTo<'T> (o : obj) (defaultValue : 'T) =
        match o with
        | :? 'T as v    -> v
        | _             -> defaultValue

    let As<'T> (o : obj) =
        match o with
        | :? 'T as v    -> Some v
        | _             -> None

    let Is<'T> (o : obj) =
        match o with
        | :? 'T         -> true
        | _             -> false


    let DisposeWith (l : #IDisposable) (r : #IDisposable) =
        OnExit <| fun () ->
                    TryDispose l
                    r.Dispose ()

    // TODO: Scrap these and replace with extension methods?
    // Extension methods seems easier to maintain since function application has high precence leads to less parantese
    // Rethinking; functions easier to use with List.map and similar. Provide both is probably best

    let inline ( <??> ) optional defaultValue = DefaultTo optional defaultValue
    let inline ( <???> ) o defaultValue = CastTo o defaultValue

    type IDisposable with
        member x.DisposeWith (o : IDisposable) = DisposeWith x o

    type Matrix3x2 with
        member x.Multiply (o : Matrix3x2) = Matrix3x2.Multiply(x,o)
        member x.TransformPoint (p : Vector2) = Matrix3x2.TransformPoint(x,p)

    type Type with
        member x.Ancestors () =
            let t = ref x
            seq {
                while !t <> null do
                    yield !t
                    t := (!t).BaseType
            }

    type RectangleF with
        member x.Union (other : RectangleF) =
            if x.IsEmpty then other
            elif other.IsEmpty then x
            else
                let left    = min x.Left    other.Left
                let top     = min x.Top     other.Top
                let right   = max x.Right   other.Right
                let bottom  = max x.Bottom  other.Bottom
                RectangleF (left, top, right - left, bottom - top)

    type Object with
        member x.CastTo (defaultValue : 'T) = x <???> defaultValue
        member x.As<'T> () = As<'T> x
        member x.Is<'T> () = Is<'T> x

    type IDictionary<'TKey, 'TValue> with
        member x.Lookup (key : 'TKey) (defaultValue : 'TValue) =
                    let v = RefOf<'TValue>
                    if x.TryGetValue(key, v) then !v
                    else defaultValue


        member x.Find (key : 'TKey) : 'TValue option =
                    let v = RefOf<'TValue>
                    if x.TryGetValue(key, v) then Some !v
                    else None

        member x.GetOrAdd (key : 'TKey, creator : 'TKey -> 'TValue) =
                    let v = RefOf<'TValue>
                    if x.TryGetValue(key, v) then !v
                    else
                        v := creator key
                        x.Add (key, !v)
                        !v

    let AsDictionary (l : seq<'TKey*'TValue>) =
        let d = Dictionary<'TKey, 'TValue> ()
        for k,v in l do
            d.[k] <- v
        d

    let AsConcurrentDictionary (l : seq<'TKey*'TValue>) =
        let d = ConcurrentDictionary<'TKey, 'TValue> ()
        for k,v in l do
            ignore <| d.TryAdd (k,v)
        d

    type TypeDictionary<'T>() =

        let safe     = obj()
        let explicit = ConcurrentDictionary<Type,'T>()
        let implicit = ConcurrentDictionary<Type,'T option>()

        member x.Add k v =
            lock safe <| fun () ->
                            if explicit.TryAdd(k,v) then
                                implicit.Clear ()
                            else
                                ()

        member x.Replace k (v : 'T) =
            lock safe <| fun () ->
                            ignore <| explicit.AddOrUpdate(k,v,(fun _ _ -> v))
                            implicit.Clear ()

        member x.Remove k v =
            lock safe <| fun () ->
                            if explicit.TryRemove(k,v) then
                                implicit.Clear ()
                            else
                                ()

        member x.Clear () =
            lock safe <| fun () ->
                            explicit.Clear ()
                            implicit.Clear ()

        member private x.TryFindBase_NoLock (k : Type) =
            let v = RefOf<'T>
            let found = explicit.TryGetValue(k, v)
            if found then
                let r = Some !v
                ignore <| implicit.TryAdd(k,r)
                r
            else
                let b = k.BaseType
                if b = null then None
                else
                    let r = x.TryFindBase_NoLock b
                    ignore <| implicit.TryAdd(k,r)
                    r


        member x.TryFind k =
            let v : 'T option ref = ref None
            if implicit.TryGetValue(k, v) then
                !v
            else
                lock safe <| fun () -> x.TryFindBase_NoLock k


module internal Async =
    let SwitchToThread2 (state : ApartmentState) (tp : ThreadPriority): Async<unit> =
        Async.FromContinuations <| fun (cont, econt, ccont) ->
                try
                    let thread = Thread(fun () ->
                                    try
                                        cont ()
                                    with
                                    | e -> econt e
                                    )
                    thread.IsBackground <- true
                    thread.SetApartmentState state
                    thread.Priority <- tp
                    thread.Start ()
                with
                | e -> econt e
