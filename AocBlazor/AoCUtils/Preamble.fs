[<AutoOpen>]
module AoCUtilsPreamble

open System
open System.IO
open System.Collections.Generic
open System.Diagnostics
open System.Text
open System.Text.RegularExpressions

[<RequireQualifiedAccess>]
module Obj =
    let inline equals a b = obj.Equals(a, b)
    let inline notEquals a b = not (equals a b)

    let inline refEquals a b = obj.ReferenceEquals(a, b)
    let inline notRefEquals a b = not (refEquals a b)

    let inline isNull a = refEquals a null
    let inline isNotNull a = notRefEquals a null

[<RequireQualifiedAccess>]
module Disposable =
    let inline dispose (x: #IDisposable) =
        if Obj.isNotNull x then
            x.Dispose()

    let inline disposeObj (x: obj) =
        match x with
        | :? IDisposable as x -> x.Dispose()
        | _ -> ()

    let defer f =
        { new IDisposable with
            member __.Dispose() = f() }

let inline dispose x = Disposable.dispose x
let inline disposeObj x = Disposable.disposeObj x
let inline defer f = Disposable.defer f

[<RequireQualifiedAccess>]
module Seq =
    let inline getEnumerator (source: 'a seq) = source.GetEnumerator()
    let inline moveNext (source: 'a IEnumerator) = source.MoveNext()
    let inline getCurrent (source: 'a IEnumerator) = source.Current

    let tee f =
        Seq.map (fun a ->
            f a
            a)

    let scanCond (f: 'state -> 'value -> ('state voption * bool)) (state: 'state) (source: 'value seq) =
        seq {
            use enumerator = getEnumerator source

            let rec loop () =
                seq {
                    if moveNext enumerator then
                        let result, cond = getCurrent enumerator |> f state

                        match result with
                        | ValueSome result -> yield result
                        | ValueNone -> ()

                        if cond then yield! loop ()
                }

            yield! loop ()
        }

    let foldCond (f: 'state -> 'value -> ('state voption * bool)) (state: 'state) (source: 'value seq) =
        scanCond f state source
        |> Seq.tryLast
        |> Option.defaultValue state

    let takeAtMost (n: int) (source: 'a seq) =
        seq {
            use enumerator = source.GetEnumerator()
            let mutable i = 0

            while i < n && enumerator.MoveNext() do
                yield enumerator.Current
                i <- i + 1
        }

[<RequireQualifiedAccess>]
module Array =
    let windows windowSize data =
        seq {
            for i = 0 to Array.length data - windowSize do
                yield ArraySegment(data, i, windowSize)
        }

    let iterateCombinations f combinationSize elementCount =
        let rec loop (combination: int []) (used: bool []) index =
            if index >= elementCount then
                f combination
            else
                let rec innerLoop element =
                    if element >= elementCount then
                        ValueNone
                    else if not used.[element] then
                        combination.[index] <- element
                        used.[element] <- true

                        match loop combination used (index + 1) with
                        | ValueSome result -> ValueSome result
                        | ValueNone ->
                            used.[element] <- false
                            innerLoop (element + 1)
                    else
                        innerLoop (element + 1)

                innerLoop 0

        let combination = Array.zeroCreate combinationSize
        let used = Array.zeroCreate elementCount
        loop combination used 0

[<RequireQualifiedAccess>]
module Array2D =
    let clearWith value source =
        Array2D.iteri (fun i j _ -> Array2D.set source i j value) source

    let countBy f source =
        let l1 = Array2D.length1 source - 1
        let l2 = Array2D.length2 source - 1
        let mutable result = 0

        for i = 0 to l1 do
            for j = 0 to l2 do
                if f (Array2D.get source i j) then
                    result <- result + 1

        result

    let countByij f source =
        let l1 = Array2D.length1 source - 1
        let l2 = Array2D.length2 source - 1
        let mutable result = 0

        for i = 0 to l1 do
            for j = 0 to l2 do
                if f i j (Array2D.get source i j) then
                    result <- result + 1

        result

[<RequireQualifiedAccess>]
module String =
    let splitByChar (ch: char) (source: string) =
        source.Split(
            [| ch |],
            StringSplitOptions.RemoveEmptyEntries
            ||| StringSplitOptions.TrimEntries
        )

    let indentWith (ch: char) (size: int) (source: string) =
        let pattern = Regex(@"^(.*)$", RegexOptions.Multiline)
        let indentStr = String(ch, size)
        pattern.Replace(source, indentStr + "$1")

    let indent (size: int) (source: string) = indentWith ' ' size source

[<RequireQualifiedAccess>]
module ValueOption =
    let inline noneWith f = function
        | ValueNone -> f()
        | ValueSome x -> ValueSome x

[<RequireQualifiedAccess>]
module Time =
    let sprintElapsed (time: TimeSpan) =
        let ms = time.TotalMilliseconds

        if ms >= 1.0 then
            sprintf "%0.3f ms" ms
        else
            let µs = ms * 1000.0

            if µs >= 1.0 then
                sprintf "%0.3f µs" µs
            else
                let ns = µs * 1000.0
                sprintf "%0.3f ns" ns

    let sprintElapsedN (n: int) (time: TimeSpan) =
        let perInput =
            if n > 0 then
                time.Divide(float n)
            else
                time

        let total = sprintElapsed time
        let perInput = sprintElapsed perInput
        $"{total} ({perInput}/input) N = {n}"

    let measureCold f =
        let sw = Stopwatch.StartNew()
        let result = f ()
        sw.Stop()
        sprintElapsed sw.Elapsed |> printfn "Elapsed: %s"
        result

    let measureColdN (n: int) f =
        let sw = Stopwatch.StartNew()
        let result = f ()
        sw.Stop()

        sprintElapsedN n sw.Elapsed
        |> printfn "Elapsed: %s"

        result

    let measure f =
        f () |> ignore
        let sw = Stopwatch.StartNew()
        let result = f ()
        sw.Stop()
        sprintElapsed sw.Elapsed |> printfn "Elapsed: %s"
        result

    let measureN (n: int) f =
        f () |> ignore
        let sw = Stopwatch.StartNew()
        let result = f ()
        sw.Stop()

        sprintElapsedN n sw.Elapsed
        |> printfn "Elapsed: %s"

        result

[<RequireQualifiedAccess>]
module OptionalBuilder =
    type OptionalBuilder() =
        member inline __.Bind(x, f) = Option.bind f x
        member inline __.Return x = Some x
        member inline __.ReturnFrom x = x

let optional = OptionalBuilder.OptionalBuilder()


[<RequireQualifiedAccess>]
module StringBuilderCE =
    type Delayed = StringBuilder -> unit


    let inline run (sb: StringBuilder) (delayed: Delayed) : string =
        do delayed sb
        sb.ToString()

    let inline delay ([<InlineIfLambda>] f: unit -> Delayed) : Delayed = fun sb -> (f ()) sb

    let zero: Delayed = ignore

    let inline combine ([<InlineIfLambda>] f: Delayed) ([<InlineIfLambda>] g: Delayed) : Delayed =
        fun sb ->
            do f sb
            do g sb

    let inline yieldString (v: string) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldCharArray (v: char []) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldChar (v: char) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldStringBuilder (v: StringBuilder) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldObj (v: obj) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldBool (v: bool) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldUInt8 (v: uint8) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldUInt16 (v: uint16) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldUInt32 (v: uint32) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldUInt64 (v: uint64) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldInt8 (v: int8) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldInt16 (v: int16) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldInt32 (v: int32) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldInt64 (v: int64) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldDecimal (v: decimal) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldFloat (v: float) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldFloat32 (v: float32) : Delayed = fun sb -> sb.Append(v) |> ignore


    let inline tryWith ([<InlineIfLambda>] handler: exn -> Delayed) ([<InlineIfLambda>] body: Delayed) : Delayed =
        fun sb ->
            try
                body sb
            with
            | exn -> (handler exn) sb

    let inline tryFinally ([<InlineIfLambda>] handler: unit -> unit) ([<InlineIfLambda>] body: Delayed) : Delayed =
        fun sb ->
            try
                body sb
            finally
                handler ()

    let inline using ([<InlineIfLambda>] body: 'd -> Delayed) (disposable: #IDisposable) =
        tryFinally (fun () -> dispose disposable) (fun sb -> body disposable sb)

    let rec whileLoop (body: Delayed) (guard: unit -> bool) : Delayed =
        fun sb ->
            if guard () then
                body sb
                whileLoop body guard sb
            else
                zero sb

    let inline forLoop ([<InlineIfLambda>] body: 'a -> Delayed) (source: 'a seq) =
        Seq.getEnumerator source
        |> using (fun enumerator ->
            (fun () -> Seq.moveNext enumerator)
            |> whileLoop (fun sb -> (enumerator |> Seq.getCurrent |> body) sb))


    type StringBuilderCEBuilder(?initialCapacity: int, ?initialValue: string) =
        let createSB () =
            match initialCapacity, initialValue with
            | None, None -> StringBuilder()
            | None, Some value -> StringBuilder(value)
            | Some capacity, None -> StringBuilder(capacity)
            | Some capacity, Some value -> StringBuilder(value, capacity)

        member this.Run delayed = run (createSB ()) delayed

        member inline this.Delay ([<InlineIfLambda>] delayed) = delay delayed

        member inline this.Zero() = ignore

        member inline this.Combine([<InlineIfLambda>] f, [<InlineIfLambda>] g) = combine f g


        member inline this.Yield value = yieldString value

        member inline this.Yield value = yieldCharArray value

        member inline this.Yield value = yieldChar value

        member inline this.Yield value = yieldStringBuilder value

        member inline this.Yield value = yieldBool value

        member inline this.Yield value = yieldObj value

        member inline this.Yield value = yieldUInt8 value

        member inline this.Yield value = yieldUInt16 value

        member inline this.Yield value = yieldUInt32 value

        member inline this.Yield value = yieldUInt64 value

        member inline this.Yield value = yieldInt8 value

        member inline this.Yield value = yieldInt16 value

        member inline this.Yield value = yieldInt32 value

        member inline this.Yield value = yieldInt64 value

        member inline this.Yield value = yieldDecimal value

        member inline this.Yield value = yieldFloat value

        member inline this.Yield value = yieldFloat32 value


        member inline this.TryWith([<InlineIfLambda>] body, [<InlineIfLambda>] handler) = tryWith handler body

        member inline this.TryFinally([<InlineIfLambda>] body, [<InlineIfLambda>] handler) = tryFinally handler body

        member inline this.Using(disposable, [<InlineIfLambda>] body) = using body disposable


        member inline this.While([<InlineIfLambda>] guard, [<InlineIfLambda>] body) = whileLoop body guard

        member inline this.For(source, [<InlineIfLambda>] body) = forLoop body source


let stringBuilder = StringBuilderCE.StringBuilderCEBuilder()

let stringBuilderWith capacity =
    StringBuilderCE.StringBuilderCEBuilder(initialCapacity = capacity)

let stringBuilderFrom value =
    StringBuilderCE.StringBuilderCEBuilder(initialValue = value)
