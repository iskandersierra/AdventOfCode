open System
open System.Collections.Generic

module Comparer =
    let ofFun (fn: 'a -> 'a -> int) =
        { new IComparer<'a> with
            member _.Compare(x, y) = fn x y }

    let invert (comparer: IComparer<'a>) =
        { new IComparer<'a> with
            member _.Compare(x, y) = comparer.Compare(y, x) }

module EqualityComparer =
    let ofFun (equals: 'a -> 'a -> bool) (hash: 'a -> int) =
        { new IEqualityComparer<'a> with
            member _.Equals(x, y) = equals x y
            member _.GetHashCode(x) = hash x }

    let never() = ofFun (fun _ _ -> false) (fun a -> ReferenceEqualityComparer.Instance.GetHashCode(a))

module Seq =
    let tryMaxBy comparer keySelector source =
        source
            |> Seq.fold
                (fun acc item ->
                    let itemKey = keySelector item
                    match acc with
                    | None -> Some (item, itemKey)
                    | Some (_, previousKey) ->
                        match comparer itemKey previousKey with
                        | comp when comp > 0 -> Some (item, itemKey)
                        | _ -> acc)
                None
            |> Option.map fst

    let tryMax comparer = tryMaxBy comparer id

    let tryMinBy comparer = tryMaxBy (fun a b -> comparer b a)

    let tryMin comparer = tryMinBy comparer id
