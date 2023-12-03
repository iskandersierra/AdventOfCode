namespace AocUtils.Search

open System.Collections.Generic

type ICostDescription<'cost> =
    abstract ZeroCost : 'cost
    abstract AddCosts : 'cost -> 'cost -> 'cost
    abstract Comparer : IComparer<'cost>

module CostDescription =
    let int32 =
        { new ICostDescription<int32> with
            member this.ZeroCost = 0
            member this.AddCosts a b = a + b
            member this.Comparer = Comparer<int32>.Default }

[<Struct>]
type SearchDecision<'item, 'decision> = { item: 'item; decision: 'decision }

type ISearchDescription<'item, 'decision> =
    abstract GetSourceItems : unit -> seq<'item>
    abstract GetDecisions : 'item -> seq<SearchDecision<'item, 'decision>>
    abstract IsSolutionItem : 'item -> bool

type IPrioritySearch<'item, 'decision, 'cost> =
    abstract Search : ISearchDescription<'item, 'decision>
    abstract Cost : ICostDescription<'cost>
    abstract ItemComparer : IEqualityComparer<'item>

    abstract GetItemCost : 'item -> 'cost
    abstract GetDecisionCost : 'decision -> 'cost

type SearchStep<'item, 'decision, 'cost> =
    | FirstStep of 'item * 'cost
    | NextStep of 'item * 'decision * 'cost

type SearchPath<'item, 'decision, 'cost> = SearchStep<'item, 'decision, 'cost> list

module PrioritySearch =
    type SearchNode<'item, 'decision, 'cost> =
        { item: 'item
          cost: 'cost
          isSolution: bool
          parent: ParentRef<'item, 'decision, 'cost> voption }

    and [<Struct>] ParentRef<'item, 'decision, 'cost> =
        { node: SearchNode<'item, 'decision, 'cost>
          decision: 'decision }

    let buildSearchPath (node: SearchNode<'item, 'decision, 'cost>) : SearchPath<'item, 'decision, 'cost> =
        let rec loop (node: SearchNode<'item, 'decision, 'cost>) list =
            match node.parent with
            | ValueNone -> list
            | ValueSome parent ->
                let step =
                    NextStep(parent.node.item, parent.decision, node.cost)

                loop parent.node (step :: list)

        loop node [ FirstStep(node.item, node.cost) ]
        |> List.rev

    let getSearchDepth (node: SearchNode<'item, 'decision, 'cost>) =
        let rec loop node depth =
            match node.parent with
            | ValueNone -> depth
            | ValueSome parent -> loop parent.node (depth + 1)

        loop node 0

    type DebugOptions<'item, 'decision, 'cost> =
        {
            yieldNode: SearchNode<'item, 'decision, 'cost> -> bool
            continueAfter: SearchNode<'item, 'decision, 'cost> -> bool
            repeating: 'item -> unit
            enqueuing: SearchNode<'item, 'decision, 'cost> * 'cost -> unit
            dequeuing: SearchNode<'item, 'decision, 'cost> * 'cost -> unit
        }

    let debugFind<'item, 'decision, 'cost>
        (options: DebugOptions<'item, 'decision, 'cost>)
        (search: IPrioritySearch<'item, 'decision, 'cost>) =
        seq {
            let queue =
                PriorityQueue<SearchNode<'item, 'decision, 'cost>, 'cost>(search.Cost.Comparer)

            let memo = HashSet<'item>(search.ItemComparer)

            let mutable finished = false

            let insertSources () : SearchNode<'item, 'decision, 'cost> seq =
                seq {
                    use enumerator = search.Search.GetSourceItems() |> Seq.getEnumerator

                    while not finished && Seq.moveNext enumerator do
                        let item = enumerator.Current
                        if not (memo.Add item) then
                            options.repeating item
                        else
                            let itemCost = search.GetItemCost item
                            let isSolution = search.Search.IsSolutionItem item

                            let node =
                                { item = item
                                  cost = itemCost
                                  isSolution = isSolution
                                  parent = ValueNone }

                            if options.yieldNode node then yield node

                            if options.continueAfter node then
                                options.enqueuing (node, itemCost)
                                queue.Enqueue(node, itemCost)
                            else
                                finished <- true
                }

            let rec loop () : SearchNode<'item, 'decision, 'cost> seq =
                seq {
                    match queue.TryDequeue() with
                    | false, _, _ -> ()
                    | true, current, itemCost ->
                        options.dequeuing (current, itemCost)

                        let item = current.item

                        use enumerator = search.Search.GetDecisions item |> Seq.getEnumerator

                        while not finished && Seq.moveNext enumerator do
                            let searchDecision = enumerator.Current
                            let nextItem = searchDecision.item

                            if not (memo.Add nextItem) then
                                options.repeating item
                            else
                                let decision = searchDecision.decision

                                let decisionCost = search.GetDecisionCost decision

                                let totalCost =
                                    search.Cost.AddCosts itemCost decisionCost

                                let isSolution = search.Search.IsSolutionItem nextItem

                                let node =
                                    { item = nextItem
                                      cost = totalCost
                                      isSolution = isSolution
                                      parent = ValueSome { node = current; decision = decision } }

                                if options.yieldNode node then yield node

                                queue.Enqueue(node, itemCost)
                }

            yield! insertSources ()
            yield! loop ()
        }

    let tryFindBest<'item, 'decision, 'cost> (search: IPrioritySearch<'item, 'decision, 'cost>) =
        search
        |> debugFind
            {
                yieldNode = fun node -> node.isSolution
                continueAfter = fun node -> not node.isSolution
                repeating = ignore
                enqueuing = ignore
                dequeuing = ignore
            }
        |> Seq.map buildSearchPath
        |> Seq.tryHead
    //let tryFindBest<'item, 'decision, 'cost> (search: IPrioritySearch<'item, 'decision, 'cost>) =
    //    let queue =
    //        PriorityQueue<SearchNode<'item, 'decision, 'cost>, 'cost>(search.Cost.Comparer)

    //    let memo = HashSet<'item>(search.ItemComparer)

    //    let insertSources () : SearchNode<'item, 'decision, 'cost> voption =
    //        search.Search.GetSourceItems()
    //        |> Seq.foldCond
    //            (fun _ item ->
    //                if memo.Add item then
    //                    let itemCost = search.GetItemCost item
    //                    let isSolution = search.Search.IsSolutionItem item

    //                    let node =
    //                        { item = item
    //                          cost = itemCost
    //                          isSolution = isSolution
    //                          parent = ValueNone }

    //                    if isSolution then
    //                        ValueSome(ValueSome node), false
    //                    else
    //                        queue.Enqueue(node, itemCost)
    //                        ValueNone, true
    //                else
    //                    ValueNone, true)
    //            ValueNone

    //    let rec loop () =
    //        match queue.TryDequeue() with
    //        | false, _, _ -> ValueNone
    //        | true, current, _ ->
    //            let item = current.item
    //            let itemCost = current.cost

    //            search.Search.GetDecisions item
    //            |> Seq.foldCond
    //                (fun _ searchDecision ->
    //                    let nextItem = searchDecision.item

    //                    if memo.Add nextItem then
    //                        let decision = searchDecision.decision

    //                        let decisionCost = search.GetDecisionCost decision

    //                        let totalCost =
    //                            search.Cost.AddCosts itemCost decisionCost

    //                        let isSolution = search.Search.IsSolutionItem nextItem

    //                        let node =
    //                            { item = nextItem
    //                              cost = totalCost
    //                              isSolution = isSolution
    //                              parent = ValueSome { node = current; decision = decision } }

    //                        if search.Search.IsSolutionItem nextItem then
    //                            ValueSome(ValueSome node), false
    //                        else
    //                            queue.Enqueue(node, totalCost)
    //                            ValueNone, true
    //                    else
    //                        ValueNone, true)
    //                ValueNone

    //    insertSources ()
    //    |> ValueOption.noneWith loop
    //    |> ValueOption.map buildSearchPath
