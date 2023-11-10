namespace AocUtils.Search

open System.Collections.Generic

type ICostDescription<'cost> =
    abstract ZeroCost : 'cost
    abstract AddCosts : 'cost -> 'cost -> 'cost
    abstract Comparer : IComparer<'cost>

type ISearchDescription<'item, 'decision> =
    abstract GetSourceItems : unit -> seq<'item>
    abstract GetDecisions : 'item -> seq<struct ('item * 'decision)>
    abstract IsTargetItem : 'item -> bool

type IPrioritySearch<'item, 'decision, 'cost> =
    abstract Search : ISearchDescription<'item, 'decision>
    abstract Cost : ICostDescription<'cost>

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

    let tryFindBest<'item, 'decision, 'cost> (search: IPrioritySearch<'item, 'decision, 'cost>) =
        let queue =
            PriorityQueue<SearchNode<'item, 'decision, 'cost>, 'cost>(search.Cost.Comparer)

        let insertSources () : SearchNode<'item, 'decision, 'cost> voption =
            search.Search.GetSourceItems()
            |> Seq.foldCond
                (fun _ item ->
                    let itemCost = search.GetItemCost item

                    let node =
                        { item = item
                          cost = itemCost
                          parent = ValueNone }

                    if search.Search.IsTargetItem item then
                        ValueSome (ValueSome node), false
                    else
                        queue.Enqueue(node, itemCost)
                        ValueNone, true)
                ValueNone

        let rec loop () =
            match queue.TryDequeue() with
            | false, _, _ -> ValueNone
            | true, current, _ ->
                let item = current.item
                let itemCost = current.cost

                search.Search.GetDecisions item
                |> Seq.foldCond
                    (fun _ (struct (nextItem, decision)) ->
                        let decisionCost = search.GetDecisionCost decision
                        let totalCost = search.Cost.AddCosts itemCost decisionCost

                        let node =
                            { item = nextItem
                              cost = totalCost
                              parent = ValueSome { node = current
                                                   decision = decision } }

                        if search.Search.IsTargetItem nextItem then
                            ValueSome (ValueSome node), false
                        else
                            queue.Enqueue(node, totalCost)
                            ValueNone, true)
                    ValueNone

        insertSources ()
        |> ValueOption.noneWith loop
        |> ValueOption.map buildSearchPath
