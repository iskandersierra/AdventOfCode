#load "Preamble.fsx"

open System.Collections.Generic
open Preamble

// Implementation of graph search algorithms

type SearchNode<'state> = { state: 'state; isSolution: bool }

type SearchEdge<'state, 'decision> =
    { state: 'state
      decision: 'decision
      isSolution: bool }

type SearchPathNode<'state, 'decision> =
    { current: SearchNode<'state>
      previous: {| decision: 'decision
                   node: SearchPathNode<'state, 'decision> |} option }

type SearchPathItem<'state, 'decision> =
    { state: 'state
      isSolution: bool
      decision: 'decision option }

/// Finds the first solution found on a search sequence.
/// Only use if the greedy search is guaranteed to find the best solution first.
let findFirstSolution searchResult =
    searchResult
    |> Seq.filter (fun node -> node.current.isSolution)
    |> Seq.tryHead

/// Finds the best solution in a search result, using the given comparer to compare states.
/// Only use if the search space is finite and relativelly small.
/// In such cases, the `next` function should be optimized to limit the search space.
let findBestSolution stateCostComparer searchResult =
    searchResult
    |> Seq.filter (fun node -> node.current.isSolution)
    |> Seq.tryMinBy stateCostComparer (fun node -> node.current.state)

/// Builds a path from a SearchPathNode.
let buildPathItems pathNode : SearchPathItem<'state, 'decision> list =
    let rec build pathNode =
        match pathNode.previous with
        | Some previous ->
            let item =
                { isSolution = pathNode.current.isSolution
                  state = pathNode.current.state
                  decision = Some previous.decision }

            item :: build previous.node

        | None ->
            let item =
                { isSolution = pathNode.current.isSolution
                  state = pathNode.current.state
                  decision = None }

            [ item ]

    build pathNode |> List.rev

/// Memoized searches. Use equality and hash over states to avoid infinite loops.

type SearchMemoOptions<'state, 'decision> =
    { startNodes: SearchNode<'state> seq
      stateCostComparer: 'state -> 'state -> int
      stateEquals: 'state -> 'state -> bool
      stateHash: 'state -> int
      next: SearchNode<'state> -> SearchEdge<'state, 'decision> seq }

// depthFirstSearchMemoSeq performs a Depth First Search on a search space
// Given that the search space could contain repeated states, a set of all visited states is memoized while searching, to avoid infinite loops
// This is a lazy sequence, so it can return as many final paths as found (possibly infinite).
// Depth first search does not use the cost of the search nodes.
let depthFirstSearchMemoSeq (options: SearchMemoOptions<'state, 'decision>) =
    seq {
        let visited =
            Dictionary<'state, SearchPathNode<'state, 'decision>>(
                EqualityComparer.ofFun options.stateEquals options.stateHash
            )

        let stack =
            Stack<SearchPathNode<'state, 'decision>>()

        for node in options.startNodes do
            let pathNode = { current = node; previous = None }
            stack.Push pathNode

        while stack.Count > 0 do
            let pathNode = stack.Pop()

            if not (visited.ContainsKey pathNode.current.state) then
                visited.Add(pathNode.current.state, pathNode)

                yield pathNode

                let edges = options.next pathNode.current

                for edge in edges do
                    let node' =
                        { state = edge.state
                          isSolution = edge.isSolution }

                    let pathNode' =
                        { current = node'
                          previous =
                            Some
                                {| decision = edge.decision
                                   node = pathNode |} }

                    stack.Push pathNode'
    }

// breathFirstSearchMemoSeq performs a Depth First Search on a search space
// Given that the search space could contain repeated states, a set of all visited states is memoized while searching, to avoid infinite loops
// This is a lazy sequence, so it can return as many final paths as found (possibly infinite).
// Breath first search does not use the cost of the search nodes.
let breadthFirstSearchMemoSeq (options: SearchMemoOptions<'state, 'decision>) =
    seq {
        let visited =
            Dictionary<'state, SearchPathNode<'state, 'decision>>(
                EqualityComparer.ofFun options.stateEquals options.stateHash
            )

        let queue =
            Queue<SearchPathNode<'state, 'decision>>()

        for node in options.startNodes do
            let pathNode = { current = node; previous = None }
            queue.Enqueue pathNode

        while queue.Count > 0 do
            let pathNode = queue.Dequeue()

            if not (visited.ContainsKey pathNode.current.state) then
                visited.Add(pathNode.current.state, pathNode)

                yield pathNode

                let edges = options.next pathNode.current

                for edge in edges do
                    let node' =
                        { state = edge.state
                          isSolution = edge.isSolution }

                    let pathNode' =
                        { current = node'
                          previous =
                            Some
                                {| decision = edge.decision
                                   node = pathNode |} }

                    queue.Enqueue pathNode'
    }

// aStarSearchMemoSeq performs a A* Search on a search space
// Given that the search space could contain repeated states, a set of all visited states is memoized while searching, to avoid infinite loops
// This is a lazy sequence, so it can return as many final paths as found (possibly infinite).
// Breath first search does not use the cost of the search nodes.
let aStarSearchMemoSeq (options: SearchMemoOptions<'state, 'decision>) =
    seq {
        let visited =
            Dictionary<'state, SearchPathNode<'state, 'decision>>(
                EqualityComparer.ofFun options.stateEquals options.stateHash
            )

        let queue =
            PriorityQueue<SearchPathNode<'state, 'decision>, 'state>(Comparer.ofFun options.stateCostComparer)

        for node in options.startNodes do
            let pathNode = { current = node; previous = None }
            queue.Enqueue(pathNode, pathNode.current.state)

        while queue.Count > 0 do
            let pathNode = queue.Dequeue()

            if not (visited.ContainsKey pathNode.current.state) then
                visited.Add(pathNode.current.state, pathNode)

                yield pathNode

                let edges = options.next pathNode.current

                for edge in edges do
                    let node' =
                        { state = edge.state
                          isSolution = edge.isSolution }

                    let pathNode' =
                        { current = node'
                          previous =
                            Some
                                {| decision = edge.decision
                                   node = pathNode |} }

                    queue.Enqueue(pathNode, pathNode'.current.state)
    }
