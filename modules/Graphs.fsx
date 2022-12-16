open System.Collections.Generic

// Implementation of graph search algorithms

// Glossary:
// - node: a vertex in the graph
// - edge: a connection between two nodes
// - node label: a label on a node. It can be a string, a tuple, a record, or any indication of a position in a search space
// - edge label: a label on an edge. It can represent the decision that was made to get from one node to another
// - state: the state of the search. It can be a number, a record, or any indication of the progress of the search
// - search node: a node in the search space, with a state, representing a state of the search algorithm
// - search edge: an edge in the search space, with a state, representing transition of the search algorithm
// - search path item: a node in the search path, with a node label and a state, and an optional arrival edge label and state
// - start nodes: the nodes in the search space that the search algorithm starts from
// - is solution: indicates whether a search node is a solution
// - state comparer: a comparer that compares the states of two search nodes. The LOWER the cost of the state, the BETTER.
// - node comparer: a comparer that compares the node labels of two search nodes. Allows to keep track of the best cost for each node label.
// - next: a function that returns the search edges that can be reached from a search node.
// The cost of a search MUST be monotonic, i.e. the cost of a search edge must be greater than or equal to the cost of the search node it comes from.
// Otherwise, the search algorithm will fail with an exception.

type SearchNode<'nodeLabel, 'state> =
    { node: 'nodeLabel
      state: 'state
      isSolution: bool }

type SearchEdge<'nodeLabel, 'edgeLabel, 'state> =
    { edgeLabel: 'edgeLabel
      target: 'nodeLabel
      isSolution: bool
      state: 'state }

type SearchPathNode<'nodeLabel, 'edgeLabel, 'state> =
    { current: SearchNode<'nodeLabel, 'state>
      previous: {| edgeLabel: 'edgeLabel
                   pathNode: SearchPathNode<'nodeLabel, 'edgeLabel, 'state> |} option }

type SearchPathItem<'nodeLabel, 'edgeLabel, 'state> =
    { node: 'nodeLabel
      state: 'state
      edge: ('edgeLabel * 'state) option }

type SearchOptions<'nodeLabel, 'edgeLabel, 'state> =
    { startNodes: SearchNode<'nodeLabel, 'state> list
      stateComparer: IComparer<'state>
      stateCost: 'state -> string
      nodeComparer: IEqualityComparer<'nodeLabel>
      next: SearchNode<'nodeLabel, 'state> -> SearchEdge<'nodeLabel, 'edgeLabel, 'state> seq }

module Seq =
    let sortByComparer (comparer: IComparer<_>) (keySelector: _ -> _) (source: _ seq) : _ seq =
        System.Linq.Enumerable.OrderBy(source, keySelector, comparer)

/// Performs a graph search.
/// Returns a secuence of steps of the search algorithm.
/// On each step of the sequence, a SearchPathNode and whether the search is finished are returned.
/// The resulting sequence is lazy, and can return as many final paths as found (possibly infinite).
/// Use functino aStar to get just the first path found.
/// Use function buildAStarPath to build a path from a SearchPathNode.
let aStarSeq options : SearchPathNode<'nodeLabel, 'edgeLabel, 'state> seq =
    seq {
        let pending = PriorityQueue(options.stateComparer)
        let visited = Dictionary(options.nodeComparer)

        for startNode in options.startNodes do
            let pathNode = { current = startNode; previous = None }
            pending.Enqueue(pathNode, startNode.state)
            visited.[startNode.node] <- pathNode

        let includeNode current edge =
            let pathNode =
                { current =
                    { node = edge.target
                      state = edge.state
                      isSolution = edge.isSolution }
                  previous =
                    Some
                        {| edgeLabel = edge.edgeLabel
                           pathNode = current |} }

            pending.Enqueue(pathNode, edge.state)
            visited.[edge.target] <- pathNode

        while pending.Count > 0 do
            let current = pending.Dequeue()
            yield current

            for edge in options.next current.current do
                let isWorseThanCurrent = options.stateComparer.Compare(edge.state, current.current.state) > 0
                if isWorseThanCurrent then
                    failwithf "The cost of a search edge %A [%s] must be greater than or equal to the cost of the search node %A [%s] it comes from." edge.edgeLabel (options.stateCost edge.state) current.current.node (options.stateCost current.current.state)
                match visited.TryGetValue(edge.target) with
                | true, previous ->
                    let isBetterState =
                        options.stateComparer.Compare(edge.state, previous.current.state) < 0

                    if isBetterState then
                        includeNode current edge

                | false, _ -> includeNode current edge
    }

/// Performs a graph search.
/// Returns the first path found, or None if no path was found.
/// Use function aStarSeq to get all paths found.
/// Use function buildAStarPath to build a path from a SearchPathNode.
let aStar options =
    aStarSeq options
    |> Seq.filter (fun pathNode -> pathNode.current.isSolution)
    |> Seq.tryHead

/// Builds a path from a SearchPathNode.
let buildAStarPath pathNode : SearchPathItem<'nodeLabel, 'edgeLabel, 'state> list =
    let rec build pathNode =
        match pathNode.previous with
        | Some previous ->
            let item =
                { node = pathNode.current.node
                  state = pathNode.current.state
                  edge = Some (previous.edgeLabel, previous.pathNode.current.state) }

            item :: build previous.pathNode

        | None ->
            let item =
                { node = pathNode.current.node
                  state = pathNode.current.state
                  edge = None }

            [ item ]

    build pathNode
    |> List.rev
