open System.Collections.Generic

type DijkstraValuedCell<'cell, 'value> = { cell: 'cell; value: 'value }

type DijkstraNode<'cell, 'value> =
    { info: DijkstraValuedCell<'cell, 'value>
      previous: 'cell option }

type DijkstraOptions<'cell, 'value> =
    { startCells: DijkstraValuedCell<'cell, 'value> list
      isEndCell: 'cell -> bool
      valueComparer: IComparer<'value>
      cellComparer: IEqualityComparer<'cell>
      terrainFn: DijkstraValuedCell<'cell, 'value> -> DijkstraValuedCell<'cell, 'value> list
      debug: (DijkstraNode<'cell, 'value> option -> DijkstraNode<'cell, 'value> list -> unit) option }

let listSortByComparer (comparer: IComparer<_>) (keySelector: _ -> _) list =
    System.Linq.Enumerable.OrderBy(list, keySelector, comparer)
    |> Seq.toList

let dijkstra options =
    let pending = PriorityQueue(options.valueComparer) // Better use a PriorityQueue here
    let visited = Dictionary(options.cellComparer)

    options.startCells
    |> Seq.iter (fun info ->
        let node = { info = info; previous = None }

        pending.Enqueue(node, info.value)
        visited.Add(info.cell, node))

    let doDebug node =
        match options.debug with
        | Some debug -> debug node (visited.Values |> Seq.toList)
        | None -> ()

    let rec run () =
        match pending.TryDequeue() with
        | false, _, _ ->
            doDebug None
            None
        | true, node, _ ->
            doDebug (Some node)

            if options.isEndCell node.info.cell then
                let rec loop node =
                    match node.previous with
                    | Some previous ->
                        let previousNode = visited.[previous]
                        node.info :: loop previousNode
                    | None -> [ node.info ]

                Some(loop node |> List.rev)

            else
                let newCells =
                    options.terrainFn node.info
                    |> listSortByComparer options.valueComparer (fun cell -> cell.value)

                for newCell in newCells do
                    let addNewCell () =
                        let newNode =
                            { info = newCell
                              previous = Some node.info.cell }

                        pending.Enqueue(newNode, newNode.info.value)
                        visited.[newCell.cell] <- newNode

                    match visited.TryGetValue(newCell.cell) with
                    | true, oldNode ->
                        match options.valueComparer.Compare(newCell.value, oldNode.info.value) < 0 with
                        | true -> addNewCell ()
                        | false -> ()
                    | false, _ -> addNewCell ()

                run ()

    run ()
