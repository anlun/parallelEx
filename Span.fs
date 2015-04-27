open System.Threading

type BiGraph (l : Option<BiGraph>, r : Option<BiGraph>) =
  member val L = l with get, set
  member val R = r with get, set
  new()  = new BiGraph(None, None)
  new(l) = new BiGraph(l, None)

  member val marked = false with get, set
  member this.fL = Option.get this.L
  member this.fR = Option.get this.R

let g =
  let d = new BiGraph()
  let e = new BiGraph()
  let b = new BiGraph(Some d, Some e)
  let c = new BiGraph(Some e)
  c.R <- Some c
  new BiGraph(Some b, Some c)

let rec span(x : Option<BiGraph>) : bool =
  match x with
  | None   -> false
  | Some g ->
    if g.marked then false
    else
      g.marked <- true
      let lRes = span(g.L)
      let rRes = span(g.R)
      if not lRes then g.L <- None
      if not rRes then g.R <- None
      true

let rec pSpan(x : Option<BiGraph>) : bool =
  match x with
  | None   -> false
  | Some g ->
    let alreadyMarked = ref false
    lock g (fun () ->
      alreadyMarked := g.marked
      g.marked <- true
    )
    if alreadyMarked.Value then false
    else
      let lThread = new Thread(ThreadStart(fun _ ->
          let lRes = pSpan(g.L)
          if not lRes then g.L <- None
        ))
      lThread.Start()
      let rRes = pSpan(g.R)
      lThread.Join()
      if not rRes then g.R <- None
      true
