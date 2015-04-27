//module QuickSort

open System.Threading

let swap (arr : int []) i j =
  let tmp = arr.[i]
  arr.[i] <- arr.[j]
  arr.[j] <- tmp

let swapping (arr : int []) l r : int * int =
  let middle = l + (r - l) / 2
  let v = arr.[middle]
  let mutable i = l
  let mutable j = r
  while (i < j) do
    while (arr.[i] < v) do i <- i + 1
    while (arr.[j] > v) do j <- j - 1
    if (i <= j)
    then
      swap arr i j      
      i <- i + 1
      j <- j - 1
  (i, j)

let rec quicksort_base (arr : int []) (l : int) (r : int) =
  let (i, j) = swapping arr l r
  if i < r then quicksort_base arr i r
  if j > l then quicksort_base arr l j

let quicksort (arr : int []) = quicksort_base arr 0 (arr.Length - 1)

let rec pQuicksort_base (threadMaxNumber : int) (arr : int []) (l : int) (r : int) =
  let (i, j) = swapping arr l r
  if i < r && j > l && threadMaxNumber > 1
  then
    let rThread = new Thread(ThreadStart(fun _ ->
        pQuicksort_base (threadMaxNumber / 2) arr i r
      ))
    rThread.Start()
    pQuicksort_base (threadMaxNumber / 2) arr l j
    rThread.Join()
  else
    if i < r then pQuicksort_base (threadMaxNumber / 2) arr i r
    if j > l then pQuicksort_base (threadMaxNumber / 2) arr l j

let rec aQuicksort_base (threadMaxNumber : int) (arr : int []) (l : int) (r : int) =
  let (i, j) = swapping arr l r
  if i < r && j > l && threadMaxNumber > 1
  then
    let lThread = async { aQuicksort_base (threadMaxNumber / 2) arr l j }
    let rThread = async { aQuicksort_base (threadMaxNumber / 2) arr i r }
    seq { yield lThread; yield rThread } |> Async.Parallel |> Async.RunSynchronously |> ignore
  else
    if i < r then aQuicksort_base (threadMaxNumber / 2) arr i r
    if j > l then aQuicksort_base (threadMaxNumber / 2) arr l j

let duration s f = 
  let timer = new System.Diagnostics.Stopwatch()
  timer.Start()
  let returnValue = f()
  printfn "Task: %s\t\t\tElapsed Time: %i" s timer.ElapsedMilliseconds
  returnValue

let pQuicksort (arr : int []) = pQuicksort_base 4 arr 0 (arr.Length - 1)
let pQuicksort_n n (arr : int []) = pQuicksort_base n arr 0 (arr.Length - 1)

let aQuicksort_n n (arr : int []) = aQuicksort_base n arr 0 (arr.Length - 1)

[<EntryPoint>]
let main (args : string []) =
  let degree = System.Int32.Parse args.[2]
  let n = pown 10 degree 
  let rnd = new System.Random(0)
  let g = Array.init n (fun i -> rnd.Next(0, n))

  let threadNum = System.Int32.Parse args.[1]
  let f = 
    if args.[0] = "a" then aQuicksort_n else pQuicksort_n
  duration (sprintf "qs p %i" threadNum) (fun () -> f threadNum g)
  0