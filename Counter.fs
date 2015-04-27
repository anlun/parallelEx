module Counter

open System.Threading

let sumInRange (arr : int []) l r : int =
  let mutable res = 0
  for i in l .. r do
    res <- res + arr.[i]
  res

let sumOnes threadNumber arraySize : int =
  let arr = Array.init arraySize (fun _ -> 1)
  let res = ref 0
  let step = arraySize / threadNumber
  let threadArray = Array.init threadNumber (fun i ->
      new Thread(ThreadStart(fun _ ->
          let threadRes = sumInRange arr (i * step) ((i+1) * step - 1)
          res := res.Value + threadRes
        ))
    )
  for t in threadArray do
    t.Start()
  for t in threadArray do
    t.Join()
  res.Value

let sumInRange_bad (arr : int []) l r res =
  for i in l .. r do
    res := res.Value + arr.[i]

let sumOnes_bad threadNumber arraySize : int =
  let arr = Array.init arraySize (fun _ -> 1)
  let res = ref 0
  let step = arraySize / threadNumber
  let threadArray = Array.init threadNumber (fun i ->
      new Thread(ThreadStart(fun _ ->
          sumInRange_bad arr
           (i * step) ((i+1) * step - 1)
           res
        ))
    )
  for t in threadArray do
    t.Start()
  for t in threadArray do
    t.Join()
  res.Value