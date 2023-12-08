open System.Text.RegularExpressions
open Checked
open System.Collections.Generic

let input =
  "inputs/5.txt"
  |> System.IO.File.ReadAllText
  |> fun txt -> Regex.Split(txt, @"^$", RegexOptions.Multiline)
  |> Array.map (fun seg ->
    seg.Split(System.Environment.NewLine)
    |> Array.map (fun l ->
      [| for m in Regex.Matches(l, @"(\d+)") -> int64 m.Groups[1].Value |])
    |> Array.filter (fun a -> not (Array.isEmpty a)))
  |> Array.filter (fun a -> not (Array.isEmpty a))

let nextVal value destR srcR lenR =
  if value >= srcR && value <= srcR + lenR - 1L then
    Some(destR + (srcR - value) * -1L)
  else
    None

let seeds = input[0] |> Array.collect id
let maps = input[1..]

seeds
|> Array.map (fun seed ->
  let mutable mappedVal = seed

  maps
  |> Array.iter (fun ms ->
    let r =
      ms |> Array.map (fun m -> nextVal mappedVal m[0] m[1] m[2]) |> Array.choose id

    match r with
    | [||] -> ()
    | [| v |] -> mappedVal <- v
    | _ -> printfn "More than one match for the seed!")

  mappedVal)
|> Array.min // part1

//
// ----------------------------------------------------
// The solution for Part 2 is still a work in progess
// It works but it's clunky and i'm not even sure
// I fully understand how it can produce the right result...
// I'll need to revisit it when I have time later
// ----------------------------------------------------
//

let toR s l = (s, s + l - 1L)

let seedRanges =
  seeds
  |> Array.windowed 2
  |> Array.mapi (fun i v -> if i % 2 = 0 then Some v else None)
  |> Array.choose id
  |> Array.map (fun i -> toR i[0] i[1])

let isInRange sr dr tr =
  let ss, se = sr
  let ts, te = tr
  let ds, de = dr

  if (ss < ts && se < ts) || (ss > te) then
    //pintfn $"match0: no match {(ss, se)} into {(ts, te)}. New dest: {(ds, de)}"
    [| [||]; [| (ss, se) |] |]
  else if (ss >= ts && se <= te) then
    //printfn $"match1: full match {(ss, se)} into {(ts, te)}. New dest: {(ds, de)}"
    let sOffset = (ss - ts)
    let eOffset = (te - se)
    let nss = ds + sOffset
    let nse = de - eOffset
    //printfn $"match1: {(ss, se)} -> {(nss, nse)}"
    [| [| (nss, nse) |]; [||] |]
  else if (ss < ts && se >= ts && se <= te) then
    //printfn $"match2: partial match {(ss, se)} into {(ts, te)}. New dest: {(ds, de)}"
    let sOffset = 0L
    let eOffset = (te - se)
    let nss = ds + sOffset
    let nse = de - eOffset
    //printfn $"match2: {(ss, se)} -> {(nss, nse)}"
    [| [| (nss, nse) |]; [| (ss, ss - 1L) |] |]
  else if (ss >= ts && ss <= te && se >= te) then
    printfn $"match3: partial match {(ss, se)} into {(ts, te)}. New dest: {(ds, de)}"
    let sOffset = (ss - ts)
    let eOffset = 0L
    let nss = ds + sOffset
    let nse = de - eOffset
    let res = [| [| (nss, nse) |]; [| (te + 1L, se) |] |]
    printfn "match3: %A" res
    res
  else if (ss < ts && se > te) then
    printfn $"match4: partial match {(ss, se)} into {(ts, te)}. New dest: {(ds, de)}"
    let sOffset = 0L
    let eOffset = 0L
    let nss = ds + sOffset
    let nse = de - eOffset
    let res = [| [| (nss, nse) |]; [| (ss, se - 1L); (te + 1L, se) |] |]
    printfn "match4: %A" res
    res
  else
    failwith "match5: should not happen!"
    [| [||] |]

let enqueueOrExtend s (q: Queue<int64 * int64>) =
  let mutable extended = false

  let extendedQ =
    q.ToArray()
    |> Array.map (fun (ts, te) ->
      let ss, se = s

      if (ss < ts && se < ts) || (ss > te) then
        // 0 no match
        ts, te
      else if ss >= ts && se <= te then
        // 1 full match
        extended <- true
        ts, te
      else if (ss < ts && se >= ts && se <= te) then
        // 2 partial match
        extended <- true
        ss, te
      else if (ss >= ts && ss <= te && se >= te) then
        // 3 partial match
        extended <- true
        ts, se
      else if (ss < ts && se > te) then
        // 4 partial match
        extended <- true
        ss, se
      else
        failwith "shouldn't happen!")

  if extended then
    printfn "Extended existing ranges in queue to include range: %A" s
    q.Clear()
    extendedQ |> Array.iter (fun i -> q.Enqueue(i))
  else
    printfn "No ranges in the queue can be extended to include range: %A. Enqueuing" s
    q.Enqueue(s)

let mutable currentPassQueue = Queue<int64 * int64>()
let mutable nextPassQueue = Queue<int64 * int64>()

currentPassQueue <- Queue()
nextPassQueue <- Queue()

for r in seedRanges do
  currentPassQueue.Enqueue(r)

maps
|> Array.iteri (fun passi pass ->
  printfn "Starting Pass %i: %A" passi (currentPassQueue.ToArray())

  let mutable maxiter = 100

  while currentPassQueue.Count > 0 && maxiter > 0 do
    printfn "-------------"
    printfn $"Queue size: {currentPassQueue.Count}"
    printfn "-------------"
    printfn "%A}" (currentPassQueue.ToArray() |> Array.sort)
    printfn "-------------"

    let sr = currentPassQueue.Dequeue()

    let results =
      pass |> Array.map (fun m -> isInRange sr (toR m[0] m[2]) (toR m[1] m[2]))

    let isNotAMatch =
      results |> Array.forall (fun i -> i[0].Length = 0 && i[1].Length > 0)

    if isNotAMatch then
      // no match across any ranges so the seed range
      // should be left untouched and placed into the nextpassqueue
      //printfn $"Pass: {passi}. No match for seed range {sr}. Leaving it untouched."
      nextPassQueue |> enqueueOrExtend sr
    else
      // there was a match across some ranges
      // putting the matched portion into the nextpassqueue
      // and putting the unmatched portion(s) into the currentpassqueue
      // note: this is probably where the problem is with duplicates
      // it might be good to try checking if the matched portion is already in the
      // queue and if so, skip processing the unmatched ones completely.
      (* this approach doesn't really work...*)
      let firstMatch =
        results |> Array.tryFind (fun i -> i.Length > 0 && i[0].Length > 0)

      firstMatch
      |> Option.iter (fun i ->
        // there's a matched portion
        printfn $"Pass: {passi}. Matched portion for seed range {sr} -> {i[0][0]}."
        nextPassQueue |> enqueueOrExtend (i[0][0])

        if i[1].Length > 0 then
          // there's unmatched portion(s)
          i[1]
          |> Array.iter (fun u ->
            printfn $"Pass: {passi}. Unmatched portion for seed range {sr} -> {u}."
            currentPassQueue.Enqueue(u)))
    //currentPassQueue |> enqueueOrExtend (u)))
    (*
      results
      |> Array.iter (fun i ->
        if i.Length > 0 && i[0].Length > 0 then
          // there's a matched portion
          printfn $"Pass: {passi}. Matched portion for seed range {sr} -> {i[0][0]}."
          nextPassQueue |> enqueueOrExtend (i[0][0])

          if i[1].Length > 0 then
            // there's unmatched portion(s)
            i[1]
            |> Array.iter (fun u ->
              printfn $"Pass: {passi}. Unmatched portion for seed range {sr} -> {u}."
              currentPassQueue |> enqueueOrExtend (u)))
      *)

    maxiter <- maxiter - 1

  // set up queues for next pass
  currentPassQueue <- nextPassQueue
  nextPassQueue <- Queue())

currentPassQueue.ToArray() |> Array.map (fun (s, _) -> s) |> Array.min
