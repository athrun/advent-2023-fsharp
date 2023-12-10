open System.Text.RegularExpressions
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

let seeds = input[0] |> Array.collect id
let passes = input[1..]
let toR start len = (start, start + len - 1L)

let seedRanges: int64[] -> (int64 * int64)[] =
  Array.windowed 2
  >> Array.mapi (fun i v -> if i % 2 = 0 then Some v else None)
  >> Array.choose id
  >> Array.map (fun i -> toR i[0] i[1])

let mapR sr dr tr =
  // maps source range to target and destination range
  let ss, se = sr
  let ts, te = tr
  let ds, de = dr

  if (ss < ts && se < ts) || (ss > te) then
    // no match
    [||], [| (ss, se) |]
  else if (ss >= ts && se <= te) then
    // fully contained:     |ts|  ss   se   |te|
    let soff, eoff = (ss - ts), (te - se)
    ([| (ts + soff, te - eoff); (ds + soff, de - eoff) |], [||])
  else if (ss < ts && se >= ts && se <= te) then
    // partially contained:  ss   |ts|    se   |te|
    let soff, eoff = 0L, (te - se)
    ([| (ts + soff, te - eoff); (ds + soff, de - eoff) |], [| (ss, ss - 1L) |])
  else if (ss >= ts && ss <= te && se >= te) then
    // partially contained: |ts|   ss    |te|   se
    let soff, eoff = (ss - ts), 0L
    ([| (ts + soff, te - eoff); (ds + soff, de - eoff) |], [| (te + 1L, se) |])
  else if (ss < ts && se > te) then
    // superset:            ss   |ts|    |te|   se
    let soff, eoff = 0L, 0L

    ([| (ts + soff, te - eoff); (ds + soff, de - eoff) |],
     [| (ss, se - 1L); (te + 1L, se) |])
  else
    failwith "match5: should not happen!"

let lowestLocation (passes: int64[][][]) seedRanges =
  let mutable queue = Queue<int64 * int64>()
  let mutable remappedItems = Queue<(int64 * int64)>()

  seedRanges |> Array.iter (fun r -> queue.Enqueue(r))

  for pass in passes do
    while queue.Count > 0 do

      let sr = queue.Dequeue()
      let results = pass |> Array.map (fun m -> mapR sr (toR m[0] m[2]) (toR m[1] m[2]))

      let matched =
        results
        |> Array.filter (fun (m, _) -> m.Length > 0)
        |> Array.map (fun (m, _) -> m)

      let unmatched =
        results
        |> Array.filter (fun (_, um) -> um.Length > 0)
        |> Array.map (fun (_, um) -> um)
        |> Array.collect id
        |> Array.distinct
        |> Array.filter (fun u ->
          // remove all umatched results that are already contained
          // in subsets of the matched results.
          matched |> Array.forall (fun m -> mapR u m[0] m[0] |> fst |> Array.length = 0))

      let final = matched |> Array.map (fun m -> m[1]) |> Array.append unmatched
      final |> Array.iter (fun r -> remappedItems.Enqueue(r))

    queue <- remappedItems
    remappedItems <- Queue()

  queue.ToArray() |> Array.map (fun (start, _) -> start) |> Array.min

seeds
|> Array.map (fun s -> [| s; 1 |])
|> Array.collect id
|> seedRanges
|> lowestLocation passes
|> printfn "part1: %d"

seeds |> seedRanges |> lowestLocation passes |> printfn "part2: %d"
