open System
open System.Collections.Generic

let input =
  "inputs/20.txt"
  |> System.IO.File.ReadAllLines
  |> Array.map (fun s ->
    let sp = s.Split(" -> ")

    let typ, label =
      if sp[0][0] = '%' || sp[0][0] = '&' then
        sp[0][1..], sp[0][0]
      else
        sp[0], '-'

    typ, label, sp[1].Split(", "))

type Widgets = Dictionary<string, char * string[] * string[]>
type States = Dictionary<string, bool>

let widgets =
  let d: Widgets = Dictionary()
  Array.iter (fun (label, typ, outputs) -> d[label] <- typ, outputs, [||]) input

  let mutable l = [] // dummy module lists

  for k in d.Keys do
    let _, outputs, _ = d[k]

    outputs
    |> Array.iter (fun out ->
      if d.ContainsKey out then
        let ltyp, loutputs, linputs = d[out]
        d[out] <- (ltyp, loutputs, (Array.append [| k |] linputs |> Array.distinct))
      else // dummy module with no outputs
        l <- (out, '-', [||], [| k |]) :: l)

  l
  |> List.iter (fun (label, typ, outputs, inputs) -> d[label] <- typ, outputs, inputs)

  d

let trd (_, _, x) = x
let agg (l, h) (l', h') = l + l', h + h'
let mul (a, b) = a * b

let inline lcm a b =
  (a * b) / System.Numerics.BigInteger.GreatestCommonDivisor(a, b)

let propagate iter (widgets: Widgets) (states: States) ssrc slabel slevel targets =
  let mutable acc = 0L, 0L // low, high
  let q = Queue()
  let mutable cycles = []

  q.Enqueue(ssrc, slabel, slevel)

  while q.Count > 0 do
    let src, label, level = q.Dequeue()
    let typ, outputs, inputs = widgets[label]

    if not level then
      acc <- agg acc (1L, 0L)
    else
      acc <- agg acc (0, 1)

    if typ = '%' && not level then
      // flip flop on low only
      let state = if states.ContainsKey label then not states[label] else true
      states[label] <- state
    else if typ = '&' then
      states[$"{label}&{src}"] <- level

    for out in outputs do
      match typ with
      | '-' -> q.Enqueue(label, out, level) // passthrough
      | '%' when not level ->
        let state = states[label]
        q.Enqueue(label, out, state) // send low when off, high when on
      | '&' ->
        let all =
          Array.forall
            (fun s ->
              let k = $"{label}&{s}"
              if states.ContainsKey k then states[k] else false)
            inputs

        if all then // send low pulse when all high
          q.Enqueue(label, out, false)
        else
          if Array.exists (fun tgt -> tgt = label) targets then
            cycles <- (label, iter) :: cycles

          q.Enqueue(label, out, true)
      | _ -> () // ignore the rest

  acc, cycles

let simulate times =
  let mutable acc = 0L, 0L // low, high
  let states: States = Dictionary()
  let mutable cycles = []
  let mutable i = 1L
  let mutable completed = false
  let targets = (trd widgets[(trd widgets["rx"])[0]])

  while i <= times && not completed do

    let count, c = (propagate i widgets states "button" "broadcaster" false targets)
    acc <- agg acc count

    if not c.IsEmpty then
      cycles <- c @ cycles

    if cycles.Length = targets.Length then
      completed <- true

    i <- i + 1L

  mul acc, cycles

simulate 1_000 |> fun (acc, _) -> printfn "part1: %d" acc

simulate 10_000
|> fun (_, cycles) ->
  cycles
  |> List.fold (fun acc (_, n) -> lcm acc (bigint n)) 1I
  |> printfn "part2: %O"
