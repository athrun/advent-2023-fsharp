open System
open System.Collections.Generic
open System.Text.RegularExpressions

let input =
  "inputs/19.txt"
  |> System.IO.File.ReadAllText
  |> fun s -> Regex.Split(s, "^$", RegexOptions.Multiline)
  |> Array.map (fun chunk ->
    chunk.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries))

type Rules = Dictionary<string, (int * char * int * string)[] * string>

let buildRules input =
  let d: Rules = Dictionary()

  Array.iter
    (fun s ->
      for m in Regex.Matches(s, @"(?<label>\w+){(?<cond>.+),(?<action>\w+)}") do
        let conds =
          m.Groups["cond"].Value.Split(',')
          |> Array.map (fun s ->
            let ps = s.Split(":")
            let act = ps[1]

            let att =
              match ps[0][0] with
              | 'x' -> 0
              | 'm' -> 1
              | 'a' -> 2
              | 's' -> 3
              | _ -> failwith "bad att"

            let tgt = int (ps[0][2..])
            let op = ps[0][1]

            att, op, tgt, act)

        d[m.Groups["label"].Value] <- conds, m.Groups["action"].Value)
    input

  d

let rules = buildRules input[0]

let parts =
  input[1]
  |> Array.map (fun s -> [| for m in Regex.Matches(s, @"(\d+)") -> int m.Value |])

let rec workflow (part: int[]) (rules: Rules) (conds, action) =
  let r =
    Array.tryPick
      (fun (att, op, tgt, act) ->
        let cmp = if op = '>' then (>) else (<)
        if cmp part[att] tgt then Some act else None)
      conds

  match r with
  | None when action = "A" -> Some action // defer to last action in the block
  | None when action <> "R" -> workflow part rules rules[action] // action is a label
  | Some v when v = "A" -> Some v
  | Some v when v <> "R" -> workflow part rules rules[v]
  | _ -> None

let accepted parts rules =
  parts
  |> Array.map (fun p -> Option.map (fun _ -> p) (workflow p rules rules["in"]))
  |> Array.choose id
  |> Array.collect id
  |> Array.sum

let combinations part =
  part
  |> Array.map (fun (s, e) -> e - s + 1)
  |> Array.map int64
  |> Array.reduce (*)

let rangeMatch (start, finish) cond tgt =
  if cond = '>' then
    if start > tgt then
      // range is fully > target
      Some(start, finish), None
    else if start <= tgt && finish > tgt then
      // range is partially > target
      Some(tgt + 1, finish), Some(start, tgt)
    else
      // range is fully <= target
      None, Some(start, finish)
  else if finish < tgt then
    // range is fully < target
    Some(start, finish), None
  else if finish >= tgt && start < tgt then
    // range is partially < target
    Some(start, tgt - 1), Some(tgt, finish)
  else
    // range is fully >= target
    None, Some(start, finish)

let rec dfs (rules: Rules) (conds, action) (part: (int * int)[]) =
  let mutable acc = 0L
  let mutable lpart = Array.copy part
  let mutable completed = false

  for (dim, op, tgt, act) in conds do
    if not completed then

      match rangeMatch lpart[dim] op tgt with
      | Some(s, e), None -> // full match
        let npart = Array.copy lpart
        npart[dim] <- (s, e)
        completed <- true

        if act = "A" then acc <- acc + (combinations npart)
        else if act = "R" then ()
        else acc <- acc + (dfs rules rules[act] npart)
      | Some(s, e), Some(us, ue) -> // partial match
        let npart = Array.copy lpart
        lpart[dim] <- (us, ue)
        npart[dim] <- (s, e)

        if act = "A" then acc <- acc + (combinations npart)
        else if act = "R" then ()
        else acc <- acc + (dfs rules rules[act] npart)
      | _ -> () // no match

  if not completed then // execute final action

    if action = "A" then acc <- acc + (combinations lpart)
    else if action = "R" then ()
    else acc <- acc + (dfs rules rules[action] lpart)

  acc

accepted parts rules |> printfn "part1: %d"

dfs rules rules["in"] [| 1, 4000; 1, 4000; 1, 4000; 1, 4000 |]
|> printfn "part2: %d"
