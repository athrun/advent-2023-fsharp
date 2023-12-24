let input =
  "inputs/14.txt"
  |> System.IO.File.ReadAllLines
  |> Array.map (fun s -> s.ToCharArray())

let tilt col =
  let mutable l = []
  let mutable i = []

  for c in col do
    if c = 'O' then
      l <- c :: l
    else if c = '.' then
      i <- c :: i
    else
      l <- c :: i @ l
      i <- []

  l <- i @ l
  l |> List.rev |> Array.ofList

let load =
  Array.map (fun row -> row |> Array.sumBy (fun c -> if c = 'O' then 1 else 0))
  >> Array.mapi (fun i c -> (input[0].Length - i) * c)
  >> Array.sum

input
|> Array.transpose
|> Array.map tilt
|> Array.transpose
|> Array.map (fun row -> row |> Array.sumBy (fun c -> if c = 'O' then 1 else 0))
|> Array.mapi (fun i c -> (input[0].Length - i) * c)
|> Array.sum
|> printfn "part1: %d"

let cycle a =
  a
  |> Array.transpose // facing north
  |> Array.map tilt
  |> Array.transpose // facing west (reset)
  |> Array.map tilt
  |> Array.transpose
  |> Array.map Array.rev // facing south
  |> Array.map tilt
  |> Array.map Array.rev
  |> Array.transpose // facing west (reset)
  |> Array.map Array.rev // facing east
  |> Array.map tilt
  |> Array.map Array.rev // facing west (reset)

let findLoop target input =
  let d = System.Collections.Generic.Dictionary<string, int * int>()
  let mutable last = input

  Seq.initInfinite id
  |> Seq.pick (fun i ->
    last <- cycle last
    let key = last |> Array.collect id |> Array.map string |> String.concat ""

    if d.ContainsKey key then
      // found loop. it starts at `offset` and repeats every `nth`.
      let offset = fst (d[key])
      let nth = i - fst (d[key])
      // the value at the target iteration is
      // offset + the modulo of the repeating loop
      // minus 1 because of 0 indexing
      let pos = offset + (target - offset) % nth - 1
      d.Values |> Seq.item pos |> snd |> Some
    else
      d[key] <- i, (load last)
      None)

findLoop 1_000_000_000 input |> printfn "part2: %d"
