open System.Text.RegularExpressions

let input = "inputs/8.txt" |> System.IO.File.ReadAllLines
let path = input[0].ToCharArray()

let network =
  input[2..]
  |> Array.map (fun s ->
    [| for m in Regex.Matches(s, @"(\w\w\w)") -> m.Groups[1].Value |])
  |> Array.map (fun a -> (a[0], (a[1], a[2])))
  |> Map.ofArray

let inline next current step =
  let dest = network[current]
  if path[step] = 'L' then fst dest else snd dest

let travel start cond =
  let mutable i = 0
  let mutable cur = start

  while not (cond cur) do
    cur <- next cur (i % path.Length)
    i <- i + 1

  i

let inline lcm a b =
  (a * b) / System.Numerics.BigInteger.GreatestCommonDivisor(a, b)

travel "AAA" (fun s -> s = "ZZZ") |> printfn "part1: %A"

// Given cycle lengths repeat themselves and are aligned,
// a LCM will give us the answer for part2.
network.Keys
|> Seq.filter (fun k -> k.EndsWith("A"))
|> Seq.toArray
|> Array.map (fun v -> travel v (fun s -> s.EndsWith("Z")))
|> Array.map bigint
|> Array.reduce lcm
|> printfn "part2: %O"
