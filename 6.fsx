open System.Text.RegularExpressions

let input = "inputs/6.txt" |> System.IO.File.ReadAllLines

let racesP1 =
  input
  |> Array.map (fun s ->
    [| for m in Regex.Matches(s, @"(\d+)") -> int64 m.Groups[1].Value |])
  |> fun a -> Array.zip a[0] a[1]

let racesP2 =
  input
  |> Array.map (fun s ->
    [| for m in Regex.Matches(s, @"(\d+)") -> m.Groups[1].Value |]
    |> String.concat ""
    |> int64)
  |> fun a -> [| a[0], a[1] |]

let run =
  Array.map (fun (time, distance) ->
    [| 0L .. time |]
    |> Array.mapi (fun i m -> m, m * (time - int64 i))
    |> Array.filter (fun (_, d) -> d > distance)
    |> Array.length)
  >> Array.reduce (*)

racesP1 |> run |> printfn "part1: %d"
racesP2 |> run |> printfn "part2: %d"
