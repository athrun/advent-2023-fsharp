let input =
  "inputs/9.txt"
  |> System.IO.File.ReadAllLines
  |> Array.map (fun s -> s.Split(' ') |> Array.map int64)

let next backwards series =
  let rec loop items =
    match items with
    | [] -> []
    | head :: _ ->
      let line = head |> Array.windowed 2 |> Array.map (fun pair -> pair[1] - pair[0])

      if line |> Array.forall (fun x -> x = 0L) then
        line :: items
      else
        loop (line :: items)

  if not backwards then
    loop [ series ] |> List.fold (fun acc n -> acc + n[n.Length - 1]) 0L
  else
    loop [ series ] |> List.fold (fun acc n -> n[0] - acc) 0L

input |> Array.map (next false) |> Array.sum |> printfn "part1: %d"
input |> Array.map (next true) |> Array.sum |> printfn "part2: %d"
