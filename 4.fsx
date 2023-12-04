open System.Text.RegularExpressions

let input =
  "4.input.txt"
  |> System.IO.File.ReadAllLines
  |> Array.map (fun line ->
    line.Split(@"|")
    |> Array.map (fun p ->
      [| for m in Regex.Matches(p, @"(\d+)") -> int m.Groups[1].Value |]))

input
|> Array.map (fun card ->
  let mutable score = 0

  for n in card[0][1..] do
    match Array.tryFind (fun m -> m = n) card[1] with
    | Some _ -> score <- if score = 0 then 1 else score * 2
    | None -> ()

  score)
|> Array.sum // part 1

let cards =
  input
  |> Array.mapi (fun i card ->
    let mutable score = 0

    for n in card[0][1..] do
      match Array.tryFind (fun m -> m = n) card[1] with
      | Some _ -> score <- score + 1
      | None -> ()

    [| 1; yield! [| for j in i + 1 .. i + score -> j |] |])

cards
|> Array.iter (fun card ->
  for _ in 0 .. card[0] - 1 do
    for m in card[1..] do
      cards[m][0] <- cards[m][0] + 1)

cards |> Array.sumBy (fun card -> card[0]) // part 2
