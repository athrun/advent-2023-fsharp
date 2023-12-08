open System.Text.RegularExpressions

let input = "inputs/3.txt" |> System.IO.File.ReadAllLines

let getPartNumbers s =
  let pattern = @"(\d+)"

  [| for m in Regex.Matches(s, pattern) do
       yield m.Groups[1].Value, m.Groups[1].Index |]

let isValid (input: string[]) (partNumbers: (string * int)[][]) =
  let pattern = @"[^.0-9]"

  partNumbers
  |> Array.mapi (fun i cols ->
    cols
    |> Array.map (fun (pn, j) ->
      let edges =
        [| (i - 1, [| j - 1 .. j + pn.Length |])
           (i, [| j - 1; j + pn.Length |])
           (i + 1, [| j - 1 .. j + pn.Length |]) |]
        |> Array.map (fun (ii, js) ->
          if ii >= 0 && ii <= input.Length - 1 then
            [| for j in js do
                 j >= 0
                 && j <= input[ii].Length - 1
                 && Regex.IsMatch((input[ii][j]).ToString(), pattern) |]
          else
            [||])
        |> Array.collect id

      match Array.tryFind id edges with
      | Some _ -> int pn
      | None -> 0))
  |> Array.collect id

input |> Array.map getPartNumbers |> isValid input |> Array.sum // part 1

let isRatio (input: string[]) (partNumbers: (string * int)[][]) =
  partNumbers
  |> Array.mapi (fun i cols ->
    cols
    |> Array.map (fun (pn, j) ->
      [| (i - 1, [| j - 1 .. j + pn.Length |])
         (i, [| j - 1; j + pn.Length |])
         (i + 1, [| j - 1 .. j + pn.Length |]) |]
      |> Array.map (fun (ii, js) ->
        if ii >= 0 && ii <= input.Length - 1 then
          [| for j in js do
               if
                 j >= 0
                 && j <= input[ii].Length - 1
                 && (input[ii][j]).ToString() = @"*"
               then
                 (int pn, ii, j)
               else
                 (0, 0, 0) |]
        else
          [||])
      |> Array.collect id
      |> Array.filter (fun (pn, _, _) -> pn <> 0)))
  |> Array.collect id

input
|> Array.map getPartNumbers
|> isRatio input
|> Array.collect id
|> Array.groupBy (fun (_, i, j) -> i, j)
|> Array.map (fun (_, vals) ->
  if vals.Length > 1 then
    vals |> Array.fold (fun acc (pn, _, _) -> pn * acc) 1
  else
    0)
|> Array.sum // part 2
