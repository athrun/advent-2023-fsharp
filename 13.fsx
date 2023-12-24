open System
open System.Text.RegularExpressions

let input =
  "inputs/13.txt"
  |> System.IO.File.ReadAllText
  |> fun txt -> Regex.Split(txt, @"^$", RegexOptions.Multiline)
  |> Array.map (fun chunk ->
    chunk.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries))
  |> Array.filter (fun a -> a <> [||])
  |> Array.map (fun a ->
    Array2D.init a.Length a[0].Length (fun row col -> a[row][col] = '#'))

let findR len mapper =
  let last = len - 1

  [| for i in 0 .. last - 1 ->
       let diff = min i (last - i - 1)

       [| i - diff .. i + diff + 1 |] |> Array.map mapper |> Array.splitInto 2 |]
  |> Array.mapi (fun i ranges -> i, ranges[0] = Array.rev ranges[1])
  |> Array.filter (fun (_, b) -> b)
  |> Array.map (fun (i, _) -> i + 1)

let reflect predicate pattern =
  let x =
    findR (Array2D.length1 pattern) (fun x -> pattern[x, 0..])
    |> Array.map (fun x -> x * 100)
    |> Array.filter predicate
    |> Array.tryHead

  let y =
    findR (Array2D.length2 pattern) (fun y -> pattern[0.., y])
    |> Array.filter predicate
    |> Array.tryHead

  match x, y with
  | Some x, None -> Some x
  | None, Some y -> Some y
  // Two reflections found for pattern
  | Some x, Some y -> if predicate x then Some x else Some y
  // No reflections found for pattern
  | None, None -> None

let ways a2d =
  let mutable l = List.empty<bool array2d>

  a2d
  |> Array2D.iteri (fun row col b ->
    let na = Array2D.copy a2d
    na[row, col] <- not b
    l <- na :: l)

  l |> Seq.rev

let reflections = input |> Array.map (reflect (fun _ -> true)) |> Array.choose id

reflections |> Array.sum |> printfn "part1: %d"

input
|> Array.mapi (fun pi pattern ->
  ways pattern |> Seq.pick (reflect (fun x -> x <> reflections[pi])))
|> Array.sum
|> printfn "part2: %d"
