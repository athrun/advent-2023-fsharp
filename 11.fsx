let input =
  "inputs/11.txt"
  |> System.IO.File.ReadAllLines
  |> Array.map (fun line -> line.ToCharArray())

let grid: char array2d =
  Array2D.init input.Length input[0].Length (fun row col -> (input[row][col]))

let expandedRows =
  [| for i in 0 .. (Array2D.length1 grid) - 1 ->
       grid[i, 0..] |> Array.forall (fun v -> v = '.') |]
  |> Array.mapi (fun i v -> i, v)
  |> Array.filter (fun (_, v) -> v)
  |> Array.map (fun (i, _) -> int64 i)

let expandedCols =
  [| for j in 0 .. (Array2D.length2 grid) - 1 ->
       grid[0.., j] |> Array.forall (fun v -> v = '.') |]
  |> Array.mapi (fun i v -> i, v)
  |> Array.filter (fun (_, v) -> v)
  |> Array.map (fun (i, _) -> int64 i)

let inline manahattan struct (x1, y1) struct (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let inline expand p slots factor =
  p + factor * (slots |> Array.filter (fun x -> x < p) |> Array.length |> int64)

let pairs =
  let stars =
    let mutable locs = []

    grid
    |> Array2D.iteri (fun i j v ->
      if v = '#' then
        locs <- struct (int64 i, int64 j) :: locs)

    locs |> Array.ofList |> Array.rev

  stars
  |> Array.allPairs stars
  |> Array.map (fun (x, y) -> if x > y then x, y else y, x)
  |> Array.distinct
  |> Array.filter (fun (x, y) -> not (x = y))

[| ("part1", 1); ("part2", 1_000_000 - 1) |]
|> Array.iter (fun (part, factor) ->
  pairs
  |> Array.map (fun ((x1, y1), (x2, y2)) ->
    manahattan
      (expand x1 expandedRows factor, expand y1 expandedCols factor)
      (expand x2 expandedRows factor, expand y2 expandedCols factor))
  |> Array.sum
  |> printf "%s: %d\n" part)
