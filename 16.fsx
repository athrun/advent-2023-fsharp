let input =
  "inputs/16.test.txt" |> System.IO.File.ReadAllLines |> Array.map _.ToCharArray()

let grid =
  Array2D.init input.Length input[0].Length (fun row col -> input[row][col])

type Dir =
  | N
  | E
  | S
  | W

let next i j dir grid =
  match dir with
  | E ->
    let ni, nj = i, j + 1

    if nj >= 0 && nj <= (Array2D.length2 grid) - 1 then
      match grid[ni, nj] with
      | '\\' -> Some [| struct (ni, nj, S) |]
      | '/' -> Some [| struct (ni, nj, N) |]
      | '|' -> Some [| struct (ni, nj, N); struct (ni, nj, S) |]
      | '-' -> Some [| struct (ni, nj, E) |]
      | _ -> Some [| struct (ni, nj, E) |]
    else
      None
  | N ->
    let ni, nj = i - 1, j

    if ni >= 0 && ni <= (Array2D.length1 grid) - 1 then
      match grid[ni, nj] with
      | '\\' -> Some [| struct (ni, nj, W) |]
      | '/' -> Some [| struct (ni, nj, E) |]
      | '|' -> Some [| struct (ni, nj, N) |]
      | '-' -> Some [| struct (ni, nj, W); struct (ni, nj, E) |]
      | _ -> Some [| struct (ni, nj, N) |]
    else
      None
  | W ->
    let ni, nj = i, j - 1

    if nj >= 0 && nj <= (Array2D.length2 grid) - 1 then
      match grid[ni, nj] with
      | '\\' -> Some [| struct (ni, nj, N) |]
      | '/' -> Some [| struct (ni, nj, S) |]
      | '|' -> Some [| struct (ni, nj, N); struct (ni, nj, S) |]
      | '-' -> Some [| struct (ni, nj, W) |]
      | _ -> Some [| struct (ni, nj, W) |]
    else
      None
  | S ->
    let ni, nj = i + 1, j

    if ni >= 0 && ni <= (Array2D.length1 grid) - 1 then
      match grid[ni, nj] with
      | '\\' -> Some [| struct (ni, nj, E) |]
      | '/' -> Some [| struct (ni, nj, W) |]
      | '|' -> Some [| struct (ni, nj, S) |]
      | '-' -> Some [| struct (ni, nj, W); struct (ni, nj, E) |]
      | _ -> Some [| struct (ni, nj, S) |]
    else
      None

let energize start grid =
  let mutable seen = Map.empty<struct (int * int * Dir), bool>
  let queue = System.Collections.Generic.Queue<struct (int * int * Dir)>()

  queue.Enqueue(start)

  while queue.Count > 0 do
    let struct (i, j, dir) = queue.Dequeue()

    next i j dir grid
    |> Option.iter (fun a ->
      a
      |> Array.iter (fun beam ->
        if not (seen.ContainsKey(beam)) then
          seen <- seen.Add(beam, true)
          queue.Enqueue(beam)))

  seen
  |> Map.toArray
  |> Array.distinctBy (fun (struct (i, j, _), _) -> i, j)
  |> Array.length

energize (0, -1, E) grid |> printf "part1: %d\n"

let ns =
  [| 0 .. (Array2D.length2 grid) - 1 |]
  |> Array.map (fun j ->
    [| energize (-1, j, S) grid; energize (Array2D.length1 grid, j, N) grid |])
  |> Array.collect id

let we =
  [| 0 .. (Array2D.length1 grid) - 1 |]
  |> Array.map (fun i ->
    [| energize (i, -1, E) grid; energize (i, Array2D.length2 grid, W) grid |])
  |> Array.collect id

Array.concat [| ns; we |] |> Array.max |> printf "part2: %d\n"
