open System.Collections.Generic

let input =
  "inputs/21.txt" |> System.IO.File.ReadAllLines |> Array.map _.ToCharArray()

let grid =
  Array2D.init input.Length input[0].Length (fun row col -> input[row][col])

let h = Array2D.length1 grid
let w = Array2D.length2 grid
let goal, cycle = 26_501_365, 26_501_365 % w

let lookup (i, j) (grid: char array2d) =
  grid[(h + (i % h)) % h, (w + (j % w)) % w] // infinite grid lookup

let adjacents (i, j) (grid: char array2d) =
  [| i - 1, j; i, j + 1; i + 1, j; i, j - 1 |]
  |> Array.filter (fun (i, j) -> lookup (i, j) grid <> '#')

let start =
  let mutable s = (0, 0)

  Array2D.iteri
    (fun i j v ->
      if v = 'S' then
        s <- (i, j))
    grid

  s

let inline ticktock iter tick tock =
  if iter % 2 = 1 then tick, tock else tock, tick

let walk start times grid =
  let tick, tock = HashSet<int * int>(), HashSet<int * int>()
  let mutable iter = 1
  let mutable discovered = [ start ]
  let bound = (cycle + 2 * w) // max bound for part 2

  while iter <= times && iter <= bound do
    // reachable and visited alternate between iterations
    let _, tgt = ticktock iter tick tock

    let disc' = // keep track of discovered nodes (speed optimisation)
      [ for p in discovered do
          for p' in adjacents p grid do
            if not (tgt.Contains p') then
              // only add to Set if new
              tgt.Add p' |> ignore
              yield p' ]

    discovered <- disc'
    iter <- iter + 1

  ticktock iter tick tock |> fun (t, _) -> t.Count

let quadratic (x, y, z) (n: int) =
  // lifted quadratic derivation from
  // https://github.com/dps/aoc/blob/main/2023/day21/main.py
  // I need to revisit this to understand how it works.
  let a = (z - 2.0 * y + x) / 2.0
  let b = y - x - a
  let c = x
  let n = float n
  a * (n * n) + b * n + c

walk start 64 grid |> printfn "part 1: %d"

[| cycle; cycle + w; cycle + 2 * w |] // after 1, 2, 3 cycles
|> Array.map (fun x -> walk start x grid |> float)
|> fun a -> quadratic (a[0], a[1], a[2]) (goal / w) |> int64
|> printfn "part 2: %d"
