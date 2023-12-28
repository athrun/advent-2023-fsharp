open System
open System.Collections.Generic

let input =
  "inputs/17.txt"
  |> System.IO.File.ReadAllLines
  |> Array.map (fun s -> s.ToCharArray() |> Array.map (string >> Int64.Parse))

let grid =
  Array2D.init input.Length input[0].Length (fun row col -> input[row][col])

// Node is a tuple of (i, j, dir, count)
type Node = (struct (int * int * int * int))

let nodes condition struct (i, j, dir, count) grid =
  // produce valid adjacent nodes based on constraints
  // dir: N = 0, E = 1, S = 2, W = 3
  [| i - 1, j, 0, 0; i, j + 1, 1, 0; i + 1, j, 2, 0; i, j - 1, 3, 0 |]
  |> Array.indexed
  |> Array.filter (fun (i, _) ->
    (dir = -1) // start position can go anywhere
    || (i <> (dir + 2) % 4)) // other dirs can't go back in the opposite way
  |> Array.map (fun (_, (ni, nj, ndir, ncount)) ->
    if dir = ndir then // if going in same dir, increase dir count
      ni, nj, ndir, count + 1
    else
      ni, nj, ndir, ncount)
  |> Array.filter (fun (ni, nj, ndir, ncount) ->
    ni >= 0
    && nj >= 0
    && ni <= (Array2D.length1 grid) - 1
    && nj <= (Array2D.length2 grid) - 1
    && (condition (i, j, dir, count) (ni, nj, ndir, ncount)))

let dijkstra start goal adjacents (grid: int64 array2d) =
  // dijkstra's algorithm, using Uniform Cost Search (goal directed)
  let mutable dist = Dictionary<Node, int64>()
  let mutable q = PriorityQueue<Node, int64>()
  let si, sj = start

  dist[struct (si, sj, -1, 0)] <- 0
  q.Enqueue((si, sj, -1, 0), 0)

  while q.Count > 0 && not (goal (q.Peek())) do
    let u = q.Dequeue()

    for (vi, vj, vdir, vcount) in adjacents u grid do
      let alt = dist[u] + grid[vi, vj]

      let cur =
        if dist.ContainsKey(struct (vi, vj, vdir, vcount)) then
          dist[struct (vi, vj, vdir, vcount)]
        else
          Int64.MaxValue

      if alt < cur then
        dist[struct (vi, vj, vdir, vcount)] <- alt
        q.Enqueue(struct (vi, vj, vdir, vcount), alt)

  dist[q.Peek()]

dijkstra
  (0, 0) // start node
  (fun node -> // goal condition
    let goal = Array2D.length1 grid - 1, Array2D.length2 grid - 1
    let struct (i, j, _, _) = node
    (i, j) = goal)
  (nodes (fun _ n -> // adjacent node filter
    let (_, _, _, ncount) = n
    // move a maximum of 3 consecutive blocks
    ncount < 3))
  grid
|> printfn "part1: %d"

dijkstra
  (0, 0) // start node
  (fun node -> // goal condition
    let goal = Array2D.length1 grid - 1, Array2D.length2 grid - 1
    let struct (i, j, _, count) = node
    (i, j) = goal && count >= 3)
  (nodes (fun s n -> // adjacent node filter
    let (_, _, sdir, scount) = s
    let (_, _, ndir, ncount) = n
    // move a maximum of ten consecutive blocks
    ncount < 10
    &&
    // if count is less than 3 changing dir is not allowed
    (scount >= 3 || sdir = ndir || sdir = -1)))
  grid
|> printfn "part2: %d"
