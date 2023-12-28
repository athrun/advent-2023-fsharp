open System
open System.Collections.Generic

let input =
  "inputs/18.txt"
  |> System.IO.File.ReadAllLines
  |> Array.map _.Split(" ")
  |> Array.map (fun a -> a[0], int64 a[1], a[2])

let path ops =
  let mutable i, j = 0L, 0L
  let mutable l = List<struct (int64 * int64)>(5000)

  for (dir, count, _) in ops do
    if dir = "U" then
      let ti = i
      i <- i - count

      l.AddRange(seq { for c in 1L .. count -> struct (ti - c, j) })
    else if dir = "D" then
      let ti = i
      i <- i + count

      l.AddRange(seq { for c in 1L .. count -> struct (ti + c, j) })
    else if dir = "R" then
      let tj = j
      j <- j + count

      l.AddRange(seq { for c in 1L .. count -> struct (i, tj + c) })
    else if dir = "L" then
      let tj = j
      j <- j - count

      l.AddRange(seq { for c in 1L .. count -> struct (i, tj - c) })
    else
      failwith "unknown dir"

  l |> Array.ofSeq

let fst struct (x, _) = x
let snd struct (_, y) = y

let shoelace (vs: struct (int64 * int64) array) =
  let mutable acc = 0L

  for i in 0 .. vs.Length - 1 do
    let j = (i + 1) % vs.Length
    acc <- acc + (fst vs[i] * snd vs[j] - fst vs[j] * snd vs[i])

  (float (abs acc)) / 2.0

let correct ops =
  let dirs = [| "R"; "D"; "L"; "U" |]

  [| for (_, _, hex: string) in ops do
       let dist = Int64.Parse(hex[2..6], Globalization.NumberStyles.HexNumber)
       let dir = dirs[Int32.Parse(hex[hex.Length - 2].ToString())]
       yield dir, dist, String.Empty |]

let area vertices =
  // Ask is for the area of the "thick" polygon formed by the 1x1 pixels along
  // the edges of the "thin" polygon.
  // The thick polygon area is = area of the thick boundary + inside area.
  // Thick boundary area is just integer path length / 2 (from Pick's theorem).
  // Thick area = thin area + path/2 + 1
  shoelace vertices + float (vertices.Length / 2) + 1.0

path input |> area |> printfn "part1: %O"
path (correct input) |> area |> printfn "part2: %O"
