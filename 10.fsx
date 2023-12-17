let input =
  "inputs/10.txt"
  |> System.IO.File.ReadAllLines
  |> Array.map (fun line -> line.ToCharArray())
  |> Array.rev

let tiles =
  [| ('|', 0b1010uy) // up and down
     ('-', 0b0101uy) // right and left
     ('L', 0b1100uy) // up and right
     ('J', 0b1001uy) // up and left
     ('7', 0b0011uy) // down and left
     ('F', 0b0110uy) // right and left
     ('S', 0b1_1111uy) // starting position
     ('X', 0b10_0000uy) // marker for inclusion
     ('.', 0b0uy) |]
  |> Map.ofArray

let load (inp: char[][]) =
  let mutable start = (0, 0)

  let grid =
    Array2D.init (inp[0].Length + 2) (inp.Length + 2) (fun y x ->
      if y = 0 || x = 0 || y > inp[0].Length || x > inp.Length then
        // place an edge around the grid to simplify adjacent calculations
        0b0uy
      else
        let v = tiles.Item(input[x - 1][y - 1])

        if v = 0b1_1111uy then
          start <- (y, x)

        v)

  grid, start

let grid, start = load input

let adjacent x y =
  [| grid[x, y + 1]; grid[x + 1, y]; grid[x, y - 1]; grid[x - 1, y] |]

let adjacentExits x y =
  [| (x, y + 1), (grid[x, y + 1] &&& 0b0010uy), (grid[x, y] &&& 0b1000uy)
     (x + 1, y), grid[x + 1, y] &&& 0b0001uy, (grid[x, y] &&& 0b0100uy)
     (x, y - 1), grid[x, y - 1] &&& 0b1000uy, (grid[x, y] &&& 0b0010uy)
     (x - 1, y), grid[x - 1, y] &&& 0b0100uy, (grid[x, y] &&& 0b0001uy) |]
  |> Array.filter (fun (_, c1, c2) -> c1 <> 0uy && c2 <> 0uy)
  |> Array.map (fun (xy, _, _) -> xy)

let revTiles = tiles |> Map.toArray |> Array.map (fun (k, v) -> v, k) |> Map.ofArray

let toC b = revTiles[b]

let printGrid (g: byte array2d) =
  for y in [ (Array2D.length2 g) - 1 .. -1 .. 0 ] do
    for x in [ 0 .. (Array2D.length1 g) - 1 ] do
      printf "%c" (toC g[x, y])

    printfn ""

let navigate (start: int * int) =
  let rec next (cur: int * int) (path: (int * int) list) =
    //printfn "path: %A" path
    //printfn "cur: %A %A" cur revTiles[(grid[(fst cur), (snd cur)])]
    let exits = adjacentExits (fst cur) (snd cur)
    //printfn "exits: %A" exits

    if cur <> start && exits.Length <> 2 then
      failwith $"exits: %A{exits}"

    match path with
    | [] -> next exits[0] [ cur ]
    | _ when cur = start -> path
    | head :: _ ->
      next (exits |> Array.filter (fun e -> e <> head) |> Array.item 0) (cur :: path)

  next start []

let path = navigate start |> List.toArray
path |> Array.length |> (fun x -> x / 2) |> printfn "part1: %d"


// ----------------------------------------
// part 2 / work in progress
// ----------------------------------------

adjacent (fst start) (snd start)
adjacentExits (fst start) (snd start)

adjacentExits (fst start) (snd start)
|> Array.map (fun (x, y) -> toC grid[x, y])

let area (g: byte array2d) startValue =
  // i'm doing an horizontal scan to compute the area
  // but it's quite tricky
  let cleanGrid = Array2D.create (Array2D.length1 g) (Array2D.length2 g) 0uy

  path
  |> Array.iter (fun (x, y) ->
    if x = (fst start) && y = (snd start) then
      // important to set the start position to the proper tile type
      cleanGrid[x, y] <- startValue
    else
      cleanGrid[x, y] <- g[x, y])

  // some pointers on the horizontal scan strategy
  // https://gist.github.com/rkirov/3bdf18b031401ead7a29512872983431
  // for some reason the code below is off by 1
  // when using the real input, but produces the right
  // result with the test inputs.

  let mutable area = 0

  for y in 0 .. (Array2D.length2 cleanGrid) - 1 do
    let mutable out = true
    let mutable lastUpOrDown = 0uy

    //  let y = 7
    for x in 0 .. (Array2D.length1 cleanGrid) - 1 do
      let c = cleanGrid[x, y]
      let isPath = c <> 0uy
      let isV = isPath && c = 0b1010uy // |
      let isCorner = isPath && c <> 0b0101uy && c <> 0b1010uy // not - or |

      //printfn $"c:{toC c} isPath:{isPath} isV:{isV} isCorner:{isCorner}"

      if (not out) && (not isPath) then
        area <- area + 1
        cleanGrid[x, y] <- 0b10_0000uy

      if isV then
        out <- not out

      if isCorner then
        let upOrDown = c &&& 0b1010uy

        if lastUpOrDown = 0uy then
          lastUpOrDown <- upOrDown
        else
          if upOrDown <> lastUpOrDown then
            out <- not out

          lastUpOrDown <- 0uy

  area

area grid 0b0101uy // setting the S value is important
|> printfn "part2: %d"
// produces 566, which is wrong
// right answer is 567, I'm off by 1...



(*
printGrid grid
grid[0, 0]
grid[140, 0..]
input[90][139]
grid[5, 1] |> (fun x -> revTiles[x])
grid[1, 1] |> (fun x -> revTiles[x])
grid[140, 140] |> (fun x -> revTiles[x])
*)
