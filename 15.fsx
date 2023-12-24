open System

let input =
  "inputs/15.txt"
  |> System.IO.File.ReadAllText
  |> (fun s -> s.Split(',', StringSplitOptions.TrimEntries))

let hashing (s: string) =
  s.ToCharArray() |> Array.fold (fun acc c -> (acc + (int c)) * 17 % 256) 0

type Box = (struct (string * int)) list

let operate (boxes: Box[]) (op: string) =
  if op.Contains "=" then
    let label, focal = op.Split('=') |> fun a -> a[0], int a[1]
    let boxi = hashing label
    let res = boxes[boxi] |> List.tryFindIndex (fun struct (l, _) -> label = l)

    match res with
    | Some i -> boxes[boxi] <- List.updateAt i struct (label, focal) boxes[boxi]
    | None -> boxes[boxi] <- boxes[boxi] @ [ struct (label, focal) ]

  else
    let label = op.Split('-')[0]
    let boxi = hashing label
    let res = boxes[boxi] |> List.tryFindIndex (fun struct (l, _) -> label = l)
    res |> Option.iter (fun i -> boxes[boxi] <- List.removeAt i boxes[boxi])

let fillBoxes ops =
  let boxes = Array.create<Box> 256 []
  Array.iter (operate boxes) ops
  boxes

input |> Array.map hashing |> Array.sum |> printfn "part1: %d"

input
|> fillBoxes
|> Array.mapi (fun boxi box ->
  box
  |> List.mapi (fun lensi struct (_, focal) -> (boxi + 1) * (lensi + 1) * focal)
  |> List.sum)
|> Array.sum
|> printfn "part2: %d"
