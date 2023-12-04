let parseInt c =
  try
    Some(int c)
  with :? System.FormatException ->
    None

"1.input.txt"
|> System.IO.File.ReadAllLines
|> Array.map (fun l ->
  l.ToCharArray()
  |> Array.choose (fun c -> c.ToString() |> parseInt)
  |> fun a -> a[0] * 10 + a[a.Length - 1])
|> Array.sum // Part 1

let validDigits =
  [| "zero"
     "one"
     "two"
     "three"
     "four"
     "five"
     "six"
     "seven"
     "eight"
     "nine" |]

// Edge case warning
// 4nine7oneighthm -> 48
// oneight should be read as 8 and not 1

"1.input.txt"
|> System.IO.File.ReadAllLines
|> Array.map (fun l ->
  let mutable pos = 0
  let mutable v = []

  while pos < l.Length do
    match parseInt (l[pos].ToString()) with
    | Some i ->
      v <- i :: v
      pos <- pos + 1
    | None ->
      let mutable newPos = 0

      validDigits
      |> Array.iteri (fun idx digit ->
        if newPos = 0 then
          if (l[pos..].StartsWith(digit)) then
            v <- idx :: v
            newPos <- pos + 1)

      if newPos > pos then pos <- newPos else pos <- pos + 1

  v <- v |> List.rev
  v |> List.toArray |> (fun a -> a[0] * 10 + a[a.Length - 1]))
|> Array.sum // Part2
