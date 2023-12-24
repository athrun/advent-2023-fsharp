let input =
  "inputs/12.txt"
  |> System.IO.File.ReadAllLines
  |> Array.map (fun l -> l.Split(' '))

let memo =
  let d = System.Collections.Generic.Dictionary<string, int64>()

  let run f (a: char[]) (b: int[]) =
    let key =
      (String.concat "" (Array.map string a))
      + (String.concat "" (Array.map string b))

    if d.ContainsKey(key) then
      d.Item(key)
    else
      let r = f a b
      d.Add(key, r)
      r

  run

let inline expand s =
  [| 0..4 |]
  |> Array.map (fun _ -> s)
  |> String.concat (if s.Contains "," then "," else "?")

let dfs pattern blocks =

  let rec loop (pattern: char[]) blocks : int64 =
    // returns early if not enough space left in pattern
    if (Array.sum blocks) + blocks.Length - 1 > pattern.Length then
      0
    else if blocks.Length = 0 then
      // if we've run out of blocks to compare
      // all the remaining patterns must be . or ? to be valid
      if Array.forall (fun c -> c = '.' || c = '?') pattern then
        1
      else
        0
    else if pattern[0] = '.' then
      // move one character further into the pattern
      memo loop (pattern[1..]) blocks
    else
      let mutable total = 0L

      if pattern[0] = '?' then
        // handle the branch where ? is really a .
        // consume the pattern by 1 and compute matches from there
        total <- memo loop (pattern[1..]) blocks

      if
        Array.forall (fun c -> c = '#' || c = '?') pattern[0 .. blocks[0] - 1]
        && (if pattern.Length > blocks[0] then
              // if the pattern is longer than the block
              // the character after the block must be a . or ?
              pattern[blocks[0]] <> '#'
            else
              true)
      then
        // full match for the block, consume the pattern by the block size
        // and compute matches from there
        total <- total + memo loop (pattern[blocks[0] + 1 ..]) (blocks[1..])

      total

  memo loop pattern blocks

input
|> Array.sumBy (fun line ->
  dfs (line[0].ToCharArray()) (line[1].Split(",") |> Array.map int))
|> printfn "part1: %d"

input
|> Array.sumBy (fun line ->
  dfs ((expand line[0]).ToCharArray()) ((expand line[1]).Split(",") |> Array.map int))
|> printfn "part2: %d"
