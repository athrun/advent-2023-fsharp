let input =
  "inputs/7.txt"
  |> System.IO.File.ReadAllLines
  |> Array.map (fun l -> l.Split ' ')
  |> Array.map (fun v -> v[0], int v[1])

let toRanks: char[] -> Map<char, int> =
  Array.rev >> Array.mapi (fun i c -> (c, i + 1)) >> Map.ofArray

let cardRanks =
  toRanks [| 'A'; 'K'; 'Q'; 'J'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2' |]

let cardRanksWithJ =
  toRanks [| 'A'; 'K'; 'Q'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'; 'J' |]

let nbRanks = cardRanks.Count

let score (hand: string) (withJokers: bool) =
  let jokerCount =
    hand.ToCharArray() |> Array.filter (fun c -> c = 'J') |> Array.length

  let handType =
    hand.ToCharArray()
    |> Array.filter (fun c -> not withJokers || c <> 'J') // remove Js if playing with Jokers
    |> Array.countBy id
    |> Array.map (fun (_, r) -> r)
    |> Array.sortDescending
    // simply add the joker count to the card type with the highest count
    |> Array.mapi (fun i v -> if withJokers && i = 0 then v + jokerCount else v)

  let score1 =
    (pown nbRanks 5)
    * match handType with
      | [||] when withJokers && jokerCount = 5 -> 7 // Five of a kind
      | [| 5 |] -> 7 // Five of a kind
      | [| 4; 1 |] -> 6 // Four of a kind
      | [| 3; 2 |] -> 5 // Full house
      | [| 3; 1; 1 |] -> 4 // Three of a kind
      | [| 2; 2; 1 |] -> 3 // Two pair
      | [| 2; 1; 1; 1 |] -> 2 // One pair
      | _ -> 1 // High card

  let ranking = if withJokers then cardRanksWithJ else cardRanks

  let score2 =
    hand.ToCharArray()
    |> Array.rev
    |> Array.mapi (fun i c -> ranking[c] * (pown nbRanks i))
    |> Array.sum

  score1 + score2

let winnings withJokers : (string * int)[] -> int =
  Array.map (fun (h, b) -> (h, b, score h withJokers))
  >> Array.sortBy (fun (_, _, s) -> s)
  >> Array.mapi (fun i (h, b, s) -> (i + 1, h, b, s))
  >> Array.sumBy (fun (i, _, b, _) -> i * b)

input |> winnings false |> printfn "part1: %A"
input |> winnings true |> printfn "part2: %A"
