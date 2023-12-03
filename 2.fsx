open System.Text.RegularExpressions

let getMaxVals s =
    let bgr = [| 0; 0; 0 |]

    [| @"(\d+) blue"; @"(\d+) green"; @"(\d+) red" |]
    |> Array.iteri (fun i p ->
        for m in Regex.Matches(s, p, RegexOptions.None) do
            let v = int m.Groups[1].Value

            if v > bgr[i] then
                bgr[i] <- v)

    bgr

let cond = [| 14; 13; 12 |]

let maxVals = "2.input.txt" |> System.IO.File.ReadAllLines |> Array.map getMaxVals

maxVals
|> Array.mapi (fun idx vals ->
    if vals[0] <= cond[0] && vals[1] <= cond[1] && vals[2] <= cond[2] then
        idx + 1
    else
        0)
|> Array.sum // part 1

maxVals |> Array.map (fun vals -> vals[0] * vals[1] * vals[2]) |> Array.sum // part 2
