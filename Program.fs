open System

type BinaryTree = 
    | Node of int * BinaryTree * BinaryTree 
    | Empty

let rnd = Random()

let rec generateRandomTree depth =
    if depth = 0 then
        Empty
    else
        let value = rnd.Next(-999, 1000)
        let left = if rnd.NextDouble() < 0.7 then generateRandomTree (depth - 1) else Empty
        let right = if rnd.NextDouble() < 0.7 then generateRandomTree (depth - 1) else Empty
        Node(value, left, right)

let replaceMinDigit (number: int) (replacement: int) =
    let absNumber = Math.Abs(number)
    
    if absNumber = 0 then replacement
    else
        let minDigit = 
            absNumber.ToString()
            |> Seq.map (fun c -> int c - int '0')
            |> Seq.min
        
        let newNumberStr = 
            absNumber.ToString()
            |> Seq.map (fun c -> 
                let digit = int c - int '0'
                if digit = minDigit then 
                    replacement.ToString()
                else 
                    c.ToString())
            |> String.concat ""
        
        if number < 0 then -int newNumberStr else int newNumberStr

let rec mapTree f tree =
    match tree with
    | Empty -> Empty
    | Node(data, left, right) ->
        Node(f data, mapTree f left, mapTree f right)

let rec printTree indent tree =
    match tree with
    | Empty -> ()
    | Node(v, l, r) ->
        printTree (indent + "  ") r
        printfn "%s- %d" indent v
        printTree (indent + "  ") l

[<EntryPoint>]
let main argv =
    let depth = 4
    let root = generateRandomTree depth
    printfn "Исходное дерево:"
    printTree "" root

    let newRoot = mapTree (fun x -> replaceMinDigit x 9) root
    printfn "\nНовое дерево (минимальная цифра заменена наjjn 9):"
    printTree "" newRoot
    
    0