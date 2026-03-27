open System

type BinaryTree = 
    | Node of int * BinaryTree * BinaryTree 
    | Empty

let rnd = Random()

let rec insert value tree =
    match tree with
    | Empty -> Node(value, Empty, Empty)
    | Node(v, left, right) ->
        if value < v then
            Node(v, insert value left, right)
        else
            Node(v, left, insert value right)

let rec generateRandomTree nodeCount =
    if nodeCount <= 0 then Empty
    else
        let value = rnd.Next(-999, 1000)
        insert value (generateRandomTree (nodeCount - 1))

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
        printfn "%s- %d" indent v
        printTree (indent + "  ") l
        printTree (indent + "  ") r

[<EntryPoint>]
let main argv =
    let nodeCount = 10
    let root = generateRandomTree nodeCount
    printfn "Исходное дерево:"
    printTree "" root

    let newRoot = mapTree (fun x -> replaceMinDigit x 9) root
    printfn "\nНовое дерево (минимальная цифра заменена на 9):"
    printTree "" newRoot
    
    0