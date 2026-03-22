open System

type Tree =
    | Empty
    | Node of string * Tree * Tree

let rnd = Random()

let randomString length =
    let chars = "0123456789"
    let randomChars = Array.init length (fun _ -> chars.[rnd.Next(chars.Length)])
    String(randomChars)

let rec insert value tree =
    match tree with
    | Empty -> Node(value, Empty, Empty)
    | Node(v, left, right) ->
        if value < v then
            Node(v, insert value left, right)
        else
            Node(v, left, insert value right)

let rec generateTree nodeCount stringLength =
    if nodeCount <= 0 then Empty
    else
        let value = randomString stringLength
        insert value (generateTree (nodeCount - 1) stringLength)

let rec fold f acc tree =
    match tree with
    | Empty -> acc
    | Node(v, left, right) ->
        let accLeft = fold f acc left
        let accNode = f accLeft v
        fold f accNode right

let countNodesWithChar targetChar tree =
    let folder acc str =
        let containsChar = str |> Seq.exists (fun c -> c = targetChar)
        if containsChar then acc + 1 else acc
    fold folder 0 tree

let rec print indent tree =
    match tree with
    | Empty -> ()
    | Node(v, left, right) ->
        printfn "%s - %s" indent v
        print (indent + "  ") left
        print (indent + "  ") right

[<EntryPoint>]
let main argv =
    let nodeCount = 8
    let stringLength = 3
    let targetChar = '1'
    
    printfn "Создаем дерево из %d случайных строк (длина %d символов)..." nodeCount stringLength
    let tree = generateTree nodeCount stringLength
    
    printfn "\nДерево:"
    print "" tree
    
    let result = countNodesWithChar targetChar tree
    
    printfn "\nКоличество узлов, содержащих символ '%c': %d" targetChar result
    
    0