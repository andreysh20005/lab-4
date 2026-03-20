//Сумма элементов дерева
open System
open System.Text

type 't btree =
    Node of 't * 't btree * 't btree
    | Nil



let infix root left right = 
    (
         root();left(); right()
    )

let iterh trav f t =
    let rec tr t h =
        match t with
        Node (x,L,R) -> trav
                            (fun () -> (f x h)) 
                            (fun () -> tr L (h+1))
                            (fun () -> tr R (h+1));
        | Nil -> ()
    tr t 0

let spaces n =
    List.fold (fun s _ -> s+".") "" [0..n]

let printTree T =
    iterh infix (fun x h -> printfn "%s%A" (spaces h)x) T



let rec insert t x =
        match t with
            Nil -> Node(x,Nil,Nil)
            | Node(z,L,R) -> 
                if x<z then Node(z,insert L x ,R)
                else Node(z,L,insert R x)

let treeFold f st tree =
    let rec ob d k =
        match d with
        | Node (t, l, r) as node ->  
            
            let leftResult = ob l k
            
            let afterNode = f leftResult node
            
            ob r afterNode
        | Nil -> k
    ob tree st

let rec mapTree f tree =
    match tree with
    | Nil -> Nil
    | Node(x, left, right) -> Node(f x, mapTree f left, mapTree f right)



let randStr () =
    let r2 = new Random()
    let length = int(r2.Next(1, 20))

    let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789".ToCharArray()
    let buffer = Random.Shared.GetItems(chars, length)
    new string(buffer)
    
let incrementLastChar (str:string) =
    if str.Length > 0 then
        str.[0..str.Length-2] +
        (char (int str.[str.Length-1] + 1)).ToString()
    else
        ""


let oneLeafNode tree =
    let rec loop acc = function
        | Nil -> acc
        | Node(x, L, R) as node ->
            let condition = 
                match node with
                //| Node(_, Node(_, Nil, Nil), Node(_, Nil, Nil)) -> false
                | Node(_, Nil, Node(_, Nil, Nil)) -> true
                | Node(_, Node(_, Nil, Nil), Nil) -> true
                | _ -> false
            let newAcc = if condition then x :: acc else acc
            loop (loop newAcc L) R
    loop [] tree 


let isOneLeaf node =
     match node with
        //| Node(_, Node(_, Nil, Nil), Node(_, Nil, Nil)) -> false
        | Node(_, Nil, Node(_, Nil, Nil)) -> true
        | Node(_, Node(_, Nil, Nil), Nil) -> true
        | _ -> false


[<EntryPoint>]
let main _ =
    
    let A =
        [            
            let r1 = new Random()
            let n = int(r1.Next(5, 15))
            for i in 1..n do
                yield randStr ()
        ]
    printfn "Исходный список %A\n\n" A
    let startBinTree = A |> List.fold insert Nil
    printfn "исходное дерево:\n\n"
    printTree startBinTree
    printfn "вершины с одним листом: %A" (
        treeFold (
            fun acc node -> 
                if isOneLeaf node then
                    match node with Node(x, _, _) -> x :: acc
                else 
                    acc
        ) [] startBinTree)


    0
