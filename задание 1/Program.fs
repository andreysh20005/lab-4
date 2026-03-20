
open System
open System.Text

type 't btree =
    | Node of 't * 't btree * 't btree
    | Nil



let infix root left right = 
    (root(); left(); right())

let iterh trav f t =
    let rec tr t h =
        match t with
        | Node (x, L, R) -> 
            trav
                (fun () -> f x h)      
                (fun () -> tr L (h+1)) 
                (fun () -> tr R (h+1)) 
        | Nil -> ()
    tr t 0

let spaces n = List.fold (fun s _ -> s + ".") "" [0..n]

let printTree T =
    iterh infix (fun x h -> printfn "%s%A" (spaces h) x) T


let rec insert t x =
    match t with
    | Nil -> Node(x, Nil, Nil)
    | Node(z, L, R) ->
        if x < z then Node(z, insert L x, R)
        else Node(z, L, insert R x)







let randFloat () =
    let r = Random()
    r.NextDouble() * 200.0 - 100.0

let treeFold f st tree =   
    let rec ob d k = 
        match d with
        | Node (t, l, r) -> ob r (f (ob l k) t)
        | Nil -> k
    ob tree st

let rec mapTree f tree =
    match tree with
    | Nil -> Nil
    | Node(x, left, right) -> Node(f x, mapTree f left, mapTree f right)

[<EntryPoint>]
let main _ =

    let rnd = Random()
    let count = rnd.Next(5, 15)
    let A = List.init count (fun _ -> randFloat ())
    printfn "Исходный список:\n%A\n" A

    let startBinTree = A |> List.fold insert Nil
    printfn "Исходное дерево:"
    printTree startBinTree


    let newBinTree = mapTree int startBinTree
    printfn "\nНовое дерево (целые части исходных чисел):\n"
    printTree newBinTree

    0
