//Alan Gilberto Sánchez Zavala
//Modulos a utilizar
open Busqueda
open Capitulo3
open CostoUniforme
open DFSL
open IDFSL
open DFS
open BFS
open Greedy
open AStar

//WOLF GOAT CABBAGE - RESUELTO CON BUSQUEDA GRAFO - BFS
open ZorroPatoSaco
printfn "\nBFS:"
busqueda_grafo (problema inicial) BFS.estrategia BFS.key
|> Option.map (fun (nodo) -> (nodo.g, acciones nodo))
|> Option.map (fun (costoTotal, xs) -> (List.length xs, costoTotal,xs))
|> printfn "%A"
(*
let resultado = 
    Capitulo3.reinicio_aleatorio
        ((fun x -> -x) << 
            NReinas.h << 
                (fun nodo -> nodo.estado))
        (fun () -> NReinas.problema 8)

printfn "resultado: %A" resultado.estado
printfn "h(resultado): %A" (NReinas.h resultado.estado)*)
