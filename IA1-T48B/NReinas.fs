namespace Busqueda

module NReinas =
    type estado = int []
    type accion = unit

    let estado_inicial (rnd:System.Random) n = 
        [|1 .. n|]
        |> Array.map (fun _ -> rnd.Next(n))

    let sucesores estado = 
        let st = [|0 .. Array.length estado - 1|]
        let moverReina i =
            st
            |> Array.map (fun k ->
                Array.mapi 
                    (fun j r -> if j <> i then r else k)
                    estado)
        st
        |> Array.collect moverReina
        |> Array.toList
        |> List.map (fun estado -> (), estado)

    let atacan ((x1,y1),(x2,y2)) =
        y1 = y2 ||
        x2-x1 = y2-y1 ||
        x2-x1 = y1-y2

    let h estado = 
        let reinas = 
            estado
            |> Array.mapi (fun i y -> (i, y))
        let pares = 
            Array.allPairs reinas reinas
            |> Array.filter (fun (r1,r2) -> r1 <> r2)
        Array.sumBy
            (fun par -> if atacan par then 1 else 0)
            pares / 2

    let meta estado = 
        h estado = 0

    let costo _ _ _ = 1.0
    
    let problema n =
        let rnd = System.Random()
        {
            inicial = estado_inicial rnd n
            sucesores = sucesores
            meta = meta
            costo = costo
        }