namespace Busqueda

module ChangoYPlatano =
    open Busqueda

    type lugar = Puerta | Ventana | Centro

    type estado = lugar * lugar * bool * bool

    type acciones = Caminar of lugar | Empujar of lugar | Subir | Tomar

    let inicial = (Puerta, Ventana, false, false)

    let sucesores x = 
        let caminar = function | (_, B, false, H) -> 
                                [(Caminar Puerta, (Puerta, B, false, H))
                                 (Caminar Ventana, (Ventana, B, false, H))
                                 (Caminar Centro, (Centro, B, false, H))]
                               | _ -> []
        let empujar = function | (M, B, false, H) when M = B -> 
                                [(Empujar Puerta, (Puerta, Puerta, false, H))
                                 (Empujar Ventana, (Ventana, Ventana, false, H))
                                 (Empujar Centro, (Centro, Centro, false, H))]
                               | _ -> []
        let subir = function | (M, B, false, H) when M = B -> 
                                [(Subir, (M, B, true, H))]
                             | _ -> []
        let tomar = function | (Centro, B, true, false) -> 
                                [(Tomar, (Centro, B, true, true))]
                             | _ -> []
        caminar x @
        empujar x @
        subir x @
        tomar x

    let meta = function 
        | (_, _, _, true) -> true
        | _ -> false

    let costo _ _ _ = 1.0
    
    let problema inicial = 
              {
                     inicial = inicial
                     sucesores = sucesores
                     meta = meta
                     costo = costo
              }

    let prueba algoritmo inicio =
        algoritmo (problema inicio)
