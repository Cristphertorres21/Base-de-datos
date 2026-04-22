namespace Busqueda

module Greedy =
    let estrategia h =
        {
            vacia = Map.empty
            sacar = 
                fun m ->
                    match Map.tryFindKey (fun _ _ -> true)
                            m with
                    | Some k -> Some (Map.find k m, 
                                      Map.remove k m)
                    | None -> None
            agregar = 
                fun mapa n -> Map.add (h n, n.estado)
                                      n mapa
        }

    let key h n = n.estado, h n
    