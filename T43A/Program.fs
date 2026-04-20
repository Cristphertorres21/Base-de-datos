open Busqueda

match Capitulo3.busquedaGrafo
        BFS.estrategia 
        BFS.key
        (Puzle8.problema Puzle8.estado_inicial_x) with
| Some r -> 
    let acciones = Capitulo3.acciones r
    printfn "Solución %A" acciones
    printfn "Profundidad: %A" (List.length acciones)
| None -> 
    printfn "No se encontró la solución"
