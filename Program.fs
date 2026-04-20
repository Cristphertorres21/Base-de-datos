open Busqueda

match Capitulo3.busquedaGrafo
        (DFSL.estrategia 20)
        DFSL.key
        (Puzzle8.problema Puzzle8.estado_inicial_x) with
| Some r -> 
    let acciones = Capitulo3.acciones r
    printfn "Solución %A" acciones
    printfn "Profundidad: %A" (List.length acciones)
| None -> 
    printfn "No se encontró la solución"
