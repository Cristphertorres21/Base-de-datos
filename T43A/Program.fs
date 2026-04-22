open Busqueda

//printfn "%A" (NReinas.aptitud [|2; 2; 2; 2; 2; 2; 2; 2|])

let rnd = System.Random()
match Capitulo4.recocidoSimulado
        (Capitulo4.temperatura 100.0 0.001 8000)
        (fun n -> -NReinas.aptitud' n)
        (NReinas.problema rnd 20)
        with
| Some n -> 
    printfn "Solución potancial: %A" n.estado
    printfn "Meta: %A" (NReinas.meta n.estado)
    printfn "Pares atacando: %A" (NReinas.aptitud n.estado)
| None -> printfn "No hay solución"
