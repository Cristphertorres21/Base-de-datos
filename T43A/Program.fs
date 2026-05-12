open Busqueda

//printfn "%A" (NReinas.aptitud [|2; 2; 2; 2; 2; 2; 2; 2|])

match CSP.backtracking 
        (NReinas.csp 20) 
    with
| Some n -> Map.iter (printfn "%A - %A") n.estado
| None -> printfn "No encontré la sol"
