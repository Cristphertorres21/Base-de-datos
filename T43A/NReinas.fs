namespace Busqueda


module NReinas =
    type estado = int []

    type accion = unit // ()

    // estado inicial aleatorio
    let inicio (rnd:System.Random) n =
        [| for i in 1 .. n do 
                rnd.Next(n) |]

    let sucesores estado =
        let n = Array.length estado    
        let mover i =
            Array.mapi 
                (fun j _ -> 
                    estado
                    |> Array.map id
                    |> (fun estado -> 
                            estado.[i] <- j
                            (), estado)) estado
        Array.collect mover [| 0..n-1 |]
        |> Array.toList

    let atacan ((x1, y1), (x2, y2)) =
        y1 = y2 ||
        abs (x2 - x1) = abs (y2 - y1)

    let aptitud estado =
        let atacan ((x1, y1), (x2, y2)) =
            if atacan ((x1, y1), (x2, y2))
            then 1.0 else 0.0
        let n = Array.length estado
        let reinas = Array.mapi (fun i y -> (i,y)) estado
        let pares = 
            Array.allPairs reinas reinas
            |> Array.filter (fun (p1,p2) -> p1 <> p2)
            |> Array.distinctBy 
                (fun (p1,p2) -> 
                    if p1 <= p2
                    then (p1,p2)
                    else (p2,p1))
        Array.sumBy atacan pares

    let aptitud' n = aptitud n.estado

    let meta estado =
        aptitud estado = 0.0

    let costo _ _ _ = 0.0

    let problema rnd n =
        {
            inicio = inicio rnd n
            meta = meta
            sucesores = sucesores
            costo = costo
        }

