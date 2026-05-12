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

    let csp n =
        let variables = [0 .. n - 1]
        let dominio = [0 .. n - 1]
        let restricciones = 
            let pares = List.allPairs variables variables
                        |> List.filter (fun (v1,v2) -> v1 <> v2)
                        |> List.distinctBy (fun (v1,v2) -> if v1 <= v2
                                                           then (v1,v2)
                                                           else (v2,v1))
            List.map (fun (v1, v2) -> 
                    let f (estado:CSP.estado<int,int>) =
                        let dom1 = Map.find v1 estado
                        let dom2 = Map.find v2 estado
                        let l1 = List.length dom1
                        let l2 = List.length dom2
                        (l1 > 0 && l2 > 0) &&
                        (l1 > 1 || l2 > 1 ||
                         not (atacan ((v1,dom1.[0]), (v2,dom2.[0]))))
                    CSP.Binaria ((v1, v2), f)) pares
        {CSP.variables = variables
         CSP.dominios  = [for _ in 1 .. n do yield dominio]
         CSP.restricciones = restricciones}

