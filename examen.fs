type problema<'s, 'a> =
    {
        inicio : 's
        meta   : 's -> bool
        //sucesores : 's -> ('a * 's) list
        sucesores : 's -> list<'a * 's>
        costo : 's -> 'a -> 's -> float
    }

type nodo<'s, 'a> =
    {
        estado :  's
        profundidad : int
        costo_ruta : float
        padre : option<nodo<'s, 'a>>
        accion : option<'a>
    }

type estrategia<'s, 'a, 'd> =
    {
        vacia : 'd
        sacar : 'd -> option<nodo<'s, 'a> * 'd>
        agregar : 'd -> nodo<'s, 'a> -> 'd
    }

module Cola =
    // Frente, Trasera
    let empty = [], []

    let enqueue (frente, atras) x =
        (frente, x :: atras)

    let rec dequeue (frente, atras) =
        match frente, atras with
        | x :: frente, atras -> Some (x, (frente, atras))
        | [], x :: atras -> 
            dequeue (List.rev (x::atras), [])
        | [], [] -> None

module BFS =
    let estrategia =
        {
            vacia = Cola.empty
            sacar = Cola.dequeue
            agregar = Cola.enqueue
        }

    let key n = n.estado
    
module AStar =
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
                fun mapa n -> Map.add (n.costo_ruta + h n, n.estado)
                                      n mapa
        }

    let key h n = n.estado, n.costo_ruta + h n
    
module CostoUniforme =
    let estrategia =
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
                fun mapa n -> Map.add (n.costo_ruta, n.estado)
                                      n mapa
        }
    
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
    
module Pila =
    let empty = []
    let push pila x =
        x :: pila

    let pop pila =
        match pila with
        | [] -> None
        | x :: xs -> Some (x, xs)

module DFSL =
    let estrategia l =
        {
            vacia = Pila.empty
            sacar = Pila.pop
            agregar = 
                fun pila n ->
                    if n.profundidad <= l
                    then Pila.push pila n
                    else pila
        }

    let key n = n.estado, n.profundidad      

module DFS =
    let estrategia =
        {
            vacia = Pila.empty
            sacar = Pila.pop
            agregar = Pila.push
        }

    let key n = n.estado
    
module Capitulo3 =
    let construir_nodo problema =
        {
            estado  = problema.inicio
            profundidad = 0
            costo_ruta = 0.0
            padre = None
            accion = None
        }

    let expand problema n =
        problema.sucesores n.estado
        |> List.map (fun (a, s) ->
            {
                estado  = s
                profundidad = n.profundidad + 1
                costo_ruta = n.costo_ruta + 
                             problema.costo n.estado a s
                padre = Some n
                accion = Some a
            })

    let busquedaArbol estrategia problema =
        let rec loop bolsa =
            match estrategia.sacar bolsa with
            | None -> None
            | Some (n, bolsa) -> 
                if problema.meta n.estado
                then Some n
                else expand problema n
                     |> List.fold estrategia.agregar bolsa
                     |> loop
        construir_nodo problema
        |> estrategia.agregar estrategia.vacia
        |> loop


    let busquedaGrafo estrategia key problema =
        let rec loop (procesados, bolsa) =
            match estrategia.sacar bolsa with
            | None -> None
            | Some (n, bolsa) -> 
                if problema.meta n.estado
                then Some n
                else 
                    if Set.contains (key n) procesados
                    then loop (procesados, bolsa)
                    else
                        expand problema n
                        |> List.fold estrategia.agregar bolsa
                        |> (fun bolsa ->
                                let procesados = Set.add (key n) procesados
                                loop (procesados, bolsa))
        construir_nodo problema
        |> estrategia.agregar estrategia.vacia
        |> (fun bolsa -> loop (Set.empty, bolsa))

    let rec acciones n =
        match n.padre, n.accion with
        | Some p, Some a ->
            acciones p @ [a]
        | _ -> []

module CSP =
    open System
    type estado<'variable,'valor when 'variable: comparison and
                                      'valor   : comparison>
        = Map<'variable, 'valor list>

    type restriccion<'variable,'valor when 'variable: comparison and
                                           'valor   : comparison>
        =  Binaria of ('variable * 'variable) * (estado<'variable,'valor> -> bool)
         | NAria of ('variable list) * (estado<'variable,'valor> -> bool)

    type csp<'variable,'valor when 'variable: comparison and
                                   'valor   : comparison> =
        {variables     : 'variable list
         dominios      : 'valor list list
         restricciones : restriccion<'variable,'valor> list
        }

    let seleccionarInt (rnd : Random) (L : ('a * int) []) =
        let choosen = rnd.Next(Array.sumBy (fun (_, w) -> w) L)
        let rec fld indx (L : ('a * int) []) i =
            let (x, w) = L.[indx]
            let i = i + w
            if choosen < i then x
            else fld (indx + 1) L i
        fld 0 L 0


    let eval r =
        match r with
            | Binaria (_, f) -> f
            | NAria (_, f) -> f

    let destBinaria = function
        | Binaria (r, f) -> Some (r, f)
        | _ -> None

    let inicio (csp : csp<'variable,'valor>) =
        csp.dominios 
        |> List.map2 (fun v dom -> (v, dom)) csp.variables
        |> List.fold (fun m (v,dom) -> Map.add v dom m) Map.empty

    let meta (estado : estado<'variable, 'valor>) =
        Map.forall (fun v dom -> List.length dom = 1) estado

    let degree csp (estado : estado<'variable, 'valor>) var =
        List.fold (fun c r -> match r with
                                | Binaria ((v,v'), _) ->
                                    if (var = v && estado.[ v' ].Length > 1) ||
                                       (var = v' && estado.[ v ].Length > 1)
                                    then c + 1
                                    else c
                                | NAria (vs, _) -> 
                                    if List.exists (fun v -> v = var) vs
                                    then c + 1
                                    else c) 0 csp.restricciones

    let values (estado : estado<'variable, 'valor>) =
        Map.fold (fun sum v dom -> sum + List.length dom) 0 estado

    let ac3 csp estado =
        let incidencias v =
            csp.restricciones
            |> List.choose destBinaria
            |> List.choose (fun ((p, q), f) -> 
                    if v = p
                    then Some (Binaria ((p, q), f), (q, v))
                    elif v = q
                    then Some (Binaria ((p, q), f), (p, v))
                    else None)
        let remover estado (restriccion, (v,v')) =
            estado |> Map.find v
                   |> List.fold (fun (modificado, dom) x ->
                            let estado' = Map.add v [x] estado
                            if List.exists (fun y -> let estado'' = Map.add v' [y] estado'
                                                     eval restriccion estado'')
                                           (Map.find v' estado)
                            then (modificado, x::dom)
                            else (true, dom)) (false, [])
        let rec ac3_aux estado restricciones =
            match restricciones with
                | (r, (v,v')) :: restricciones ->
                    let (modificado, dom) = remover estado (r, (v,v'))
                    if modificado
                    then let estado = Map.add v dom estado
                         let arcos = incidencias v
                         ac3_aux estado (arcos @ restricciones)
                    else ac3_aux estado restricciones
                | [] -> estado
        csp.restricciones
            |> List.collect (fun r -> match r with
                                        | Binaria ((p,q), _) ->
                                            [(r, (p,q)); (r, (q,p))]
                                        | NAria _ -> [])
            |> ac3_aux estado

    let sucesores (csp : csp<'variable,'valor>) (estado : estado<'variable, 'valor>) =
        Map.toList estado
        |> List.filter (fun (_, dom) -> List.length dom > 1)
        |> (fun l -> match l with
                     | _ :: _ -> 
                         // Minimum remaining values and
                         // Degree heuristic
                         l |> List.minBy (fun (var, dom) -> (List.length dom, 
                                                             -degree csp estado var))
                           // Arc consistency
                           |> (fun (var, dom) -> 
                               List.choose (fun v -> 
                                    let s = estado |> Map.add var [v]
                                                   |> ac3 csp
                                    if not (Map.exists (fun _ vs -> List.isEmpty vs) s) &&
                                       List.forall (fun r -> eval r s) csp.restricciones
                                    then Some ((), s)
                                    else None) dom)
                           // Least constraining value
                           |> List.sortBy (fun (_, s) -> - values s)
                     | _ -> [])

    let backtracking (csp : csp<'variable,'valor>) =
        let problema = 
            {inicio = inicio csp
             meta = meta
             sucesores = sucesores csp
             costo = fun _ _ _ -> 1.0
            } : problema<estado<'variable,'valor>, unit>
        Capitulo3.busquedaGrafo DFS.estrategia DFS.key problema

    let minimosConflictos (csp : csp<'variable,'valor>) max_iteraciones =
        let rnd = System.Random()
        let n = List.length csp.variables
        let estado = List.map2 (fun var dom -> (var, [List.item (rnd.Next(n)) dom]))
                               csp.variables
                               csp.dominios
                     |> Map.ofList
        let vars_dom = List.map2 (fun v dom -> (v, dom)) csp.variables csp.dominios
                       |> Map.ofList
        let h estado = List.sumBy (fun r -> if eval r estado then 0 else 1)
                                  csp.restricciones
        let rec loop i estado =
            if i < max_iteraciones
            then if List.forall (fun r -> eval r estado) csp.restricciones
                 then Some estado
                 else let var = List.filter (fun r -> not (eval r estado)) csp.restricciones
                                |> List.choose destBinaria
                                |> List.collect ((fun (v1,v2) -> [v1;v2]) << fst)
                                |> List.countBy id
                                |> (seleccionarInt rnd << List.toArray)
                      let dom = vars_dom.[ var ]
                      let valor = List.minBy (fun v -> h (Map.add var [v] estado)) dom
                      loop (i + 1) (Map.add var [valor] estado)
            else None
        loop 0 estado

    let minimosConflictosReinicio (csp : csp<'variable,'valor>) max_iteraciones =
        let rec loop i =
            printfn "%i" i
            match minimosConflictos csp max_iteraciones with
            | Some estado -> estado
            | None -> loop (i + 1)
        loop 0

let busqueda (p: problema<'s, 'a>) (e: estrategia<'s, 'a, 'd>) =
    let nodoRaiz = { estado = p.inicio; profundidad = 0; costo_ruta = 0.0; padre = None; accion = None }
    
    let rec expandir (frontera: 'd) (visitados: Set<'s>) =
        match e.sacar frontera with
        | None -> None
        | Some (nodo, restoFrontera) ->
            if p.meta nodo.estado then
                Some nodo
            else if Set.contains nodo.estado visitados then
                expandir restoFrontera visitados
            else
                let sucesores = p.sucesores nodo.estado
                let nuevosNodos = 
                    sucesores |> List.map (fun (accion, nuevoEstado) ->
                        { estado = nuevoEstado
                          profundidad = nodo.profundidad + 1
                          costo_ruta = nodo.costo_ruta + p.costo nodo.estado accion nuevoEstado
                          padre = Some nodo
                          accion = Some accion })
                
                let nuevaFrontera = List.fold e.agregar restoFrontera nuevosNodos
                expandir nuevaFrontera (Set.add nodo.estado visitados)

    expandir (e.agregar e.vacia nodoRaiz) Set.empty

let rec reconstruirCamino nodo =
    match nodo.padre with
    | None -> [nodo.estado]
    | Some p -> (reconstruirCamino p) @ [nodo.estado]

let viajar (inicioCiudad: string) : string list =
    // Datos del Mapa
    let conexiones = 
        [ "Arad", [("Zerind", 75.0); ("Sibiu", 140.0); ("Timisoara", 118.0)]
          "Zerind", [("Arad", 75.0); ("Oradea", 71.0)]
          "Oradea", [("Zerind", 71.0); ("Sibiu", 151.0)]
          "Sibiu", [("Arad", 140.0); ("Oradea", 151.0); ("Fagaras", 99.0); ("Rimnicu Vilcea", 80.0)]
          "Timisoara", [("Arad", 118.0)]
          "Fagaras", [("Sibiu", 99.0)]
          "Rimnicu Vilcea", [("Sibiu", 80.0)] ] |> Map.ofList

    let tablaHeuristica = 
        [ "Arad", 366.0; "Zerind", 374.0; "Oradea", 380.0; "Sibiu", 253.0; 
          "Timisoara", 329.0; "Fagaras", 176.0; "Rimnicu Vilcea", 193.0 ] |> Map.ofList

    // Definición del problema según tu tipo 'problema'
    let probCiudades = {
        inicio = inicioCiudad
        meta = fun s -> s = "Sibiu"
        sucesores = fun s -> 
            match Map.tryFind s conexiones with
            | Some lista -> lista |> List.map (fun (destino, _) -> (destino, destino)) // accion es el nombre de la ciudad
            | None -> []
        costo = fun s1 _ s2 -> 
            let lista = Map.find s1 conexiones
            let (_, c) = List.find (fun (dest, _) -> dest = s2) lista
            c
    }

    // Definición de la heurística para el nodo
    let h (n: nodo<string, string>) = 
        Map.find n.estado tablaHeuristica

    // Ejecución usando AStar
    match busqueda probCiudades (AStar.estrategia h) with
    | Some nodoFinal -> reconstruirCamino nodoFinal
    | None -> []

 // Etsamwn pasado