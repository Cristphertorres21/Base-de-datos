namespace Busqueda

(* Variables de tipo 's: estado y 'a: acción
*)
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


module Capitulo4 =
    open Capitulo3

    let ascensionColinas h problema =
        let current = construir_nodo problema
        let rec loop current =
            let neighbor = 
                expand problema current
                |> List.maxBy h
            if h neighbor <= h current
            then Some current
            else loop neighbor
        loop current

    let temperatura k lam iteraciones t =
        if t < iteraciones
        then k * System.Math.Exp (-lam*t)
        else 0.0

    let recocidoSimulado schedule h problema =
        let rnd = System.Random()
        let current = construir_nodo problema
        let rec loop (t, current) =
            let T = schedule t
            if T = 0.0
            then if problema.meta current.estado
                 then Some current
                 else None
            else 
                let succs = 
                    expand problema current
                let next = succs.[rnd.Next succs.Length]
                let deltaE = h next - h current
                if deltaE > 0.0 ||
                   rnd.NextDouble() <= System.Math.Exp (deltaE / T)
                then loop (t + 1.0, next)
                else loop (t + 1.0, current)
        loop (0.0, current)