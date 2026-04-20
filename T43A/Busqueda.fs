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

