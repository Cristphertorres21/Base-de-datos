namespace Busqueda

module Puzle8 =

    // Definir estados y acciones
    type estado = int * int * int *
                  int * int * int *
                  int * int * int
    type accion = 
        | Left
        | Right
        | Up
        | Down
    let estado_inicial = (7, 2, 4, 5, 0, 6, 8, 3, 1)
    let estado_inicial_02 = (1, 2, 3, 4, 0, 6, 7, 5, 8)
    let estado_inicial_14 = (0, 3, 6, 2, 1, 7, 5, 4, 8)
    let estado_inicial_20 = (7, 2, 4, 5, 0, 6, 8, 3, 1)
    let estado_inicial_23 = (5, 2, 4, 1, 3, 0, 6, 8, 7)

    let costo _ _ _ = 1.0

    let meta x = 
        x = (1,2,3,4,5,6,7,8,0)

    let sucesores estado =
      match estado with
        | (0, x2, x3, x4, x5, x6, x7, x8, x9) -> 
            [(Right, (x2, 0, x3, x4, x5, x6, x7, x8, x9))
             (Down, (x4, x2, x3, 0, x5, x6, x7, x8, x9))]
        | (x1, 0, x3, x4, x5, x6, x7, x8, x9) -> 
            [(Left, (0, x1, x3, x4, x5, x6, x7, x8, x9))
             (Right, (x1, x3, 0, x4, x5, x6, x7, x8, x9))
             (Down, (x1, x5, x3, x4, 0, x6, x7, x8, x9))]
        | (x1, x2, 0, x4, x5, x6, x7, x8, x9) -> 
            [(Left, (x1, 0, x2, x4, x5, x6, x7, x8, x9))
             (Down, (x1, x2, x6, x4, x5, 0, x7, x8, x9))]
        | (x1, x2, x3, 0, x5, x6, x7, x8, x9) -> 
            [(Up, (0, x2, x3, x1, x5, x6, x7, x8, x9))
             (Right, (x1, x2, x3, x5, 0, x6, x7, x8, x9))
             (Down, (x1, x2, x3, x7, x5, x6, 0, x8, x9))]
        | (x1, x2, x3, x4, 0, x6, x7, x8, x9) -> 
            [(Up, (x1, 0, x3, x4, x2, x6, x7, x8, x9))
             (Left, (x1, x2, x3, 0, x4, x6, x7, x8, x9))
             (Right, (x1, x2, x3, x4, x6, 0, x7, x8, x9))
             (Down, (x1, x2, x3, x4, x8, x6, x7, 0, x9))]
        | (x1, x2, x3, x4, x5, 0, x7, x8, x9) -> 
            [(Up, (x1, x2, 0, x4, x5, x3, x7, x8, x9))
             (Left, (x1, x2, x3, x4, 0, x5, x7, x8, x9))
             (Down, (x1, x2, x3, x4, x5, x9, x7, x8, 0))]
        | (x1, x2, x3, x4, x5, x6, 0, x8, x9) -> 
            [(Up, (x1, x2, x3, 0, x5, x6, x4, x8, x9))
             (Right, (x1, x2, x3, x4, x5, x6, x8, 0, x9))]
        | (x1, x2, x3, x4, x5, x6, x7, 0, x9) -> 
            [(Left, (x1, x2, x3, x4, x5, x6, 0, x7, x9))
             (Up, (x1, x2, x3, x4, 0, x6, x7, x5, x9))
             (Right, (x1, x2, x3, x4, x5, x6, x7, x9, 0))]
        | (x1, x2, x3, x4, x5, x6, x7, x8, 0) -> 
            [(Left, (x1, x2, x3, x4, x5, x6, x7, 0, x8))
             (Up, (x1, x2, x3, x4, x5, 0, x7, x8, x6))]
        | _ -> []



    let tupla_a_lista (x1,x2,x3,x4,x5,x6,x7,x8,x9) =
        [x1;x2;x3;x4;x5;x6;x7;x8;x9]
    let meta_lista = tupla_a_lista (1,2,3,4,5,6,7,8,0)
    let h1 n =
        let estado_lista = 
            n.estado
            |> tupla_a_lista
        List.zip estado_lista meta_lista
        |> List.sumBy (fun (x,x') -> 
            if x = 0
            then 0.0
            else if x <> x' then 1.0 else 0.0)

    let coordenadas i =
        (i % 3, i / 3)

    let man (x1,y1) (x2,y2) =
        abs (x2-x1) + abs (y2-y1)

    let h2 n =
        n.estado
        |> tupla_a_lista
        |> List.indexed
        |> List.sumBy (fun (i,x) -> 
            if x <> 0 
            then man (coordenadas i) (coordenadas (x-1))
            else 0)
        |> float

    let problema inicial =
        {
            inicio = inicial
            sucesores = sucesores
            meta = meta
            costo = costo
        }
