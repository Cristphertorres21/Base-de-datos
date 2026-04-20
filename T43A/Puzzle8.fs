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


