namespace Busqueda

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


