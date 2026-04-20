namespace Busqueda

module Cola =
    let empty = []
    let enqueue cola x =
        x :: cola

    let dequeue cola =
        match List.tryLast cola with
        | None -> None
        | Some x -> Some (x, List.take (List.length cola - 1) 
                                cola)

