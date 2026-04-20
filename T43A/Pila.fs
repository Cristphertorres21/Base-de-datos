namespace Busqueda

module Pila =
    let empty = []
    let push pila x =
        x :: pila

    let pop pila =
        match pila with
        | [] -> None
        | x :: xs -> Some (x, xs)

