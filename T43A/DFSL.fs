namespace Busqueda

module DFSL =
    open Pila
    let estrategia l =
        {
            vacia = empty
            sacar = pop
            agregar = 
                fun pila n ->
                    if n.profundidad <= l
                    then push pila n
                    else pila
        }

    let key n = n.estado, n.profundidad
    