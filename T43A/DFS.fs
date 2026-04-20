namespace Busqueda

module DFS =
    open Pila
    let estrategia =
        {
            vacia = empty
            sacar = pop
            agregar = push
        }
    