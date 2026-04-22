namespace Busqueda

module BFS =
    open Cola
    let estrategia =
        {
            vacia = empty
            sacar = dequeue
            agregar = enqueue
        }

    let key n = n.estado
    