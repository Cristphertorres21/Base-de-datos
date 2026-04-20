namespace Busqueda 

module IDFSL = 

    let iterativa key problema = 
    let rec loop i=
        match Capitulo3.busquedaGrafo
                (DFSL.estrategia i)
                DFSL.key
                problema with
            |Some n-> Some n
            |None -> loop (i+1)
                       loop 0