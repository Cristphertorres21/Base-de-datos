Suponga la siguiente declaración

let f xs ys = List.fold (fun xs x -> x::xs) xs ys

¿Cuál es la evaluación de f [1;2;3] [5; 4]?

Implementa una función longitudFold que calcule el número de elementos de una lista utilizando la función List.fold. No uses recursión explícita ni List.length, debes usar fold.