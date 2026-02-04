Suponga la siguiente declaración

let f xs ys = List.fold (fun xs x -> x::xs) xs ys

¿Cuál es la evaluación de f [1;2;3] [5; 4]?

Implementa una función longitudFold que calcule el número de elementos de una lista utilizando la función List.fold. No uses recursión explícita ni List.length, debes usar fold.


// 1. Función f del enunciado
let f xs ys = List.fold (fun xs x -> x::xs) xs ys

// Evaluación de f [1;2;3] [5;4]
let resultadoF = f [1;2;3] [5;4]
printfn "f [1;2;3] [5;4] = %A" resultadoF  // Output: [4;5;1;2;3]

// 2. Función longitudFold usando List.fold
let longitudFold lista =
    List.fold (fun contador _ -> contador + 1) 0 lista

// Versión alternativa con pipe operator
let longitudFold2 lista =
    lista |> List.fold (fun acc _ -> acc + 1) 0

// Tests de longitudFold
printfn "\nTests de longitudFold:"
printfn "longitudFold [] = %d" (longitudFold [])  // 0
printfn "longitudFold [1] = %d" (longitudFold [1])  // 1
printfn "longitudFold [1;2;3] = %d" (longitudFold [1;2;3])  // 3
printfn "longitudFold ['a';'b';'c';'d'] = %d" (longitudFold ['a';'b';'c';'d'])  // 4
printfn "longitudFold [\"hola\"; \"mundo\"] = %d" (longitudFold ["hola"; "mundo"])  // 2

// También puedes probar la versión 2
printfn "\nUsando longitudFold2:"
printfn "longitudFold2 [1;2;3;4;5] = %d" (longitudFold2 [1;2;3;4;5])  // 5

// Demostración del proceso de fold
printfn "\nDemostración del proceso:"
let listaEjemplo = [10; 20; 30]
let pasoAPaso = 
    listaEjemplo 
    |> List.fold (fun acc elem -> 
        printfn "Acumulador: %d, Elemento: %d -> Nuevo acumulador: %d" acc elem (acc + 1)
        acc + 1) 0
printfn "Resultado final: %d" pasoAPaso