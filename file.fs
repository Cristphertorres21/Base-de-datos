
// Factorial y Power (Recursividad numérica)
let rec factorial = function
    | 0 -> 1
    | n -> n * factorial(n - 1)

let rec power = function
    | (_, 0) -> 1.0
    | (x, n) -> x * power(x, n - 1)

// Remove (Recursividad con listas y descomposicion)
let rec remove x list =
    match list with
    | [] -> [] // Caso base: lista vacía
    | b :: t -> 
        if x = b then remove x t     
        else b :: remove x t         

// BLOQUE 2: LIST.MAP 
// Lo usamos para aplicar una función a cada elemento
let elevarAlCuadrado lista = 
    List.map (fun x -> power(float x, 2)) lista


// BLOQUE 3: LIST.FOLD (Reducción / Acumulador de Izq a Der)
// Lo usamos para obtener un solo valor (Suma, Producto, etc.)
// State T (acc) es un número
let sumarFactoriales lista = 
    List.fold (fun acc x -> acc + factorial x) 0 lista

let evaluarPolinomio coef x =
    List.foldBack (fun c acc -> c + x * acc) coef 0.0


printfn "--- RECURSIVIDAD ---"
printfn "Factorial de 5: %d" (factorial 5)
printfn "Potencia 2.0^3: %f" (power(2.0, 3))

let listaDatos = [1; 2; 3; 4; 5]
printfn "\n--- OPERACIONES CON LISTAS ---"
printfn "Original: %A" listaDatos
printfn "Sin el numero 3: %A" (remove 3 listaDatos)
printfn "Cada uno al cuadrado (Map): %A" (elevarAlCuadrado listaDatos)
printfn "Suma de sus factoriales (Fold): %d" (sumarFactoriales [1; 2; 3])

printfn "\n--- POLINOMIOS (PDF) ---"
let miPoli = [2.0; 0.0; 3.0] // 2 + 0x + 3x^2
printfn "Polinomio %A evaluado en x=2: %f" miPoli (evaluarPolinomio miPoli 2.0)