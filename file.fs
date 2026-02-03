let rec length = function
    | []      -> 0        
    | _ :: xs -> 1 + length xs  

printfn "Longitud de [1; 2; 3; 4]: %d" (length [1; 2; 3; 4])