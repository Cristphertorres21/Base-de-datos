// 1. Definimos los tipos de datos (Modelado de dominio)
type Producto = {
    Nombre: string
    Precio: decimal
}

type Descuento =
    | Porcentaje of decimal
    | ValorFijo of decimal
    | SinDescuento

// 2. Función para calcular el precio final usando Pattern Matching
let calcularPrecioFinal producto descuento =
    match descuento with
    | Porcentaje p -> producto.Precio * (1.0m - p)
    | ValorFijo v -> producto.Precio - v
    | SinDescuento -> producto.Precio

// 3. Creamos algunos datos
let miLaptop = { Nombre = "Laptop Pro"; Precio = 1000.0m }
let ofertaBlackFriday = Porcentaje 0.20m // 20% de descuento

// 4. Ejecución y salida
let precioFinal = calcularPrecioFinal miLaptop ofertaBlackFriday

printfn "Producto: %s" miLaptop.Nombre
printfn "Precio Original: $%M" miLaptop.Precio
printfn "Precio con Descuento: $%M" precioFinal