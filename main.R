F_inv <- function(x) {
  result <- -0.5 * log(1 - x / exp(2))
  return(result)
}

generate_random_values <- function(n) {
  # Generar un array de n elementos
  x <- runif(n, 0, 1)
  
  # Aplicar la función inversa a cada elemento de x_values
  y <- sapply(x, F_inv)
  
  return(y)
}

# Ejemplo de uso con n = 5
n <- 5
result_array <- generate_random_values(n)
print(result_array)

