F_inv <- function(x, theta) {
  result <- -0.5 * log(1 - x / exp(2 * theta))
  return(result)
}

generate_random_values <- function(n, theta) {
  # Generar un array de n elementos
  x <- runif(n, 0, 1)
  
  # Aplicar la función inversa a cada elemento de x_values
  y <- sapply(x, function(x) F_inv(x, theta))
  
  return(y)
}

# Ejemplo de uso con n = 5
array_n <- c(10, 30, 100, 1000)
array_theta <- c(2, 5)

# Generar todas las combinaciones de n y theta
combinations <- expand.grid(n = array_n, theta = array_theta)

# Función para generar valores aleatorios y aplicar la función inversa
simulate <- function(n, theta) {
  measurements <- generate_random_values(n, theta)
}

# Aplicar la función para cada combinación
apply(combinations, 1, function(row) simulate(row["n"], row["theta"]))

