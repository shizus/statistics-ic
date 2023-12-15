F_inv <- function(x, theta) {
  result <- -0.5 * log(exp(-2 * theta) - x / exp(2 * theta))
  return(result)
}

generate_random_values <- function(n, theta) {
  # Generar un array de n elementos
  x <- runif(n, 0, 1)
  
  # Aplicar la función inversa a cada elemento de x_values
  y <- sapply(x, function(x) F_inv(x, theta))
  
  return(y)
}

# calcula x raya
calculate_x_bar <- function(measurements) {
  result <- sum(measurements) / length(measurements)
  return(result)
}

calculate_with_pivote_1 <- function(measurements) {
  n <- length(measurements)
  x_bar <- calculate_x_bar(measurements)
  a <- -1.96 - 1/2 + 2 * sqrt(n) * x_bar
  b <- 1.96 - 1/2 + 2 * sqrt(n) * x_bar
  
  # Crear el objeto interval con atributos a y b
  result_interval <- list(a = a, b = b)
  
  return(result_interval)
}

# Generar todas las combinaciones de n y theta
combinations <- expand.grid(n = array_n, theta = array_theta)


# Función para generar valores aleatorios y aplicar la función inversa
simulate <- function(n, theta) {
  measurements <- generate_random_values(n, theta)
  
}



# Aplicar la función para cada combinación
apply(combinations, 1, function(row) simulate(row["n"], row["theta"]))

