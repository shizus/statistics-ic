calculate_gamma_interval <- function(n, lambda) {
  # Calcular los valores críticos para acumular el 2.5% y el 97.5% de probabilidad
  lower_value <- qgamma(0.025, shape = n, rate = lambda)
  upper_value <- qgamma(0.975, shape = n, rate = lambda)
  
  # Devolver el intervalo [a, b]
  return(c(lower_value, upper_value))
}

# Configuración del valor fijo de la tasa lambda
lambda <- 2

# Valores de n a considerar
array_n <- c(10, 30, 100, 1000)

# Calcular el intervalo para cada valor de n
for (n in array_n) {
  interval <- calculate_gamma_interval(n, lambda)
  formatted_interval <- paste("Intervalo para n =", n, ":", "[", interval[1], "-", interval[2], "]")
  print(formatted_interval)
}

