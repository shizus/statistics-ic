K = 5000

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

write_to_csv <- function(pivot_name, n, theta, measurements, interval, interval_length, in_interval) {
  # Obtener el directorio de trabajo actual
  current_dir <- getwd()
  
  # Crear el nombre del archivo usando el formato {n}_{theta}.csv
  filename <- file.path(current_dir, paste0(pivot_name, "_", n, "_", theta, ".csv"))
  
  # Si el archivo no existe creo el encabezado
  if (!file.exists(filename)) {
    file_conn <- file(filename, open = "a")
    # header <- "mediciones,a,b,longitud,cobertura"
    header <- "a,b,longitud,cobertura"
    
    writeLines(header, filename)
    
    # Cerrar el archivo
    close(file_conn)
  }
  
  # Abrir el archivo en modo de escritura
  file_conn <- file(filename, open = "a")
  
  # Convertir la lista measurements a una cadena. Separo con ;
  # measurements_str <- paste(measurements, collapse = ";")
  
  # Escribir una línea en el archivo CSV
  # writeLines(paste(measurements_str, interval$a, interval$b, interval_length, in_interval, sep = ","), file_conn)
  writeLines(paste(interval$a, interval$b, interval_length, in_interval, sep = ","), file_conn)
  
  # Cerrar el archivo
  close(file_conn)
}

# calcula x raya
calculate_x_bar <- function(measurements) {
  result <- sum(measurements) / length(measurements)
  return(result)
}

pivot_1 <- function(measurements) {
  n <- length(measurements)
  x_bar <- calculate_x_bar(measurements)
  a <- (-1.96 / (2 * sqrt(n)) ) - 1/2 +  x_bar
  b <- (1.96 / (2 * sqrt(n)) ) - 1/2 +  x_bar
  
  # Crear el objeto interval con atributos a y b
  result_interval <- list(a = a, b = b)
  
  return(result_interval)
}

get_interval_by_n <- function(n) {
  # valores calculados en gamma_95.R
  gamma_by_n_intervals <- list(
    list(n = 10, a = 2.39769434806622, b = 8.54240172570958),
    list(n = 30, a = 10.1204370107105, b = 20.8244187192933),
    list(n = 100, a = 40.6819956254616, b = 60.2644738765777),
    list(n = 1000, a = 469.486509203848, b = 531.460575612444)
  )
  
  # Buscar la lista correspondiente al valor de n
  result_interval <- NULL
  for (element in gamma_by_n_intervals) {
    if (element$n == n) {
      result_interval <- list(a=element$a, b=element$b)
      break
    }
  }
  
  # Devolver la lista correspondiente o NULL si no se encuentra
  return(result_interval)
}

pivot_2 <- function(measurements) {
  n <- length(measurements)
  x_bar <- calculate_x_bar(measurements)
  
  interval <- get_interval_by_n(n)
  a_n <- interval$a
  b_n <- interval$b
  
  a <- (-b_n) / n + x_bar
  b <- (-a_n) / n + x_bar
  
  # Crear el objeto interval con atributos a y b
  result_interval <- list(a = a, b = b)
  
  return(result_interval)
}

pivot_3 <- function(measurements) {
  # Placeholder para función no implementada
  return(NULL)
}



# Dado un n y un theta genero las mediciones y calculo para el pivote 1
simulate <- function(measurements, n, theta, pivot_func, pivot_name) {
  interval <- pivot_func(measurements)
  interval_length <- interval$b - interval$a
  
  # Verificar si theta está en el intervalo
  in_interval <- ifelse(theta >= interval$a & theta <= interval$b, 1, 0)
  
  write_to_csv(pivot_name, n, theta, measurements, interval, interval_length, in_interval)
  
}

array_n <- c(10, 30, 100, 1000)
array_theta <- c(2, 5)

# Generar todas las combinaciones de n y theta
combinations <- expand.grid(n = array_n, theta = array_theta)


# Aplicar la función para cada combinación
for (i in 1:nrow(combinations)) {
  n_value <- combinations$n[i]
  theta_value <- combinations$theta[i]
  # Por cada n y tita simulo k veces
  for (j in 1:K) {
    measurements <- generate_random_values(n_value, theta_value)
    simulate(measurements, n_value, theta_value, pivot_1, "pivot_1")
    simulate(measurements, n_value, theta_value, pivot_2, "pivot_2")
    # simulate(measurements, n_value, theta_value, pivot_3, "pivot_3")
  }
}

