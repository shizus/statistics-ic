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

write_to_csv <- function(n, theta, measurements, interval, interval_length, in_interval) {
  # Obtener el directorio de trabajo actual
  current_dir <- getwd()
  
  # Crear el nombre del archivo usando el formato {n}_{theta}.csv
  filename <- file.path(current_dir, paste0(n, "_", theta, ".csv"))
  
  # Si el archivo no existe creo el encabezado
  if (!file.exists(filename)) {
    file_conn <- file(filename, open = "a")
    header <- "mediciones,a,b,longitud,cobertura"
    
    writeLines(header, filename)
    
    # Cerrar el archivo
    close(file_conn)
  }
  
  # Abrir el archivo en modo de escritura
  file_conn <- file(filename, open = "a")
  
  # Convertir la lista measurements a una cadena. Separo con ;
  measurements_str <- paste(measurements, collapse = ";")
  
  # Escribir una línea en el archivo CSV
  writeLines(paste(measurements_str, interval$a, interval$b, interval_length, in_interval, sep = ","), file_conn)
  
  # Cerrar el archivo
  close(file_conn)
}

# calcula x raya
calculate_x_bar <- function(measurements) {
  result <- sum(measurements) / length(measurements)
  return(result)
}

calculate_with_pivot_1 <- function(measurements) {
  n <- length(measurements)
  x_bar <- calculate_x_bar(measurements)
  a <- -1.96 - 1/2 + 2 * sqrt(n) * x_bar
  b <- 1.96 - 1/2 + 2 * sqrt(n) * x_bar
  
  # Crear el objeto interval con atributos a y b
  result_interval <- list(a = a, b = b)
  
  return(result_interval)
}



# Dado un n y un theta genero las mediciones y calculo para el pivote 1
simulate <- function(n, theta) {
  measurements <- generate_random_values(n, theta)
  interval <- calculate_with_pivot_1(measurements)
  interval_length <- interval$b - interval$a
  
  # Verificar si theta está en el intervalo
  in_interval <- ifelse(theta >= interval$a & theta <= interval$b, 1, 0)
  
  write_to_csv(n, theta, measurements, interval, interval_length, in_interval)
  
}

array_n <- c(10, 30, 100, 1000)
array_theta <- c(2, 5)

# Generar todas las combinaciones de n y theta
combinations <- expand.grid(n = array_n, theta = array_theta)


# Aplicar la función para cada combinación
for (i in 1:nrow(combinations)) {
  n_value <- combinations$n[i]
  theta_value <- combinations$theta[i]
  simulate(n_value, theta_value)
}

