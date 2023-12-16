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

remove_file <- function(pivot_name, n, theta) {
  # Obtener el directorio de trabajo actual
  current_dir <- getwd()
  
  # Crear el nombre del archivo usando el formato {pivot_name}_{theta}.csv
  filename <- file.path(current_dir, paste0(pivot_name, "_", n, "_", theta, ".csv"))
  
  # Borrar el archivo si existe
  if (file.exists(filename)) {
    file.remove(filename)
    cat("Archivo borrado:", filename, "\n")
  } else {
    cat("El archivo no existe:", filename, "\n")
  }
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



# Dado un n y un theta genero las mediciones y calculo para el pivote 1
simulate <- function(measurements, n, theta, pivot_func, pivot_name) {
  interval <- pivot_func(measurements)
  interval_length <- interval$b - interval$a
  
  # Verificar si theta está en el intervalo
  in_interval <- ifelse(theta >= interval$a & theta <= interval$b, 1, 0)
  
  write_to_csv(pivot_name, n, theta, measurements, interval, interval_length, 
               in_interval)
  
  result_simulation <- list(pivot_name = pivot_name, 
                            n = n, 
                            theta = theta, 
                            interval_length = interval_length, 
                            in_interval = in_interval)
  return(result_simulation)
  
}

save_dataframe_as_csv <- function(df_simulation_statistics) {
  # Guardo todo
  write.csv(df_simulation_statistics, file = "full_results.csv", row.names = FALSE)
  
  # Creo un dataframe sin la columna 'coverage'
  df_punto_3 <- df_simulation_statistics
  df_punto_3$coverage <- NULL
  
  write.csv(df_punto_3, file = "punto_3.csv", row.names = FALSE)
  
  #  Creo un dataframe sin la columna 'expected_length
  df_punto_4 <- df_simulation_statistics
  df_punto_4$expected_length <- NULL
  
  write.csv(df_punto_4, file = "punto_4.csv", row.names = FALSE)
  
}

save_and_show_results <- function(simulation_statistics) {
  
  df_simulation_statistics <- do.call(rbind, simulation_statistics) 
  # Mostrar la tabla por pantalla
  print(df_simulation_statistics)
  
  cat("Punto 3\n")
  cat("pivot, n, theta, expected_length\n")
  
  for (i in seq_len(nrow(df_simulation_statistics))) {
    cat(
      df_simulation_statistics[i, "pivot"]$pivot, ", ",
      df_simulation_statistics[i, "n"]$n, ", ",
      df_simulation_statistics[i, "theta"]$theta, ", ",
      df_simulation_statistics[i, "expected_length"]$expected_length, "\n"
    )
  }
  
  cat("Punto 4\n")
  cat("pivot, n, theta, coverage\n")
  
  for (i in seq_len(nrow(df_simulation_statistics))) {
    cat(
      df_simulation_statistics[i, "pivot"]$pivot, ", ",
      df_simulation_statistics[i, "n"]$n, ", ",
      df_simulation_statistics[i, "theta"]$theta, ", ",
      df_simulation_statistics[i, "coverage"]$coverage, "\n"
    )
  }
  
  save_dataframe_as_csv(df_simulation_statistics)
  
}

array_n <- c(10, 30, 100, 1000)
array_theta <- c(2, 5)

# Generar todas las combinaciones de n y theta
combinations <- expand.grid(n = array_n, theta = array_theta)

simulation_statistics = list()

# Aplicar la función para cada combinación
for (i in 1:nrow(combinations)) {
  
  pivot_accum = list(pivot_1 = list(length = 0, in_interval_count = 0),
                     pivot_2 = list(length = 0, in_interval_count = 0),
                     pivot_3 = list(length = 0, in_interval_count = 0))
  
  
  n_value <- combinations$n[i]
  theta_value <- combinations$theta[i]
  
  # Borro el csv para empezar de cero
  remove_file("pivot_1", n_value, theta_value)
  remove_file("pivot_2", n_value, theta_value)
  remove_file("pivot_3", n_value, theta_value)
  
  # Por cada n y tita simulo K veces
  for (j in 1:K) {
    measurements <- generate_random_values(n_value, theta_value)
    result_pivot_1 <- simulate(measurements, n_value, theta_value, pivot_1, "pivot_1")
    result_pivot_2 <- simulate(measurements, n_value, theta_value, pivot_2, "pivot_2")
    result_pivot_3 <- simulate(measurements, n_value, theta_value, pivot_3, "pivot_3")
    
    pivot_accum$pivot_1$length <- pivot_accum$pivot_1$length + 
      result_pivot_1$interval_length
    pivot_accum$pivot_1$in_interval_count <- pivot_accum$pivot_1$in_interval_count +
      result_pivot_1$in_interval
    
    
    pivot_accum$pivot_2$length <- pivot_accum$pivot_2$length + 
      result_pivot_2$interval_length
    pivot_accum$pivot_2$in_interval_count <- pivot_accum$pivot_2$in_interval_count +
      result_pivot_2$in_interval
    
    
    pivot_accum$pivot_3$length <- pivot_accum$pivot_3$length +
     result_pivot_3$interval_length
    pivot_accum$pivot_3$in_interval_count <- pivot_accum$pivot_3$in_interval_count +
     result_pivot_3$in_interval
    
  }
  
  new_statistic <- list(
    pivot= "pivot_1",
    n = n_value,
    theta = theta_value,
    expected_length = pivot_accum$pivot_1$length / K,
    coverage = pivot_accum$pivot_1$in_interval_count / K
    )
  
  simulation_statistics <- c(simulation_statistics, list(new_statistic))
  
  new_statistic <- list(
    pivot= "pivot_2",
    n = n_value,
    theta = theta_value,
    expected_length = pivot_accum$pivot_2$length / K,
    coverage = pivot_accum$pivot_2$in_interval_count / K
    )
  
  simulation_statistics <- c(simulation_statistics, list(new_statistic))
  
  
  new_statistic <- list(
    pivot= "pivot_3",
    n = n_value,
    theta = theta_value,
    expected_length = pivot_accum$pivot_3$length / K,
    coverage = pivot_accum$pivot_3$in_interval_count / K
    )

  simulation_statistics <- c(simulation_statistics, list(new_statistic))
  
}

save_and_show_results(simulation_statistics)

print("Terminado exitosamente.")

