odds <- function(a, b = 1, conf.level = 0.95, digits = 3) {
  
  # Validación de la tabla
  a <- as.matrix(a)
  if (!all(dim(a) == c(2, 2))) {
    stop("Cuidado con la longitud: la tabla debe ser de 2 x 2")
  }
  
  # Nombres de filas y columnas (por defecto, genéricos)
  if (is.null(rownames(a))) rownames(a) <- c("Fila 1", "Fila 2")
  if (is.null(colnames(a))) colnames(a) <- c("Columna 1", "Columna 2")
  
  # Si b != 1 se compara fila 2 vs fila 1 (se invierten las filas)
  if (b != 1) a <- a[2:1, ]
  
  z <- qnorm(conf.level + (1 - conf.level) / 2)
  
  n1 <- a[1, 1] + a[1, 2]
  n2 <- a[2, 1] + a[2, 2]
  p1 <- a[1, 1] / n1
  p2 <- a[2, 1] / n2
  
  # Odds ratio
  OR        <- (a[1, 1] * a[2, 2]) / (a[1, 2] * a[2, 1])
  se.log.OR <- sqrt(sum(1 / a))
  
  # Riesgo relativo
  RR        <- p1 / p2
  se.log.RR <- sqrt((1 - p1) / (p1 * n1) + (1 - p2) / (p2 * n2))
  
  resultado <- data.frame(
    Estimacion  = c(OR, RR),
    IC.inf = c(exp(log(OR) - z * se.log.OR),
                    exp(log(RR) - z * se.log.RR)),
    IC.sup = c(exp(log(OR) + z * se.log.OR),
                    exp(log(RR) + z * se.log.RR)),
    row.names   = c("Odds ratio", "Riesgo relativo")
  )
  # resultado$log.estimacion <- log(resultado$Estimacion)
  
  # Salida por pantalla
  cat("\nComparacion:", rownames(a)[1], "vs", rownames(a)[2], "\n")
  cat("Evento (columna de referencia):", colnames(a)[1], "\n")
  cat("Nivel de confianza:", paste0(conf.level * 100, "%"), "\n\n")
  print(round(resultado, digits))
  cat("\n")
  
  invisible(resultado)
}