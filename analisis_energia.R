# Paso 1: Configuración inicial

# Vectores de tipo de energía
energia <- c(rep("Renovable", 10), rep("No Renovable", 10))

# Vectores de consumo en kWh (con algunos valores NA)
set.seed(123)  # Para reproducibilidad
consumo <- c(runif(5, 10, 50), rep(NA, 3), runif(5, 10, 50), rep(NA, 2))

# Vectores de costo por kWh
costo_kwh <- c(rep(0.15, 10), rep(0.20, 10))

# Paso 2: Limpieza de datos
# Reemplazamos los valores NA en consumo por la mediana de cada tipo de energía
for (tipo in unique(energia)) {
  indices <- which(energia == tipo)
  mediana_consumo <- median(consumo[indices], na.rm = TRUE)
  consumo[indices][is.na(consumo[indices])] <- mediana_consumo
}

# Paso 3: Crear el dataframe
df_consumo <- data.frame(energia = energia, consumo = consumo, costo_kwh = costo_kwh)

# Paso 4: Cálculos

# Calcular el costo total (consumo * costo por kWh)
df_consumo$costo_total <- df_consumo$consumo * df_consumo$costo_kwh

# Calcular el total de consumo y costo total por tipo de energía
total_consumo <- tapply(df_consumo$consumo, df_consumo$energia, sum)
total_costo <- tapply(df_consumo$costo_total, df_consumo$energia, sum)

# Calcular la media del consumo diario para cada tipo de energía
media_consumo <- tapply(df_consumo$consumo, df_consumo$energia, mean)

# Agregar la columna ganancia (simulación de aumento del 10%)
df_consumo$ganancia <- df_consumo$costo_total * 1.1

# Paso 5: Resumen

# Ordenar el dataframe por costo_total en orden descendente
df_consumo_ordenado <- df_consumo[order(-df_consumo$costo_total), ]

# Calcular el total de consumo energético por tipo de energía
total_consumo_energia <- tapply(df_consumo$consumo, df_consumo$energia, sum)

# Calcular el costo total por tipo de energía
total_costo_energia <- tapply(df_consumo$costo_total, df_consumo$energia, sum)

# Extraer las tres filas con el mayor costo_total
top_3_costos <- head(df_consumo_ordenado, 3)

# Crear la lista resumen_energia
resumen_energia <- list(
  total_consumo_energia = total_consumo_energia,
  total_costo_energia = total_costo_energia,
  top_3_costos = top_3_costos,
  media_consumo = media_consumo
)

# Mostrar el resumen
print(resumen_energia)
