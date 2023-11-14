# Implementación del primero modelo - regresión lineal

# Cargamos el conjunto de datos omitiendo la columna de Registro
ruta <- "C:/Users/danie/SOFTWARE/9NO/SeminarioTesis/Dataset/accidentes_carretera.csv"
accidentes <- read.csv(ruta, row.names = 1)
str(accidentes)
class(accidentes)

"Indice_Accidentalidad" %in% names(accidentes)

# Visualizar las primeras filas del conjunto de datos
head(accidentes)

# Tipo de variables correcto
accidentes$Dia <- as.factor(accidentes$Dia)
accidentes$Rango_Hora <- as.factor(accidentes$Rango_Hora)
accidentes$Rango_Edad <- as.factor(accidentes$Rango_Edad)
accidentes$Genero <- as.factor(accidentes$Genero)
accidentes$Tipo_Camino <- as.factor(accidentes$Tipo_Camino)
accidentes$Tipo_Vehiculo <- as.factor(accidentes$Tipo_Vehiculo)
accidentes$Indice_Accidentalidad <- as.numeric(accidentes$Indice_Accidentalidad)

# Dividir Conjuntos de pruebas y de entrenamiento
set.seed(123)
indices_entrenamiento <- sample(1:nrow(accidentes), 0.8 * nrow(accidentes))
conjunto_entrenamiento <- accidentes[indices_entrenamiento, ]
conjunto_prueba <- accidentes[-indices_entrenamiento, ]

# Implementación del Modelo de regresión lineal
modelo <- lm(Indice_Accidentalidad ~ Dia + Rango_Hora + Rango_Edad + Genero + Tipo_Camino + Tipo_Vehiculo, data = conjunto_entrenamiento)

summary(modelo)
  

# Predicciones en el conjunto de prueba
predicciones <- predict(modelo, newdata = conjunto_prueba)

# Métricas de rendimiento

#ERROR CUADRATICO MEDIO
error_cuadratico_medio <- mean((conjunto_prueba$Indice_Accidentalidad - predicciones)^2)
print(paste("Error Cuadratico Medio:",error_cuadratico_medio))

# ACCURACY
umbral <- 0.7

# Convertir las predicciones en clasificación binaria
predicciones_binarias <- ifelse(predicciones > umbral, 1, 0)

verdad <- conjunto_prueba$Indice_Accidentalidad

# Calcular el accuracy
accuracy <- sum(predicciones_binarias == verdad) / length(verdad)

print(paste("Accuracy:", accuracy))

# PRECISION
umbral_prec <- 0.5

# Convertir las predicciones en clasificación binaria
predicciones_binarias_prec <- ifelse(predicciones > umbral_prec, 1, 0)
predicciones_binarias_prec

verdad_prec <- conjunto_prueba$Indice_Accidentalidad

# Calcular los componentes de la precisión
verdaderos_positivos <- sum(predicciones_binarias_prec == 1 & verdad_prec == 1)
falsos_positivos <- sum(predicciones_binarias_prec == 1 & verdad_prec == 0)

# Calcular la precisión
precision <- verdaderos_positivos / (verdaderos_positivos + falsos_positivos)
print(paste("Precision:", precision))

# RECALL
umbral_recall <- 0.5

# Convertir las predicciones en clasificación binaria
predicciones_binarias_recall <- ifelse(predicciones > umbral_recall, 1, 0)

verdad_recall <- conjunto_prueba$Indice_Accidentalidad

# Calcular los componentes del recall
verdaderos_positivos_recall <- sum(predicciones_binarias_recall == 1 & verdad_recall == 1)
falsos_negativos_recall <- sum(predicciones_binarias_recall == 0 & verdad_recall == 1)

# Calcular el recall
recall <- verdaderos_positivos_recall / (verdaderos_positivos_recall + falsos_negativos_recall)
print(paste("Recall:", recall))

# MATRIZ DE CONFUSION

umbral_mc <- 0.6

# Convertir las predicciones en clasificación binaria
predicciones_binarias_mc <- ifelse(predicciones > umbral_mc, 1, 0)

verdad_mc <- conjunto_prueba$Indice_Accidentalidad

# Calcular la matriz de confusión
matriz_confusion <- table(Real = verdad_mc, Prediccion = predicciones_binarias)
print("Matriz de Confusión:")
print(matriz_confusion)
  
  

