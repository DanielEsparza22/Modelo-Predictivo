# Implementación del segundo modelo - random forest

# Instalación del paquete
# install.packages("randomForest")

library(randomForest)

# Cargamos el conjunto de datos omitiendo la columna de Registro
ruta <- "C:/Users/danie/SOFTWARE/9NO/SeminarioTesis/Dataset/accidentes_carretera.csv"
accidentes <- read.csv(ruta, row.names = 1)
str(accidentes)
class(accidentes)

normalize <- function(x){return((x-min(x)) / (max(x) - min(x)))}

accidente_norm <- as.data.frame(lapply(accidentes,normalize))

# Dividir Conjuntos de pruebas y de entrenamiento
accident_train <- accidente_norm[1:127, ]
accident_test <- accidente_norm[128:159, ]

summary(accidente_norm$Indice_Accidentalidad)

# Ajustar el modelo de Regresión de Random Forest
modelo_rf <- randomForest(Indice_Accidentalidad ~ Dia + Rango_Hora + Rango_Edad + Genero + Tipo_Camino + Tipo_Vehiculo,
                          data = accident_train)

modelo_rf

# Prediccion
predicciones_rf <- predict(modelo_rf, accident_test)
print(head(predicciones_rf))


# Metricas de desempeño

#Correlacion
#Usamos coorrelación para conocer la coorrelación que hay entre las predicciones contra la real.
cor(predicciones_rf, accident_test$Indice_Accidentalidad)

# ERROR CUADRATICO MEDIO
error_cuadratico_medio <- mean((accident_test$Indice_Accidentalidad - predicciones_rf)^2)
print(paste("Error Cuadratico Medio:",error_cuadratico_medio))

# RECALL
# Definir un umbral para clasificar en categorías de riesgo
umbral <- 0.4  # Ajusta el umbral según tus necesidades

# Convertir las predicciones en clasificación binaria (por encima del umbral o no)
predicciones_binarias <- ifelse(predicciones_rf > umbral, 1, 0)
valores_reales_binarios <- ifelse(accident_test$Indice_Accidentalidad > umbral, 1, 0)

# Calcular el Recall para la categoría de riesgo alto
verdaderos_positivos <- sum(predicciones_binarias == 1 & valores_reales_binarios == 1)
todos_positivos_reales <- sum(valores_reales_binarios == 1)

recall <- verdaderos_positivos / todos_positivos_reales

print(paste("Recall para la categoría de riesgo alto:", recall))

# PRECISION

# Convertir las predicciones en clasificación binaria (por encima del umbral o no)}
umbral_p <- 0.4
predicciones_binarias <- ifelse(predicciones_rf > umbral_p, 1, 0)

# Calcular la Precisión
verdaderos_positivos <- sum(predicciones_binarias == 1 & valores_reales_binarios == 1)
falsos_positivos <- sum(predicciones_binarias == 1 & valores_reales_binarios == 0)

precision <- verdaderos_positivos / (verdaderos_positivos + falsos_positivos)

print(paste("Precisión:", precision))


# MATRIZ DE CONFUSION
predicciones_binarias <- ifelse(predicciones_rf > umbral, 1, 0)
valores_reales_binarios <- ifelse(accident_test$Indice_Accidentalidad > umbral, 1, 0)

# Crear la matriz de confusión
confusion_matrix <- table(Actual = valores_reales_binarios, Predicted = predicciones_binarias)

# Imprimir la matriz de confusión
print("Matriz de Confusión:")
print(confusion_matrix)
