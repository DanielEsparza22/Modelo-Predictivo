# Implementación del primer modelo - redes neuronales

setwd("C:/Users/danie/SOFTWARE/9NO/SeminarioTesis/Dataset")
accidente <- read.csv("accidentes_carretera.csv",row.names = 1)

#Estructura del dataset
str(accidente)

#Creamos y aplciamos una función de normalización de datos
normalize <- function(x){return((x-min(x)) / (max(x) - min(x)))}

accidente_norm <- as.data.frame(lapply(accidente,normalize))

#Dividimos los dataset en 80% para entrenar el set y 20% para el set de pruebas
accident_train <- accidente_norm[1:127, ]
accident_test <- accidente_norm[128:159, ]

summary(accidente_norm$Indice_Accidentalidad)

#Entrenar el algoritmo usando neuralnet
library(neuralnet)

#Agregamos esta opción para que los resultados sean repetibles, es decir, evitar que cada vez que lo ejecutemos hayan valores
#diferentes
set.seed(12354)

#Crear el modelo usando neuralnet
model <- neuralnet(Indice_Accidentalidad~Dia+Rango_Hora+Rango_Edad+Genero+Tipo_Camino+Tipo_Vehiculo,data = accident_train, hidden = 5)

#Visualizamos la red neuronal
plot(model,rep = "best")

#Usando la función compute() vamos a generar las predicciones correspondientes.
#La función compute nos va a regresar dos componentes, las neuronas de cada capa de la red y las predicciones generadas por el modelo
model_results <- compute(model,accident_test[1:6])

#Obtenemos solo parte donde se guardan las predicciones, usando net.result
predicted_accident <- model_results$net.result
head(predicted_accident)


# Metricas de desempeño

#Correlacion
#Usamos coorrelación para conocer la coorrelación que hay entre las predicciones contra la real.
cor(predicted_accident, accident_test$Indice_Accidentalidad)[,1]

# Calcular el Error Cuadrático Medio (MSE)
mse <- mean((accident_test$Indice_Accidentalidad - predicted_accident)^2)


print(paste("Error Cuadrático Medio:", mse))

#RECALL
# Definir un umbral para clasificar en categorías de riesgo
umbral <- 0.5  # Ajusta el umbral según tus necesidades

# Convertir las predicciones en clasificación binaria (por encima del umbral o no)
predicciones_binarias <- ifelse(predicted_accident > umbral, 1, 0)
valores_reales_binarios <- ifelse(accident_test$Indice_Accidentalidad > umbral, 1, 0)

# Calcular el Recall para la categoría de riesgo alto
verdaderos_positivos <- sum(predicciones_binarias == 1 & valores_reales_binarios == 1)
todos_positivos_reales <- sum(valores_reales_binarios == 1)

recall <- verdaderos_positivos / todos_positivos_reales

print(paste("Recall para la categoría de riesgo alto:", recall))


# Convertir las predicciones en clasificación binaria (por encima del umbral o no)}
umbral_p <- 0.6
predicciones_binarias <- ifelse(predicted_accident > umbral_p, 1, 0)

# Calcular la Precisión
verdaderos_positivos <- sum(predicciones_binarias == 1 & valores_reales_binarios == 1)
falsos_positivos <- sum(predicciones_binarias == 1 & valores_reales_binarios == 0)

precision <- verdaderos_positivos / (verdaderos_positivos + falsos_positivos)

print(paste("Precisión:", precision))

# Matriz de Confusión
predicciones_binarias <- ifelse(predicted_accident > umbral, 1, 0)
valores_reales_binarios <- ifelse(accident_test$Indice_Accidentalidad > umbral, 1, 0)

# Crear la matriz de confusión
confusion_matrix <- table(Actual = valores_reales_binarios, Predicted = predicciones_binarias)

print("Matriz de Confusión:")
print(confusion_matrix)
  




