#Modelo de regresión logística para vacunas


library(caret) #Para seleccion de muestras
library(pROC) #Para analisis ROC

#Introduccion de los datos
datos<-read.table("CH14PR14.txt")
#Variables de salida
names(datos)[names(datos) == "V1"] <- "vacuna"
#Entrada
names(datos)[names(datos) == "V2"] <- "edad"
names(datos)[names(datos) == "V3"] <- "cuid"
names(datos)[names(datos) == "V4"] <- "genero"

#Modelo de regresion logistica
modLog<-glm(vacuna~., data=datos, family = "binomial")
summary(modLog)

#El modelo es log(p/(1-p))=-1.7716+0.728X_1-0.099X_2+0.434X_3

#Estimacion de ejemplo
#gen=1, edad=55, cuid=60
k <- sum(modLog$coefficients*c(1,55,60,1))
p <- exp(k)/(1+exp(k))

#Buscamos el umbral optimo con ROC
rocY <- roc(datos$vacuna, modLog$fitted.values)
#Calculamos el estadÃ?stico J de Youden
youd<-rocY$sensitivities+rocY$specificities-1
#Asignamos con su respectivo umbral
umbyoud<-cbind(rocY$thresholds, youd)
#Ordenamos de mayor a menor los estadisticos
youdsort<-umbyoud[order(youd, decreasing = TRUE)]
#Umbral optimo
umb<-youdsort[1]
umb

#k-CrossValidation para k=10 y muestras de tamaño 120 
#Aprox. el 75% de los datos
i <- 1
res_fpr <- c()
res_fnr <- c()
while (i<11){
  set.seed(i)
  indices_entren <- createDataPartition(datos$vacuna, p=0.75)$Resample1
  #conjunto de entrenamiento
  datos_entrenamiento <- datos[indices_entren, ] 
  #conjunto de prueba
  datos_prueba <- datos[-indices_entren, ] 
  modPrueba <- glm(vacuna~edad+cuid+genero, data=datos_entrenamiento, family="binomial")
  datos_prueba$pred <- predict(modPrueba, datos_prueba, type= "response")
  datos_prueba$pred_final <- ifelse(datos_prueba$pred > umb, 1, 0)
  tp <- 0
  fp <- 0
  fn <- 0
  tn <- 0
  j <- 1
  while (j<=(159-length(indices_entren))){
    if (datos_prueba$vacuna[j]==1){
      if (datos_prueba$pred_final[j]==1){
        tp <- tp+1
      } else{
        fn <- fn+1
      }
    } else {
      if (datos_prueba$pred_final[j]==1){
        fp <- fp+1
      } else{
        tn <- tn+1
      }
    }
    j <- j+1
  }
  fpr <- fp/(fp+tn)
  fnr <- fn/(tp+fn)
  res_fpr <- c(res_fpr,fpr)
  res_fnr <- c(res_fnr,fnr)
  i <- i+1
}
mean(res_fnr)
mean(res_fpr)
sd(res_fnr)
sd(res_fpr)
