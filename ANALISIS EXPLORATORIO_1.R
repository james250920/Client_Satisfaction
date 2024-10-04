#######################################
#                                     #
#        ANÁLISIS EXPLORATORIO        #
#    MAG.OMAR CHINCARO DEL CORAL      #   
#       oachincaro@gmail.com          #
#                                     #
#######################################

#---------------------------------------------------------
#Limpiar la memoria de RStudio
rm(list = ls())

#---------------------------------------------------------

# Cargando los paquetes
install.packages(c("caret",
                   "DataExplorer",
                   "VIM",
                   "missForest",
                   "ggplot2",
                   "lattice",
                   "colorspace",
                   "data.table",
                   "randomForest",
                   "foreach",
                   "itertools",
                   "MASS",
                   "pROC",
                   "foreign",
                   "gmodels",
                   "caTools",
                   "dplyr",
                   "iterators",
                   "tidyverse",
                   "kableExtra",
                   "scales",
                   "Boruta",
                   "factoextra",
                   "mvoutlier",
                   "outliers",
                   "cluster",
                   "fpc",
                   "mclust",
                   "dbscan",
                   "readxl",
                   "psych",
                   "corrplot",
                   "mclust",
                   "gclus", 
                   "rrcov",
                   "tourr",
                   "aplpack",
                   "TeachingDemos",
                   "rgl",
                   "ape",
                   "GGally",
                   "Hmisc",
                   "e1071",
                   "class",
                   "sqldf"),dependencies = T)
library(ggplot2)
library(caret)
library(DataExplorer)
library(VIM)
library(missForest)
library(ggplot2)
library(lattice)
library(colorspace)
library(data.table)
library(randomForest)
library(foreach)
library(itertools)
library(MASS) 
library(pROC)
library(foreign)
library(gmodels)
library(caTools)
library(dplyr)
library(tidyverse)
library(kableExtra)
library(scales)
library(Boruta)
library(factoextra)
library(mvoutlier)
library(outliers)
library(cluster)
library(fpc)
library(mclust)
library(dbscan)
library(readxl)
library(psych)
library(corrplot)
library(mclust)
library(gclus) 
library(rrcov)
library(tourr)
library(aplpack)
library(rgl)
library(ape)
library(GGally)
library(Hmisc)
library(e1071)  
library(class)  
library(sqldf)


setwd("E:/ESAN/ANALISIS MULTIVARIADO I/BASES/") #Fija el directorio de trabajo
getwd() # Ver el directorio de trabajo

#Lectura de datos
library(foreign)

datos <-read.spss("BASE PROPENSION AL USO DE TC.sav",
                  use.value.labels=TRUE, 
                  to.data.frame=TRUE) %>% data.frame()
# Viendo la estructura de los datos
dim(datos) #Muestra la dimensi?n de los datos
glimpse(datos)


#####################################################################################################
################################### ANÁLISIS EXPLORATORIO DE DATOS ##################################
#####################################################################################################



#####################################
#### ANALISIS DE DATOS PERDIDODS ####
#####################################


# Ver la cantidad de datos perdidos

library(DataExplorer)
plot_missing(datos)

datos_perdidos <- colSums(is.na(datos))  |> as.data.frame()

N_Datos <- sqldf::sqldf("SELECT CodCli,Edad,Gener,EstCiv,Sexo,NZONA,Region,Ingreso,Perfil,
                                Lin_Ent001,Lin_Ent002,Lin_Ent049,Lin_Ent050,Lin_Ent058,Lin_Ent110,Lin_Ent113,MaxLin,
                                TC_Ent001,TC_Ent002,TC_Ent058,TC_Ent110,MaxSal,UsoTC
                         FROM datos")

#############################################
#### DISTRIBUCION DE VARIABLES CONTINUAS ####
#############################################



#En el histograma hay que mirar la distribucion y el tipo de variable
par(mfrow = c(2,2))
hist(N_Datos$Edad, main = "Edad del cliente",col = "orange")
hist(N_Datos$Ingreso , main = "Ingreso del cliente",col = "orange")
hist(N_Datos$MaxLin , main = "Ingreso del cliente",col = "orange")
hist(N_Datos$MaxSal , main = "Ingreso del cliente",col = "orange")
par(mfrow = c(1,1))

par(mfrow = c(2,4))
hist(N_Datos$Lin_Ent001, main = "Linea TC - B. Credito",col = "orange")
hist(N_Datos$Lin_Ent002, main = "Linea TC - B. Interbank",col = "orange")
hist(N_Datos$Lin_Ent049, main = "Linea TC - F. Crediscotia",col = "orange")
hist(N_Datos$Lin_Ent050, main = "Linea TC - B. Ripley",col = "orange")
hist(N_Datos$Lin_Ent058, main = "Linea TC - B. Financiero",col = "orange")
hist(N_Datos$Lin_Ent110, main = "Linea TC - B. Falabella",col = "orange")
hist(N_Datos$Lin_Ent113, main = "Linea TC - F. OH",col = "orange")
hist(N_Datos$MaxLin, main = "Máxima Linea de TC",col = "orange")
par(mfrow = c(1,1))

par(mfrow = c(2,3))
hist(N_Datos$TC_Ent001, main = "Saldo TC - B. Credito",col = "orange")
hist(N_Datos$TC_Ent002, main = "Saldo TC - B. Interbank",col = "orange")
hist(N_Datos$TC_Ent058, main = "Saldo TC - B. Financiero",col = "orange")
hist(N_Datos$TC_Ent110, main = "Saldo TC - B. Falabella",col = "orange")
hist(N_Datos$MaxSal, main = "Máxima Saldo de TC",col = "orange")
par(mfrow = c(1,1))




###############################################
#### DISTRIBUCION DE VARIABLES CATEGORICAS ####
###############################################


par(mfrow = c(2,3))
plot(N_Datos$Gener, main = "Grupo generacionales del cliente",col = "seashell")
plot(N_Datos$Sexo, main = "Genero del cliente",col = "seashell")
plot(N_Datos$EstCiv, main = "Estado civildel cliente",col = "seashell")
plot(N_Datos$NZONA, main = "Zona donde vive el cliente",col = "seashell")
plot(N_Datos$Region, main = "Región donde vive el cliente",col = "seashell")
plot(N_Datos$Perfil, main = "Perfil donde vive el cliente",col = "seashell")
par(mfrow = c(1,1))

plot(N_Datos$UsoTC, main = "Uso de la TC",col = "seashell")





######################################
#### DIICOTOMIZACION DE VARIABLES ####
######################################


#VARIABLLES CONTINUAS
D_Lin_Ent001 <- as.data.frame(ifelse(is.na(N_Datos$Lin_Ent001) == TRUE,0,1))
D_Lin_Ent002 <- as.data.frame(ifelse(is.na(N_Datos$Lin_Ent002) == TRUE,0,1))
D_Lin_Ent049 <- as.data.frame(ifelse(is.na(N_Datos$Lin_Ent049) == TRUE,0,1))
D_Lin_Ent050 <- as.data.frame(ifelse(is.na(N_Datos$Lin_Ent050) == TRUE,0,1))
D_Lin_Ent058 <- as.data.frame(ifelse(is.na(N_Datos$Lin_Ent058) == TRUE,0,1))
D_Lin_Ent110 <- as.data.frame(ifelse(is.na(N_Datos$Lin_Ent110) == TRUE,0,1))
D_Lin_Ent113 <- as.data.frame(ifelse(is.na(N_Datos$Lin_Ent113) == TRUE,0,1))
D_TC_Ent001 <- as.data.frame(ifelse(is.na(N_Datos$TC_Ent001) == TRUE,0,1))
D_TC_Ent002 <- as.data.frame(ifelse(is.na(N_Datos$TC_Ent002) == TRUE,0,1))
D_TC_Ent058 <- as.data.frame(ifelse(is.na(N_Datos$TC_Ent058) == TRUE,0,1))
D_TC_Ent110 <- as.data.frame(ifelse(is.na(N_Datos$TC_Ent110) == TRUE,0,1))

Datos_Dico_cont <- cbind(D_Lin_Ent001,D_Lin_Ent002,D_Lin_Ent049,D_Lin_Ent050,
                         D_Lin_Ent058,D_Lin_Ent110,D_Lin_Ent113,
                         D_TC_Ent001,D_TC_Ent002,D_TC_Ent058,D_TC_Ent110)

#Renombrar las variables
colnames(Datos_Dico_cont) <- c("D_Lin_Ent001","D_Lin_Ent002","D_Lin_Ent049","D_Lin_Ent050",
                               "D_Lin_Ent058","D_Lin_Ent110","D_Lin_Ent113",
                               "D_TC_Ent001","D_TC_Ent002","D_TC_Ent058","D_TC_Ent110")


#VARIABLLES CATEGORICAS
install.packages("fastDummies")
library(fastDummies)

Datos_Categoricos <- sqldf::sqldf("SELECT Gener,EstCiv,Sexo,NZONA,Perfil FROM N_Datos")

#Dicotomizacion de variables categoricas
Datos_Dico_cat <- dummy_cols(Datos_Categoricos,select_columns = c("Gener","EstCiv","Sexo","NZONA","Perfil")) |> 
                  select(-c("Gener","EstCiv","Sexo","NZONA","Perfil"))

colnames(Datos_Dico_cat) <- c("D_Gener_Z","D_Gener_Y","D_Gener_X","D_Gener_BB","D_Gener_S",
                              "D_EstCiv_Sol","D_EstCiv_Conv","D_EstCiv_Cas","D_EstCiv_Div","D_EstCiv_Viu","D_Fallecido",
                              "D_Sexo_M","D_Sexo_F",
                              "D_Zona_Norte","D_Zona_Centro","D_Zona_Sur","D_Zona_Selva",
                              "D_Lima_Norte","D_Lima_Este","D_Lima_Cercado","D_Lima_Moderna","D_Lima_Sur",
                              "D_Callao","D_Balnearios","D_Lima_Provincia","D_No_Reg",
                              "D_Conservador","D_Imdiferente","D_Funcional","D_Tecnologico","D_Reacio","D_Aspiracional")




###############################################
#### TRANSFORMACION DE VARIABLES CONTINUAS ####
###############################################


# Normalizaci?n de variables entre [0, 1]
#Z = (X - min(x)) / (max(x) - min(x))

#Estandarizaci?n de variables
#Z = X - mean(x) / sd(x)


summary(N_Datos$Ingreso)
summary(N_Datos$MaxLin)
summary(N_Datos$MaxSal)

T_Ingreso <- log(N_Datos$Ingreso) %>% as.data.frame()
T_MaxLin <- log(N_Datos$MaxLin) %>% as.data.frame() %>% as.data.frame()
T_MaxSal <- log(N_Datos$MaxSal) %>% as.data.frame()
Z_Edad <- scale(N_Datos$Edad, center = TRUE, scale = TRUE) %>% as.data.frame()
colnames(T_Ingreso) <- c("T_Ingreso")
colnames(T_MaxLin) <- c("T_MaxLin")
colnames(T_MaxSal) <- c("T_MaxSal")
colnames(Z_Edad) <- c("Z_Edad")



par(mfrow = c(1,2))
hist(N_Datos$Ingreso,main = "Distribuci?n del Ingreso",xlab = "Ingreso",ylab = "Frecuencia",col = 1)
hist(T_Ingreso,main = "Distribuci?n del Ingreso",xlab = "Ingreso",ylab = "Frecuencia",col = 2)
par(mfrow = c(1,1))

par(mfrow = c(1,2))
hist(N_Datos$MaxLin,main = "Distribuci?n de la Maxima Linea TC",xlab = "Ingreso",ylab = "Frecuencia",col = 1)
hist(T_MaxLin,main = "Distribuci?n de la Maxima Linea TC",xlab = "Ingreso",ylab = "Frecuencia",col = 2)
par(mfrow = c(1,1))

par(mfrow = c(1,2))
hist(N_Datos$MaxSal,main = "Distribuci?n de la Maxima Saldo TC",xlab = "Ingreso",ylab = "Frecuencia",col = 1)
hist(T_MaxSal,main = "Distribuci?n de la Maxima Saldo TC",xlab = "Ingreso",ylab = "Frecuencia",col = 2)
par(mfrow = c(1,1))


#Creacion de variables dicotomicas como factores para analisis
MasterTable <- cbind(select(N_Datos,CodCli,UsoTC),Z_Edad,Datos_Dico_cat,Datos_Dico_cont,T_Ingreso,T_MaxLin,T_MaxSal)

