# Ver base de datos
View(Encuesta_Nacional_UDP_2015)
Encuesta_Nacional_UDP_2015[sapply(Encuesta_Nacional_UDP_2015, is.numeric)] <- lapply(Encuesta_Nacional_UDP_2015[sapply(Encuesta_Nacional_UDP_2015, is.numeric)], as.factor)
# Instalar Paquetes 
library(tidyverse)
library(dplyr)
library(plyr)
library(tidyr)
library(mlogit)
# Creación variables dicotómicas
# 1. Variable Dependiente P11E
Encuesta_Nacional_UDP_2015$P11EREC = revalue(Encuesta_Nacional_UDP_2015$P11E, c("1"="1", "2"="0", "8"="0", "9"="0"))
table(Encuesta_Nacional_UDP_2015$P11EREC)
# 2. Variables Independientes
# 2.1 Pregunta 62 
Encuesta_Nacional_UDP_2015$P62REC = revalue(Encuesta_Nacional_UDP_2015$P62, c("1"="1", "2"="1", "3"="1", "4"="1", "5"="0", "6"="0", "7"="0", "8"="0", "9"="0"))
table(Encuesta_Nacional_UDP_2015$P62REC)
# 2.2 Pregunta 23
Encuesta_Nacional_UDP_2015$P23REC = revalue(Encuesta_Nacional_UDP_2015$P23, c("1"="1", "2"="1", "3"="1", "4"="0", "8"="0", "9"="0"))
table(Encuesta_Nacional_UDP_2015$P23REC)
# 2.3 Pregunta 21
Encuesta_Nacional_UDP_2015$P21REC = revalue(Encuesta_Nacional_UDP_2015$P21, c("1"="1", "2"="1", "3"="1", "4"="1", "5"="1", "6"="1", "7"="1", "8"="1", "9"="1", "10"="1", "77"="0", "88"="0", "99"="0"))
table(Encuesta_Nacional_UDP_2015$P21REC)
# 2.4 Pregunta 15
Encuesta_Nacional_UDP_2015$P15REC = revalue(Encuesta_Nacional_UDP_2015$P15, c("1"="1", "2"="1", "3"="1", "4"="0", "8"="0", "9"="0"))
table(Encuesta_Nacional_UDP_2015$P15REC)
#2.5 Pregunta 13
Encuesta_Nacional_UDP_2015$P13R = revalue(Encuesta_Nacional_UDP_2015$P13, c("1"="1", "2"="1", "3"="1", "4"="0", "8"="0", "9"="0"))
table(Encuesta_Nacional_UDP_2015$P15REC)

# 3. Variable de control: sexo entrevistado
table(Encuesta_Nacional_UDP_2015$Sexo_Entrevistado)

# 4. Instalar Paquete "Stargazer"
install.packages("stargazer")
library(stargazer)
# 5. Correr Regresion Logistica Binaria 
m1 = glm(P11EREC ~ P15REC + P23REC + P21REC, data = Encuesta_Nacional_UDP_2015, family = binomial())
View(m1)
summary(m1)
# 6. Tabular Regresión Logística Binaria
stargazer(m1, title = "Modelo 1", align =TRUE, out = "resultados1F.txt")
# 7. Instalar función de bondad de ajuste
install.packages("rsq")
library(rsq)
# 8. Calcular bondad de ajuste
rsq(m1)
# 9. Probamos un segundo modelo con una nueva variable
# Pregunta 62

# 10. Corremos segundo modelo Regresion Logistica Binaria 
m2 = glm(P11EREC ~ P15REC + P23REC + P21REC + P62REC, data = Encuesta_Nacional_UDP_2015, family = binomial())
summary(m2)
# 11. Calcular bondad de ajuste
rsq(m2)
# 12. Tabular Regresion Logistica Binaria
stargazer(m1, m2, title = "Resultados", align =TRUE, out = "resultados2.txt")
# 13. Probamos un tercer modelo con una nueva variable
# Pregunta 14I

# 14. Corremos 3 modelo Regresion Logistica Binaria 
m3 = glm(P11EREC ~ P15REC + P23REC + P21REC + P62REC + P13R, data = Encuesta_Nacional_UDP_2015, family = binomial())
summary(m3)
# 15. Calcular bondad de ajuste
rsq(m3)
# 16. Tabular Regresion Logistica Binaria
stargazer(m1, m2, m3, title = "Resultados", align =TRUE, out = "resultadosFinal.txt")

#Para Exportar (Para usarlo debo cargar la Base de Datos nuevamente)
# Expotar Tablas de contingencia dos variabes
# Instar sjPlot
sjt.xtab(Encuesta_Nacional_UDP_2015$P11E, Encuesta_Nacional_UDP_2015$P15, file = "tablaClaudia1.doc")

