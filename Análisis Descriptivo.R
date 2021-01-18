#Cargar Encuesta
# Ver base de datos
View(Encuesta_Nacional_UDP_2015)
# Activación de packages
library(tidyverse)
library(dplyr)
library(plyr)
library(tidyr)
library(mlogit)

# 1. Análisis descriptivo 
# 1.1 Frecuencias
table(Encuesta_Nacional_UDP_2015$P62)
table(Encuesta_Nacional_UDP_2015$P11E)
#1.2 Tablas de contingencia en porcentajes 
table(Encuesta_Nacional_UDP_2015$P11E,Encuesta_Nacional_UDP_2015$P62)
religión_violación <-table(Encuesta_Nacional_UDP_2015$P11E,Encuesta_Nacional_UDP_2015$P62)
prop.table(religión_violación)*100
#1.3 Cálculo de correlación
cor.test(Encuesta_Nacional_UDP_2015$P11E,Encuesta_Nacional_UDP_2015$P62)
#1.4 Repetir lo mismo con cada variable
#1.4.1 # Variable P23
table(Encuesta_Nacional_UDP_2015$P23)
table(Encuesta_Nacional_UDP_2015$P11E,Encuesta_Nacional_UDP_2015$P23)
posición_violación <-table(Encuesta_Nacional_UDP_2015$P11E,Encuesta_Nacional_UDP_2015$P23)
cor.test(Encuesta_Nacional_UDP_2015$P11E,Encuesta_Nacional_UDP_2015$P23)
prop.table(posición_violación)*100
#1.4.2 Variable P21
table(Encuesta_Nacional_UDP_2015$P21)
table(Encuesta_Nacional_UDP_2015$P11E,Encuesta_Nacional_UDP_2015$P21)
partidos_violación <-table(Encuesta_Nacional_UDP_2015$P11E,Encuesta_Nacional_UDP_2015$P21)
cor.test(Encuesta_Nacional_UDP_2015$P21,Encuesta_Nacional_UDP_2015$P11E)
prop.table(partidos_violación)*100
#1.4.3 Variable P13
table(Encuesta_Nacional_UDP_2015$P13)
table(Encuesta_Nacional_UDP_2015$P11E,Encuesta_Nacional_UDP_2015$P13)
aborto_Democracia <-table(Encuesta_Nacional_UDP_2015$P11E,Encuesta_Nacional_UDP_2015$P13)
cor.test(Encuesta_Nacional_UDP_2015$P11E,Encuesta_Nacional_UDP_2015$P13)
prop.table(aborto_Democracia)*100
#1.4.4
table(Encuesta_Nacional_UDP_2015$P15)
table(Encuesta_Nacional_UDP_2015$P11E,Encuesta_Nacional_UDP_2015$P15)
aborto_Interes <-table(Encuesta_Nacional_UDP_2015$P11E,Encuesta_Nacional_UDP_2015$P15)
cor.test(Encuesta_Nacional_UDP_2015$P11E,Encuesta_Nacional_UDP_2015$P15)
prop.table(aborto_Interes)*100
library(readr)
# Exportar tablas Frecuencias en Excel 
write.csv2(religión_violación, file = "tablamuestraP62.csv")
write.csv2(posición_violación, file = "tablamuestraP23.csv")
write.csv2(partidos_violación, file = "tablamuestraP21.csv")
write.csv2(aborto_Democracia, file = "tablamuestraP13.csv")
write.csv2(aborto_Interes, file = "tablamuestraP15.csv")
# 2. Exportar tablas Porcentajes en Excel
# 2.1 Nombrar tabla a exportar 
religiónprop<-prop.table(religión_violación)*100
# 2.2 Exportar tabla a Excel (Buscar exportación en Documentos)
write.csv2(religiónprop, file = "tablamuestraP62P.csv")
# 2.3 Repetir lo mismo con cada variable
# 2.3.1 Nombrar tablas
posiciónprop<-prop.table(posición_violación)*100
partidosprop<-prop.table(partidos_violación)*100
democraciaprop<-prop.table(aborto_Democracia)*100
interésprop<-prop.table(aborto_Interes)*100
# 2.3.1 Exportar tablas
write.csv2(posiciónprop, file = "tablamuestraP23P.csv")
write.csv2(partidoprop, file = "tablamuestraP21P.csv")
write.csv2(democraciaprop, file = "tablamuestraP13P.csv")
write.csv2(interésprop, file = "tablamuestraP15P.csv")







