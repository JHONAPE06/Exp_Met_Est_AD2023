#JONATHAN DE JESUS PEREZ DE LA ROSA
# 20/10/2022
# Tarea 4

# Ejercicio 1 -------------------------------------------------------------

nematodos <- read.csv("scripts/nematodos.csv", header = T)
View(nematodos)
nematodos
attach(nematodos)
names(nematodos)
class(Nematodos)
class(Cantidad.de.nematodos)
summary(nematodos)
boxplot(Cantidad.de.nematodos~ Nematodos)
shapiro.test(nematodos$Cantidad.de.nematodos)
bartlett.test(nematodos$Cantidad.de.nematodos ~ nematodos$Nematodos)
loc.aov <- aov(nematodos$Cantidad.de.nematodos ~ nematodos$Nematodos)
summary(loc.aov)
TukeyHSD(loc.aov)
plot(TukeyHSD(loc.aov))
tapply(nematodos$Cantidad.de.nematodos, nematodos$Nematodos, mean)
# Explora los datos de la muestra mediante gráficos y descriptivos. ¿Observamos diferencias en los valores promedios y de variabilidad por grupos?
# Si
#Aplique la función tapply y encuentre las varianzas de los cinco tratamientos. ¿Cuántas veces es la diferencia entre la varainza más pequeña y la más grande?
# S1    S2    S3    S4    S5 
#148.8 140.8 130.4 100.4 161.6
# Realiza un test F (ANOVA) para comparar las medias de las 5 muestras ¿Cuáles serían las hipótesis alterna y alternativa?
# La hipótesis nula dependiendo del tipo de suelo que se encuentre va ver diferencia signifactiva.
# La hipotesis que dependiendo de tipo de suelo no a ver diferencias significativas
# Describe los resultados obtenidos indicando cuál es el valor del estadístico de contraste (F), los grados de libertad del factor, los grados de libertad residuales y el valor de P.
# Bartlett's K-squared = 3.0807, df = 4, p-value = 0.5444
# También indica cuál sería el valor crítico de F bajo la hipótesis nula, que nos proporcionará la definición de una región de aceptación y rechazo (consideramos un nivel de significación alfa = 0.05).
# W = 0.96032, p-value = 0.42


# Ejercicio 2 -------------------------------------------------------------

crecimiento <- read.csv("scripts/crecimiento.csv", header = T)
View(crecimiento)
crecimiento
attach(crecimiento)
names(crecimiento)
class(Numero.de.observacionbes)
summary(crecimiento)
boxplot(Numero.de.observacionbes ~ Observaciones)
bartlett.test(crecimiento$Numero.de.observacionbes ~ crecimiento$Observaciones)
loc.aov <- (crecimiento$Numero.de.observacionbes ~ crecimiento$Observaciones )
summary (loc.aov)
TukeyHSD(loc.aov)
plot(TukeyHSD(loc.aov))
tapply(crecimiento$Numero.de.observacionbes, crecimiento$Observaciones, mean)

#  Explora los datos de la muestra mediante gráficos y descriptivos. ¿Observamos diferencias en los valores promedios y de variabilidad por grupos?
# Si
# Aplique la función tapply y encuentre las varianzas de los cinco tratamientos. ¿Cuántas veces es la diferencia entre la varainza más pequeña y la más grande?
# Alto      Bajo     Medio 
# 35.000000  7.333333 16.000000
#Realiza un test F (ANOVA) para comparar las medias de las 5 muestras ¿Cuáles serían las hipótesis nula y alternativa?
# Hipotesis alterna si existe una relacion significativa entre nivel de riego y el crecmiento de las plantas
# Describe los resultados obtenidos indicando cuál es el valor del estadístico de contraste (F), los gradosde libertad del factor, los grados de libertad residuales y el valor de P.
# Bartlett's K-squared = 6.6164, df = 2, p-value = 0.03658
#También indica cuál sería el valor crítico de F bajo la hipótesis nula, que nos proporcionará la definición de una región de aceptación y rechazo (consideramos un nivel de significación alfa = 0.05)

#Tras evaluar la tabla ANOVA, ¿cuál sería tu conclusión en el contexto del problema?


