# Jonathan de Jesus de la Rosa
# 2176695
# 21/08/20223

# Importar datos de un archivo de excel a la consola de R
# funcion "read.csv"

# Importar ----------------------------------------------------------------
setwd("C:/REPOSITORIO/Exp_Met_Est_AD2023/Scripts")
ocampo <- read.csv("Ema_Ocampo.csv" , header = T)
head(ocampo)
tail(ocampo)

# Descriptivas ------------------------------------------------------------

mean(ocampo$X......DIRS)
mean(ocampo$TEMP)
median(ocampo$TEMP)
sd(ocampo$TEMP)
var(ocampo$TEMP)
range(ocampo$TEMP)
fivenum(ocampo$TEMP)

# Datos vivero ------------------------------------------------------------

boxplot(ocampo$TEMP,
        col = "lightgreen",
        ylim=c(10,  30),
        ylamb = "TEMP °C",
        main = "Ema Ocampo",
        horizontal = T)
mtext("Ema", side = 4, adj =1, padj = 1)
mtext("Datos de agosto 2023", side =1, padj =3, adj =1)

# Datos vivero ------------------------------------------------------------

IE <- read.csv("Vivero_IE.csv", header = T)
IE$Tratamiento <-as.factor(IE$Tratamiento)

mean(IE$IE)

tapply(IE$IE, IE$Tratamiento, mean)
tapply(IE$IE, IE$Tratamiento, length)


# Normalidad de datos  ----------------------------------------------------


# Shapiro wilks -----------------------------------------------------------

shapiro.test(IE$IE)

# Homogeneidad de varianzas -----------------------------------------------

bartlett.test(IE$IE ~ IE$Tratamiento)

# Aplicar la prueba de t para muestras independiente ----------------------

t.test(IE$IE ~ IE$Tratamiento, var.equal = T)

# Prueba sw t de una muestra
#Subconjunto con los datos de ctrl y Fert

Ctrl <- subset(IE$IE, IE$Tratamiento == "Ctrl")
Fert <- subset(IE$IE, IE$Tratamiento == "Fert")

t.test(Ctrl, mu = 0.95)
t.test(Ctrl, mu = 0.80)                           
t.test(Ctrl, mu = 0.90)        

aire <- airquality


# prueba de t dependientes
# Datos de airquality de la ciudad de NY, USA
# Comparar las variables en datos periodos verano (junio)
# Otoño (septimebre)

boxplot(aire$Ozone ~ aire$Month)
boxplot(aire$Temp ~ aire$Month)
aire$centi <- (aire$Temp - 32) / 1.8
aire$centi <- round((aire$Temp - 32) / 1.8,1)
boxplot(aire$centi ~ aire$Month)


# Crear un subconjunto solo con los meses de Junio y Sept
aire.jun <- subset(aire, Month == "6")
aire.sep <- subset(aire, Month == "9")

t.test(aire.jun$Wind , aire.sep$Wind, paired =T)
boxplot(aire$Wind ~ aire$Month)

#MAGT
# Correlacion
#
library(repmis)
erupciones <- source_data("https://www.dropbox.com/s/liir6sil7hkqlxs/erupciones.csv?dl=0")
erupciones <- source_data("https://www.dropbox.com/s/liir6sil7hkqlxs/erupciones.csv?dl=1")

plot(erupciones$waiting, erupciones$eruptions, xlab = "Tiempo de espera entre erupciones (min)",
     ylab = "Duracion de las erupciones (min)", pch = 19)
# Promedio de las erupciones
mean(erupciones$eruptions)
mean(erupciones$waiting)

range(erupciones$eruptions)
range(erupciones$waitin)

boxplot(erupciones$eruptions)
boxplot(erupciones$waiting)


median(erupciones$eruptions)
sd(erupciones$eruptions)
var(erupciones$eruptions)

# Establecer HO y H1
# funcion de correlacion cor.test

cor.test(erupciones$eruptions , erupciones$waiting)

# Canopy

# Canopy ------------------------------------------------------------------


copa <- read.csv("canopy_short.csv", header = T)
copa$Forest <- as.factor(copa$Forest)

# Identificacionde tendencias

plot(copa$Cnpy, copa$LAI4)
plot(copa$Cnpy, copa$GLI)

cor.test(copa$Cnpy , copa$LAI4)
cor.test(copa$Cnpy, copa$GLI)
