# https://tidyr.tidyverse.org/reference/gather.html
# https://mauricioanderson.com/curso-r-tidyr/
# http://statseducation.com/Introduction-to-R/modules/tidy%20data/spread/

# Librerías utilizadas #########################################################
library(dplyr)
library(tidyverse)
library(psych) 
library(modeest)
library(ggplot2)
# ##############################################################################


# Lectura de los datos
data <- read.csv("Data/datos.csv")

# Vista preliminar de nombres de variables
names(data)

# Personalización de nombres de variables
colnames(data) <- c("nombre_curso", "genero", "periodo", "curso", 
                    "anio_asignacion", "anio_ingreso", "zona", "nota_total",
                    "nombre_area", "anio_nacimiento")

# Limpieza de datos para eliminar datos atípicos
data <- data[data$anio_ingreso <= 2018 
             & data$anio_ingreso >= 1979
             & data$anio_asignacion > data$anio_ingreso,]

# Número de registros
nrow(data)

# Ajuste año de ingreso 2012 a todos los menores o iguales a 2012
data2 <- data
data2[data2$anio_ingreso < 2012, ]$anio_ingreso <- 2012


write.csv(data2, file = "Output/data2.csv")

# ##############################################################################

# Tabla VIII.	Distribución de asignaciones por género
data2 %>% group_by(genero) %>% tally()

# Tabla IX.	Distribución de asignaciones por período
data %>% group_by(periodo) %>% tally()

# Tabla X.	Estadísticas descriptivas de las notas
# Media
mean(data2$nota_total)
# Moda
mfv(data2$nota_total)
# Mediana 	
median(data2$nota_total)
# S
sd(data2$nota_total)
# S^2 	
var(data2$nota_total)
# Coeficiente de variacion (C.V.)
sd(data2$nota_total)/mean(data$nota_total)
# Curtosis
kurtosi(data2$nota_total)
# Asimetría
skew(data2$nota_total)
# ##############################################################################


# 3.1.1.	Pruebas de normalidad  ###############################################

# Histograma + curva normal teórica
ggplot(data = data2, aes(x = nota_total)) +
  geom_histogram(aes(y = ..density.., fill = ..count..)) +
  scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
  stat_function(fun = dnorm, 
                colour = "firebrick", 
                args = list(mean = mean(data2$nota_total),
                            sd = sd(data2$nota_total))) +
  xlab("Nota total") +
  ylab("Densidad") +
  labs(fill = "Cantidad") +
  theme_bw()


# Tabla XI.	Prueba de normalidad de las notas
ks.test(data2$nota_total, "pnorm", mean(data2$nota_total), sd(data2$nota_total))
################################################################################
#
#
#
#
#
#
boxplot(nota_total~nombre_curso, data = data, xaxt='n', col = "orange", 
        main = "Boxplot de los cursos profesionales", xlab = "Código de curso",
        ylab = "Nota total")
axis(1, at = c(1:31), labels = unique(data$curso), las = 2)
