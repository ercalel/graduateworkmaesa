
# Cursos que tienen asignaciones durante el periodo analizado
unique(data2[, c(4,1,9)])


# Estudiantes asignados por curso en cada año y periodo 
unique(data2[, c(4,1)])

################################################################################
# Número de asignaciones

asignaciones <- data2 %>% 
  mutate(tipo_periodo = ifelse(periodo==1 | periodo==5, "S", "V")) %>%
  group_by(nombre_curso, anio_asignacion, tipo_periodo) %>%
  summarise(n=n())

asignaciones
asignaciones %>% spread(tipo_periodo, n)

################################################################################

promedios <- data2 %>% 
  mutate(tipo_periodo = ifelse(periodo==1 | periodo==5, "S", "V")) %>%
  group_by(nombre_curso, anio_asignacion, tipo_periodo) %>%
  summarise(promedio = mean(nota_total))

promedios
promedios %>% spread(tipo_periodo, promedio)

# Promedio ponderado por filas
View(data2 %>% 
       select(nombre_curso, nota_total) %>%
       group_by(nombre_curso) %>%
       summarise(promedio =round(  mean(nota_total),1)))

# Promedio ponderado por columnas
View(data2 %>% 
       select(nombre_curso, anio_asignacion, periodo, nota_total) %>%
       mutate(tipo_periodo = ifelse(periodo==1 | periodo==5, "S", "V")) %>%
       group_by(anio_asignacion, tipo_periodo) %>%
       summarise(promedio = round(mean(nota_total),1)))

# Promedio general
mean(data2$nota_total)


# Figura 2.	Nota promedio por curso
ggplot(data2, aes(x = factor(nombre_curso), y = nota_total, na.rm = T)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Nota total") +
  xlab("")
################################################################################

# Tabla de asignados
asignados <- data2 %>%
  select(nombre_curso, anio_asignacion, periodo, nota_total) %>%
  mutate(tipo_periodo = ifelse(periodo==1 | periodo==5, "S", "V")) %>%
  group_by(nombre_curso, anio_asignacion, tipo_periodo) %>%
  summarise(total = n())
asignados <- asignados %>% spread(tipo_periodo, total)
asignados

# Tabla de aprobados
aprobados <- data2 %>% filter(nota_total>=61) %>%
  select(nombre_curso, anio_asignacion, periodo, nota_total) %>%
  mutate(tipo_periodo = ifelse(periodo==1 | periodo==5, "S", "V")) %>%
  group_by(nombre_curso, anio_asignacion, tipo_periodo) %>%
  summarise(total = n())
aprobados <- aprobados %>% spread(tipo_periodo, total)
aprobados

# Resultado
porcentaje_aprobacion <- data.frame(
  nombre_curso = aprobados$nombre_curso,
  anio_ingreso = aprobados$anio_asignacion,
  S = round((aprobados$S*100)/asignados$S, 1),
  V = round((aprobados$V*100)/asignados$V, 1)
)
porcentaje_aprobacion

write.csv(porcentaje_aprobación, "Output/porcentaje_aprobacion.csv", na = "0")   


# Tabla de asignados por curso
asignados <- data2 %>%
  select(nombre_curso, nota_total) %>%
  group_by(nombre_curso) %>%
  summarise(total = n())
asigandos

# Tabla de aprobados por curso
aprobados <- data2 %>% filter(nota_total>=61) %>%
  select(nombre_curso, nota_total) %>%
  group_by(nombre_curso) %>%
  summarise(total = n())
aprobados

# Resultado por curso
porcentaje_aprobación <- data.frame(
  nombre_curso = aprobados$nombre_curso,
  porcentaje = round((aprobados$total*100)/asignados$total, 1)
)
View(porcentaje_aprobación)


# Tabla de asignados por año
asignados <- data2 %>%
  select(anio_asignacion, periodo, nota_total) %>%
  mutate(tipo_periodo = ifelse(periodo==1 | periodo==5, "S", "V")) %>%
  group_by(anio_asignacion, tipo_periodo) %>%
  summarise(total = n())
asignados

# Tabla de aprobados  por año
aprobados <- data2 %>% filter(nota_total>=61) %>%
  select(anio_asignacion, periodo, nota_total) %>%
  mutate(tipo_periodo = ifelse(periodo==1 | periodo==5, "S", "V")) %>%
  group_by(anio_asignacion, tipo_periodo) %>%
  summarise(total = n())
aprobados

# Resultado  por año
porcentaje_aprobación <- data.frame(
  anio_ingreso = aprobados$anio_asignacion,
  periodo = round((aprobados$total*100)/asignados$total, 1)
)
View(porcentaje_aprobación)


# Tabla de asignados total
asignados <- data2 %>% summarise(total = n())
asignados

# Tabla de aprobados total
aprobados <- data2 %>% filter(nota_total>=61) %>% summarise(total = n())
aprobados

round((aprobados$total*100)/asignados$total, 1)

asignados <- data2 %>%
  group_by(curso, anio_asignacion) %>%
  summarise(total = n())
aprobados <- data2 %>% filter(nota_total>=61) %>%
  group_by(curso, anio_asignacion) %>%
  summarise(total = n())
porcentaje_aprobación <- data.frame(
  curso = as.character(aprobados$curso),
  anio = aprobados$anio_asignacion,
  porcentaje = round((aprobados$total*100)/asignados$total, 1)
)
porcentaje_aprobación

ggplot(porcentaje_aprobación, 
       aes(anio, porcentaje, colour = curso, group = curso)) + 
  geom_line(size = 1, show.legend = F) +
  geom_point() +
  labs(x = "Año", y = "% de aprobación") +
  scale_color_discrete(name = "Código curso")

################################################################################


require(car)
leveneTest(nota_total ~ nombre_curso,data2, center = "median")

#
#
#
#
#
#
#
#
#
#
asignados <- data2 %>%
  mutate(tipo_periodo = ifelse(periodo==1 | periodo==5, "S", "V")) %>%
  group_by(nombre_curso, anio_asignacion, tipo_periodo) %>%
  summarise(total = n())

aprobados <- data2 %>% filter(nota_total>=61) %>%
  mutate(tipo_periodo = ifelse(periodo==1 | periodo==5, "S", "V")) %>%
  group_by(nombre_curso, anio_asignacion, tipo_periodo) %>%
  summarise(total = n())

p_aprobacon <- data.frame(
  curso = aprobados$nombre_curso,
  anio = aprobados$anio_asignacion,
  periodo = aprobados$tipo_periodo,
  porcentaje = round((aprobados$total*100)/asignados$total, 1)
)
p_aprobacon



################################



p_aprobacon %>% spread(periodo, porcentaje)


promedios_ponderados_y <- function(cursos){
  result <- c()
  for (curso in cursos) {
    media_ponderada <- round(
      weighted.mean(
        p_aprobacon[p_aprobacon$curso==curso,]$porcentaje,
        asignados[asignados$nombre_curso==curso,]$total
      ), 1
    )
    
    result <- c(result, media_ponderada)
  }
  result
}

promedios_ponderados_y(sort(unique(data2$nombre_curso)))

iris$Sepal.Length


anova <- aov(formula = p ~ anio + Error(curso/anio), data = p_aprobacon)
summary(anova)
