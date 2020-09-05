complemento <- read.csv("Data/cursos.csv")
names(complemento)[1] <- "curso"

View (complemento %>% 
  group_by(semestre, obligaroriedad) %>% 
  summarise(n = n()) %>%
  spread(obligaroriedad, n))

data3 <- merge(x = data2, y = complemento, by.x = "curso", by.y = "curso")

write.csv(data3, file = "Data/data3.csv")

################################################################################

asignados <- data3 %>%
  group_by(semestre, anio_asignacion) %>%
  summarise(total = n())
View(asignados %>% spread(anio_asignacion, total))

ggplot(asignados, 
       aes(anio_asignacion, total, colour = as.character(semestre), group = semestre)) + 
  geom_line(size = 1, show.legend = F) +
  geom_point(aes(size = total)) +
  labs(x = "Año", y = "No. asignaciones") +
  scale_color_discrete(name = "Semestre")

################################################################################

notas_promedio <- data3 %>%
  group_by(semestre, anio_asignacion) %>%
  summarise(promedio = round(mean(nota_total), 1)) 

ggplot(notas_promedio, 
       aes(anio_asignacion, promedio, colour = as.character(semestre), group = semestre)) + 
  geom_line(size = 1, show.legend = F) +
  geom_point(aes(size = promedio)) +
  labs(x = "Año", y = "Promedio") +
  scale_color_discrete(name = "Semestre")

View(notas_promedio %>% spread(anio_asignacion, promedio))

data3 %>% 
  group_by(semestre) %>%
  summarise(total = round(mean(nota_total), 1))

data3 %>% 
  group_by(anio_asignacion) %>%
  summarise(total = round(mean(nota_total), 1)) %>% 
  spread(anio_asignacion, total)

ggplot(data3, aes(x = factor(semestre), y = nota_total, na.rm = T)) + 
  geom_boxplot(aes(colour = factor(semestre)), show.legend = F) + 
  theme(axis.text.x = element_text(angle = 0, hjust = 2)) +
  labs(x = "Semestre", y = "Nota final") +
  scale_color_discrete(name = "Semestre")
################################################################################

# Tabla de asignados
asignados <- data3 %>%  group_by(semestre, anio_asignacion) %>% summarise(total = n()) 

# Tabla de aprobados
aprobados <- data3 %>% filter(nota_total>=61) %>%
  group_by(semestre, anio_asignacion) %>%
  summarise(total = n())

# Resultado
porcentaje_aprobación <- data.frame (
  semestre = aprobados$semestre,
  anio = aprobados$anio_asignacion,
  total = round((aprobados$total*100)/asignados$total, 1)
)

ggplot(porcentaje_aprobación, 
       aes(anio, total, colour = as.character(semestre), group = semestre)) + 
  geom_line(size = 1, show.legend = F) +
  geom_point(aes(size = total)) +
  labs(x = "Año", y = "% de aprobación") +
  scale_color_discrete(name = "Semestre")

View(porcentaje_aprobación %>% spread(anio, total))

# Por fila
asignados <- data3 %>% group_by(semestre) %>% summarise(total = n())
aprobados <- data3 %>% filter(nota_total>=61) %>% 
  group_by(semestre) %>% summarise(total = n())

# Por columnas
asignados <- data3 %>% group_by(anio_asignacion) %>% summarise(total = n())
aprobados <- data3 %>% filter(nota_total>=61) %>%
  group_by(anio_asignacion) %>%  summarise(total = n())
round((aprobados$total*100)/asignados$total, 1)


# Tabla de asignados total
asignados <- data2 %>% summarise(total = n())
aprobados <- data2 %>% filter(nota_total>=61) %>% summarise(total = n())

round((aprobados$total*100)/asignados$total, 1)

################################################################################
