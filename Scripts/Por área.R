# Cursos por área
data2 %>% select(nombre_area, nombre_curso) %>% unique() %>%
  group_by(nombre_area) %>% summarise(total = n())


round((c(11,5,11,4)*100)/31, 0)




# Asignados por area, año-periodo
asignados <- data2 %>%
  select(nombre_area, anio_asignacion, periodo, nota_total) %>%
  mutate(tipo_periodo = ifelse(periodo==1 | periodo==5, "S", "V")) %>%
  group_by(nombre_area, anio_asignacion, tipo_periodo) %>%
  summarise(total = n())
asignados <- asignados %>% spread(tipo_periodo, total)
View(asignados)


asignados <- data2 %>%
  group_by(nombre_area, anio_asignacion) %>%
  summarise(total = n())
asignados
ggplot(asignados, 
       aes(anio_asignacion, total, colour = nombre_area, group = nombre_area)) + 
  geom_line(size = 1, show.legend = F) +
  geom_point(aes(size = total)) +
  labs(x = "Año", y = "No. asignaciones") +
  scale_color_discrete(name = "Área")

################################################################################

# Promedio por area
notas_promedio <- data2 %>% 
  select(nombre_area, anio_asignacion, periodo, nota_total) %>%
  mutate(tipo_periodo = ifelse(periodo==1 | periodo==5, "S", "V")) %>%
  group_by(nombre_area, anio_asignacion, tipo_periodo) %>%
  summarise(promedio = round(mean(nota_total), 1))
notas_promedio <- notas_promedio %>% spread(tipo_periodo, promedio)
View(notas_promedio)


# Promedio por filas
View(data2 %>% 
       select(nombre_area, anio_asignacion, periodo, nota_total) %>%
       group_by(nombre_area) %>%
       summarise(promedio = round(mean(nota_total), 1)))

# Figura 2.	Nota promedio por curso
ggplot(data2, aes(x = factor(nombre_area), y = nota_total, na.rm = T)) + 
  geom_boxplot(aes(colour = nombre_area), show.legend = F) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "Nota final") +
  scale_color_discrete(name = "Área")

################################################################################

# Tabla de asignados
asignados <- data2 %>%
  mutate(tipo_periodo = ifelse(periodo==1 | periodo==5, "S", "V")) %>%
  group_by(nombre_area, anio_asignacion, tipo_periodo) %>%
  summarise(total = n())
asignados <- asignados %>% spread(tipo_periodo, total)
asignados

# Tabla de aprobados
aprobados <- data2 %>% filter(nota_total>=61) %>%
  mutate(tipo_periodo = ifelse(periodo==1 | periodo==5, "S", "V")) %>%
  group_by(nombre_area, anio_asignacion, tipo_periodo) %>%
  summarise(total = n())
aprobados <- aprobados %>% spread(tipo_periodo, total)
aprobados

# Resultado
porcentaje_aprobación <- data.frame(
  nombre_area = aprobados$nombre_area,
  anio_ingreso = aprobados$anio_asignacion,
  S = round((aprobados$S*100)/asignados$S, 1),
  V = round((aprobados$V*100)/asignados$V, 1)
)
View(porcentaje_aprobación)


# Tabla de asignados fila
asignados <- data2 %>% group_by(nombre_area) %>% summarise(total = n())

# Tabla de aprobados fila
aprobados <- data2 %>% filter(nota_total>=61) %>% 
  group_by(nombre_area) %>%
  summarise(total = n())

# Resultado fila
porcentaje_aprobación <- data.frame(
  nombre_area = aprobados$nombre_area,
  porcentaje = round((aprobados$total*100)/asignados$total, 1)
)
View(porcentaje_aprobación)





# Grafico

asignados <- data2 %>% group_by(nombre_area, anio_asignacion) %>% summarise(total = n())
aprobados <- data2 %>% filter(nota_total>=61) %>% 
  group_by(nombre_area, anio_asignacion) %>% summarise(total = n())
porcentaje_aprobación <- data.frame(
  nombre_area = aprobados$nombre_area,
  anio_asignacion = aprobados$anio_asignacion,
  porcentaje = round((aprobados$total*100)/asignados$total, 1)
)
porcentaje_aprobación


ggplot(porcentaje_aprobación, 
       aes(anio_asignacion, porcentaje, colour = nombre_area, group = nombre_area)) + 
  geom_line(size = 1, show.legend = F) +
  geom_point(aes(size = porcentaje)) +
  labs(x = "Año", y = "Porcentaje") +
  scale_color_discrete(name = "Área")

################################################################################

leveneTest(nota_total ~ nombre_area,data2, center = "median")
