# Asignados por genero
asignados <- data2 %>%
  group_by(genero, anio_asignacion) %>%
  summarise(total = n()) 
asignados <- asignados %>% mutate(genero = ifelse(genero==1,"Hombre", "Mujer"))
asignados %>% spread(anio_asignacion, total)

ggplot(asignados, 
       aes(anio_asignacion, total, colour = genero, group = genero)) + 
  geom_line(size = 1, show.legend = F) +
  geom_point(aes(size = total)) +
  labs(x = "Año", y = "No. asignaciones") +
  scale_color_discrete(name = "Género")

################################################################################

# Promedio por genero
notas_promedio <- data2 %>%
  group_by(genero, anio_asignacion) %>%
  summarise(promedio = round(mean(nota_total), 1)) 
notas_promedio <- notas_promedio %>% mutate(genero = ifelse(genero==1,"Hombre", "Mujer"))

ggplot(notas_promedio, 
       aes(anio_asignacion, promedio, colour = genero, group = genero)) + 
  geom_line(size = 1, show.legend = F) +
  geom_point(aes(size = promedio)) +
  labs(x = "Año", y = "Promedio") +
  scale_color_discrete(name = "Género")

notas_promedio <- notas_promedio %>% spread(anio_asignacion, promedio)

View(cbind(notas_promedio, 
      data2 %>% 
        group_by(genero) %>%
        summarise(total = round(mean(nota_total), 1))))

data2 %>% 
  group_by(anio_asignacion) %>%
  summarise(total = round(mean(nota_total), 1)) %>% 
  spread(anio_asignacion, total)


data4 <- data2 %>% mutate(genero = ifelse(genero==1,"Hombre", "Mujer"))
ggplot(data4, aes(x = factor(genero), y = nota_total, na.rm = T)) + 
  geom_boxplot(aes(colour = factor(genero)), show.legend = F) + 
  theme(axis.text.x = element_text(hjust = 1)) +
  labs(x = "", y = "Nota final") +
  scale_color_discrete(name = "Género")

################################################################################

# Tabla de asignados
asignados <- data2 %>%
  group_by(genero, anio_asignacion) %>%
  summarise(total = n()) 
asignados

# Tabla de aprobados
aprobados <- data2 %>% filter(nota_total>=61) %>%
  group_by(genero, anio_asignacion) %>%
  summarise(total = n())
aprobados <- aprobados %>% spread(tipo_periodo, total)
aprobados

# Resultado
porcentaje_aprobación <- data.frame (
  genero = aprobados$genero,
  anio_ingreso = aprobados$anio_asignacion,
  total = round((aprobados$total*100)/asignados$total, 1)
)
porcentaje_aprobación <- porcentaje_aprobación %>% 
  mutate(genero = ifelse(genero==1,"Hombre", "Mujer"))

porcentaje_aprobación

View(porcentaje_aprobación %>% spread(anio_ingreso, total))

ggplot(porcentaje_aprobación, 
       aes(anio_ingreso, total, colour = genero, group = genero)) + 
  geom_line(size = 1, show.legend = F) +
  geom_point(aes(size = total)) +
  labs(x = "Año", y = "% de aprobación") +
  scale_color_discrete(name = "Género")



# Por fila
asignados <- data2 %>% group_by(genero) %>% summarise(total = n())
aprobados <- data2 %>% filter(nota_total>=61) %>% 
  group_by(genero) %>% summarise(total = n())
round((aprobados$total*100)/asignados$total, 1)


# Por columnas
asignados <- data2 %>% group_by(anio_asignacion) %>% summarise(total = n())
aprobados <- data2 %>% filter(nota_total>=61) %>%
  group_by(anio_asignacion) %>%  summarise(total = n())
round((aprobados$total*100)/asignados$total, 1)


# Tabla de asignados total
asignados <- data2 %>% summarise(total = n())
aprobados <- data2 %>% filter(nota_total>=61) %>% summarise(total = n())

round((aprobados$total*100)/asignados$total, 1)

################################################################################