
asignaciones <- data2 %>% 
  group_by(anio_asignacion, anio_ingreso) %>% 
  tally()
asignaciones
asignaciones %>% spread(anio_asignacion, n)

aprobados <- data2 %>% 
  filter(nota_total >= 61) %>%
  group_by(anio_asignacion, anio_ingreso) %>% 
  tally()
aprobados
aprobados %>% spread(anio_asignacion, n)

porcentaje_aprobacion <- data.frame(
  cohorte = aprobados$anio_ingreso,
  anio = aprobados$anio_asignacion,
  porcentaje = round((aprobados$n*100)/asignaciones$n, 1)
)

View(porcentaje_aprobacion %>% spread(anio, porcentaje))


asignados <- data2 %>% group_by(anio_ingreso) %>% summarise(total = n())
aprobados <- data2 %>% filter(nota_total>=61) %>% 
  group_by(anio_ingreso) %>% summarise(total = n())
round((aprobados$total*100)/asignados$total, 1)

################################################################################

# Creditos asignados
c_asignados <- data3 %>% 
  group_by(anio_asignacion, anio_ingreso) %>%
  summarise(total = sum(creditos))
c_asignados
View(c_asignados %>% spread(anio_asignacion, total))


# Dreditos aprobados
c_aprobados <- data3 %>% 
  filter(nota_total >= 61) %>%
  group_by(anio_asignacion, anio_ingreso) %>%
  summarise(total = sum(creditos))
c_aprobados
View(c_aprobados %>% spread(anio_asignacion, total))

avance <- data.frame(
  cohorte = c_asignados$anio_ingreso,
  anio = c_asignados$anio_asignacion,
  porcentaje = round((c_aprobados$total*100)/c_asignados$total,1)
)
View(avance %>% spread(anio, porcentaje))


# Por fila
c_asignados <- data3 %>% group_by(anio_ingreso) %>% summarise(total = sum(creditos))
c_aprobados <- data3 %>% filter(nota_total >= 61) %>%
  group_by(anio_ingreso) %>% summarise(total = sum(creditos))
round((c_aprobados$total*100)/c_asignados$total, 1)

# Por columnas
c_asignados <- data3 %>% group_by(anio_asignacion) %>% summarise(total = sum(creditos))
c_aprobados <- data3 %>% filter(nota_total >= 61) %>%
  group_by(anio_asignacion) %>% summarise(total = sum(creditos))
round((c_aprobados$total*100)/c_asignados$total, 1)


# Total
c_asignados <- data3 %>% summarise(total = sum(creditos))
c_asignados
c_aprobados <- data3 %>% filter(nota_total >= 61) %>% summarise(total = sum(creditos))
c_aprobados
round((c_aprobados$total*100)/c_asignados$total, 1)

################################################################################

data3 %>% select(nombre_curso, creditos, obligaroriedad) %>%
  unique() %>%
  summarise(nc = sum(creditos))
  
