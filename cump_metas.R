# Importar datos
datos <- read.csv("../Cumplimiento vs metas.csv", na.strings = "#N/A")
str(datos)



# Prácticas que NO se cumplían en LB ------------------------------------------------


# Número de prácticas por seleccionar, por productor
pract.opcion.meta <- nrow(datos) - sum(datos$cumple_lb, na.rm = TRUE)
n.productores <- nrow(datos) / 50
pract.opcion.meta.prod <- pract.opcion.meta / n.productores

# Prácticas seleccionadas como meta, por productor
pract.meta.prod <- sum(datos$meta, na.rm = TRUE) / n.productores

# Porcentaje de prácticas con meta como porcentaje de no cumplidas
pract.meta.porc <- pract.meta.prod / pract.opcion.meta.prod

# Cumplimiento de las prácticas seleccionadas como metas, por productor
datos.m <- datos[datos$meta == 1, ]
pract.cump.m <- sum(datos.m$cumple_seg, na.rm = TRUE) / n.productores



# Prácticas que SÍ se cumplían en LB ------------------------------------------------

# Número de prácticas cumplidas en LB, por productor
pract.cump.lb <- sum(datos$cumple_lb, na.rm = TRUE) 
pract.cump.lb.prod <- pract.cump.lb / n.productores

# Cumplimiento de prácticas que ya cumplían en LB, por productor
datos.c <- datos[datos$cumple_lb == 1, ]
pract.cump.lb.seg <- sum(datos.c$cumple_seg, na.rm = TRUE) / n.productores







# Nivel de cumplimiento en prácticas seleccionadas como meta
cump.por.practica <- aggregate(datos.m, by = select(datos, ))






# Mantener solo prácticas con meta
datos.meta <- datos[datos$Meta == 1, ]

# Promedio de prácticas con meta cumplidas
n.productores <- nrow(datos) / 50
prom.cump <- colSums(datos$Cumple)

# Promedio de practicas con meta