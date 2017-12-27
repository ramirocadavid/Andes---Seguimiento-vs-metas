# Importar datos --------------------------------------------------------------------

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

# Cumplimiento de prácticas cumplidas en LB, por productor
datos.c <- datos[datos$cumple_lb == 1, ]
pract.cump.lb.seg <- sum(datos.c$cumple_seg, na.rm = TRUE) / n.productores

# Por práctica
library(dplyr)
cump.lb.seg.practica <- aggregate(select(datos.c, cumple_lb, cumple_seg), 
                                  by = list(datos.c$practica), FUN = sum, na.rm = TRUE)
cump.lb.seg.practica <- data.frame(cump.lb.seg.practica,
                                   diferencia = cump.lb.seg.practica$cumple_seg - 
                                         cump.lb.seg.practica$cumple_lb)


# Prácticas que no cumplían y no meta -----------------------------------------------

# Número de productores
pract.nocump.nometa <- nrow(datos[datos$cumple_lb == 0 & datos$meta == 0, ])
pract.nocump.nometa.prod <- pract.nocump.nometa / n.productores

# Prácticas cumplidas en seguimiento por productor
datos.nc.nm <- datos[datos$cumple_lb == 0 & datos$meta == 0, ]
pract.nocump.nometa.seg <- sum(datos.nc.nm$cumple_seg, na.rm = TRUE)
pract.nocump.nometa.seg.prod <- pract.nocump.nometa.seg / n.productores

# Por práctica
ncnm.seg.practica <- aggregate(select(datos.nc.nm, cumple_lb, cumple_seg), 
                                  by = list(datos.nc.nm$practica), FUN = sum, na.rm = TRUE)
ncnm.seg.practica <- data.frame(ncnm.seg.practica,
                                   diferencia = ncnm.seg.practica$cumple_seg - 
                                      ncnm.seg.practica$cumple_lb)


# Prácticas que no se cumplían y Sí meta --------------------------------------------

# Número de productores
pract.ncm <- nrow(datos[datos$cumple_lb == 0 & datos$meta == 1, ])
pract.ncm.prod <- pract.ncm / n.productores

# Prácticas cumplidas en seguimiento por productor
datos.ncm <- datos[datos$cumple_lb == 0 & datos$meta == 1, ]
pract.ncm.seg <- sum(datos.ncm$cumple_seg, na.rm = TRUE)
pract.ncm.seg.prod <- pract.ncm.seg / n.productores

# Por práctica
ncm.seg.practica <- aggregate(select(datos.ncm, cumple_lb, cumple_seg), 
                               by = list(datos.ncm$practica), FUN = sum, na.rm = TRUE)
ncm.seg.practica <- data.frame(ncm.seg.practica,
                                diferencia = ncm.seg.practica$cumple_seg - 
                                      ncm.seg.practica$cumple_lb)


# Cumplimiento de prácticas con respecto a metas ------------------------------------

# Agrupar por práctica
library(dplyr)
practicas <- aggregate(x = select(datos.m, cumple_lb, meta, cumple_seg),
                       by = select(datos.m, practica), FUN = sum, na.rm = TRUE)

# Calcular cumplimiento como porcentaje de meta
practicas <- data.frame(practicas, 
                        cump.por.meta = practicas$cumple_seg / practicas$meta)

# Ordenar y exportar a excel
practicas <- practicas[order(practicas$cump.por.meta, decreasing = TRUE), ]
library(xlsx)
write.xlsx(practicas, "../practicas.xlsx")
