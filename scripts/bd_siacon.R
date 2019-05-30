library(DBI)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tufte)

#### DATOS ####

# Conexion y exploracion de base de datos SIACON_NG
con <- dbConnect(RSQLite::SQLite(), "C:/Users/OfficeMax/Documents/SIACON_NG/siacon-ng.db")
dbListTables(con)

# Obtencion de clave (id) de de entidades
dbListFields(con, "cat_estado")
id_edos <- dbReadTable(con, "cat_estado")

# Obrencion de clave de cultivos
id_cult <- dbReadTable(con, "cat_cultivo")

# Obtencion de grupo natural
id_gpo <- dbReadTable(con, "cat_gruponatural")

# Obtencion de clave del aguacate
dbListFields(con, "cat_cultivo")
id_agt <- dbReadTable(con, "cat_cultivo") %>% filter(nomcultivo == "Aguacate")

# Obtencion de clave de variedades de aguacate;
# base de datos de mayor tamanio, requiere SQL query para mayor rapidez
dbListFields(con, "cat_variedad")
res <- dbSendQuery(con, "SELECT *
                            FROM cat_variedad
                            WHERE idcultivo = 5060000")
id_var <- dbFetch(res)
dbClearResult(res)

# Obtencion de tabla de datos del aguacate por estados;
# base de gran tamaño, requiere SQL query para mayor rapidez
dbListFields(con, "agro_cierre_estatal")
dbReadTable(con, "agro_cierre_estatal") %>% filter(idcultivo == 5060000) %>% select(anio) %>% distinct()
res <- dbSendQuery(con, "SELECT *
                        FROM agro_cierre_estatal
                        WHERE idcultivo = 5060000")
agt_nal <- dbFetch(res)
dbClearResult(res)

# Obtencion de tabla de datos del aguacate por municipios;
# base de gran tamaño, requiere SQL query para mayor rapidez
res <- dbSendQuery(con, "SELECT *
                        FROM agro_cierre_municipal
                        WHERE idcultivo = 5060000")
agt_mun <- dbFetch(res)
dbClearResult(res)

# Obtencion de tabla de todos los cultivos en 2017
dbListFields(con, "agro_cierre_estatal")
res <- dbSendQuery(con, "SELECT idcultivo,
                                anio,
                                SUM(volumenproduccion) AS vol,
                                SUM(sembrada) AS sup_sem,
                                SUM(cosechada) AS sup_cos,
                                SUM(siniestrada) AS sup_sin,
                                SUM(valorproduccion) AS val,
                                SUM(volumenproduccion * rendimiento) / SUM(volumenproduccion) AS yield,
                                SUM(volumenproduccion * preciomediorural) / SUM(volumenproduccion) AS pmr
                        FROM agro_cierre_estatal
                        WHERE anio = 2017
                        GROUP BY idcultivo")
cultivos17 <- dbFetch(res)
dbClearResult(res)

# Desconexion a la base de datos
dbDisconnect(con)


##### Exploracion ####

# datos de contexto: posicion del aguacate ante otros cultivos al 2017
cultivos17 %>% left_join(select(id_cult, idcultivo, nomcultivo), by = "idcultivo") %>% 
  select(-anio) %>% mutate(aprov = val/sup_cos) %>% top_n(20, aprov) %>% 
  select(nomcultivo, idcultivo)

agt_nal %>%
  group_by(anio) %>% 
  summarise(vol = sum(volumenproduccion)) %>% 
  ggplot(aes(anio, vol)) +
           geom_line()

agt_nal %>% 
  filter(anio == 2017) %>% 
  select(idestado, volumenproduccion) %>%
  left_join(select(id_edos, idestado, nomestado), id = "idestado") %>% 
  ggplot(aes(nomestado, volumenproduccion)) +
    geom_col() +
    coord_flip()

agt_nal %>% 
  filter(anio == 2017) %>% 
  select(idvariedad, volumenproduccion) %>% 
  group_by(idvariedad) %>% 
  summarise(vol = sum(volumenproduccion)) %>% 
  left_join(select(id_var, idvariedad, nomvariedad), by = "idvariedad")

agt_nal %>% 
  filter(idestado == 16) %>% 
  group_by(anio) %>% 
  summarise(vol = sum(volumenproduccion)) %>% 
  ggplot(aes(anio, vol)) +
    geom_line()

agt_mun %>% 
  filter(idestado == 14, anio == 2017) %>% 
  group_by(anio, idmunicipio) %>% 
  summarise(vol = sum(volumenproduccion)) %>% 
  ggplot(aes(idmunicipio, vol)) + 
    geom_col() +
    coord_flip()
