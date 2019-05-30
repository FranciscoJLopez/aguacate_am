## script para contexto

# Extraccion de tablas para clasificar cultivos segun su grupo natural;
# las tablas estan contenidas en el documento nagrop_full.pdf.
# En principio se extaerían con tabulazer, pero se generó un error
# por construcción de las tablas, por lo que se procedio con la copia
# manual de las tablas a un archivo de texto gn.txt

library(tabulizer)

path_pdf <- "C:/Users/OfficeMax/Desktop/nagrop_full.pdf"

gn_cer <- extract_tables(path_pdf, pages = 266)
gn_cer <- gn_cer[[1]][4:11,]
gn_cer[,4] <- stringr::str_replace(gn_cer[,4], "Ã", "í")
gn_cer <- data.frame(gn_cer, stringsAsFactors = FALSE)
names(gn_cer) <- c("id_gn", "gn", "id_cult", "cult")

gn_epm <- extract_tables(path_pdf, pages = 267)
gn_epm <- gn_epm[[1]][4:nrow(gn_epm[[1]]),]
gn_epm[,4] <- stringr::str_replace(gn_epm[,4], "Ã©", "é")
gn_epm[,4] <- stringr::str_replace(gn_epm[,4], "Ã³", "ó")
gn_epm[,4] <- stringr::str_replace(gn_epm[,4], "Ã", "í")
gn_epm <- data.frame(gn_epm, stringsAsFactors = FALSE)
names(gn_epm) <- c("id_gn", "gn", "id_cult", "cult")

gn_for <- extract_tables(path_pdf, pages = 268)
gn_for <- gn_for[[1]][4:nrow(gn_for[[1]]),]
gn_for[,4] <- stringr::str_replace(gn_for[,4], "Ã©", "é")
gn_for[,4] <- stringr::str_replace(gn_for[,4], "Ã³", "ó")
gn_for[,4] <- stringr::str_replace(gn_for[,4], "Ãº", "ú")
gn_for[,4] <- stringr::str_replace(gn_for[,4], "Ã±", "ñ")
gn_for[,4] <- stringr::str_replace(gn_for[,4], "Ã", "í")
gn_for <- data.frame(gn_for, stringsAsFactors = FALSE)
names(gn_for) <- c("id_gn", "gn", "id_cult", "cult") 

gn_fru01 <- extract_tables(path_pdf, pages = 269)
gn_fru01 <- gn_fru01[[1]][4:nrow(gn_fru01[[1]]),]
gn_fru01[,4] <- stringr::str_replace(gn_fru01[,4], "Ã©", "é")
gn_fru01[,4] <- stringr::str_replace(gn_fru01[,4], "Ã¡", "á")
gn_fru01[,4] <- stringr::str_replace(gn_fru01[,4], "Ã³", "ó")
gn_fru01[,4] <- stringr::str_replace(gn_fru01[,4], "Ãº", "ú")
gn_fru01[,4] <- stringr::str_replace(gn_fru01[,4], "Ã±", "ñ")
gn_fru01[,4] <- stringr::str_replace(gn_fru01[,4], "Ã", "í")
gn_fru01 <- data.frame(gn_fru01, stringsAsFactors = FALSE)
names(gn_fru01) <- c("id_gn", "gn", "id_cult", "cult") 

gn_fru02 <- extract_tables(path_pdf, pages = 270)
gn_fru02 <- gn_fru02[[1]][4:nrow(gn_fru02[[1]]),]
gn_fru02[,4] <- stringr::str_replace(gn_fru02[,4], "Ã©", "é")
gn_fru02[,4] <- stringr::str_replace(gn_fru02[,4], "Ã¡", "á")
gn_fru02[,4] <- stringr::str_replace(gn_fru02[,4], "Ã³", "ó")
gn_fru02[,4] <- stringr::str_replace(gn_fru02[,4], "Ãº", "ú")
gn_fru02[,4] <- stringr::str_replace(gn_fru02[,4], "Ã±", "ñ")
gn_fru02[,4] <- stringr::str_replace(gn_fru02[,4], "Ã", "í")
gn_fru02 <- data.frame(gn_fru02, stringsAsFactors = FALSE)
names(gn_fru02) <- c("id_gn", "gn", "id_cult", "cult")

gn_hor01 <- extract_tables(path_pdf, pages = 271)
gn_hor01 <- gn_hor01[[1]][4:nrow(gn_hor01[[1]]),]
gn_hor01[,4] <- stringr::str_replace(gn_hor01[,4], "Ã©", "é")
gn_hor01[,4] <- stringr::str_replace(gn_hor01[,4], "Ã¡", "á")
gn_hor01[,4] <- stringr::str_replace(gn_hor01[,4], "Ã³", "ó")
gn_hor01[,4] <- stringr::str_replace(gn_hor01[,4], "Ãº", "ú")
gn_hor01[,4] <- stringr::str_replace(gn_hor01[,4], "Ã±", "ñ")
gn_hor01[,4] <- stringr::str_replace(gn_hor01[,4], "Ã", "í")
gn_hor01 <- data.frame(gn_hor01, stringsAsFactors = FALSE)
names(gn_hor01) <- c("id_gn", "gn", "id_cult", "cult")

gn_hor02 <- extract_tables(path_pdf, pages = 272)
gn_hor02 <- gn_hor02[[1]][4:nrow(gn_hor02[[1]]),]
gn_hor02[,4] <- stringr::str_replace(gn_hor02[,4], "Ã©", "é")
gn_hor02[,4] <- stringr::str_replace(gn_hor02[,4], "Ã¡", "á")
gn_hor02[,4] <- stringr::str_replace(gn_hor02[,4], "Ã³", "ó")
gn_hor02[,4] <- stringr::str_replace(gn_hor02[,4], "Ãº", "ú")
gn_hor02[,4] <- stringr::str_replace(gn_hor02[,4], "Ã±", "ñ")
gn_hor02[,4] <- stringr::str_replace(gn_hor02[,4], "Ã", "í")
gn_hor02 <- data.frame(gn_hor02, stringsAsFactors = FALSE)
names(gn_hor02) <- c("id_gn", "gn", "id_cult", "cult")


# Inicio del procesamiento del archivo de texto generado
# que contiene los datos de grupos naturales de cultivos 

gn <- read_csv("~/OneDrive/aguacate19/aux_doc/gn.txt", col_names = FALSE,
               locale = locale(encoding = "WINDOWS-1252"))

library(stringr)

gn01 <- str_split(gn$X1, pattern = " ", n = 4) %>% purrr::map_chr(~.x[1]) %>% data_frame(id_gn = .)
gn02 <- str_split(gn$X1, pattern = " ", n = 4) %>% purrr::map_chr(~.x[2]) %>% data_frame(gn = .)
gn03 <- str_split(gn$X1, pattern = " ", n = 4) %>% purrr::map_chr(~.x[3]) %>% data_frame(id_cult = .)
gn04 <- str_split(gn$X1, pattern = " ", n = 4) %>% purrr::map_chr(~.x[4]) %>% data_frame(cult = .)

df_gn <- cbind(gn01, gn02, gn03, gn04)

# tarea de limpieza
rm(gn01, gn02, gn03, gn04)

# tareas de verificacion
df_gn$cult %>% unique() %>% length()
str(df_gn$cult)
df_gn <- distinct(df_gn, cult, .keep_all = TRUE)


names(cultivos17)
unique(cultivos17$idcultivo) %>% length()

# adicion de nombre de cultivo al archivo original de cultivos
cultivos17 %<>% left_join(select(id_cult, idcultivo, nomcultivo), by = "idcultivo")
# creacion de tabla referencial de cultivo y su respectivo grup natural
cult_gn <- cultivos17 %>% select(nomcultivo) %>% set_colnames("cult") %>% 
  left_join(select(df_gn, gn, cult), by = "cult")

# Debido a la presencia de un gran número de casos NA en la variable gn
# se efectuan tareas de depuracion de forma iterativa para etiquetar casos con
# claros patrones
depur <- str_which(cultivos17$nomcultivo, "manojo|planta|gruesa")
# se añade un índice que permite faciltar las operaciones de filtrado
cult_gn %<>% mutate(id_depur = row_number())
depur2 <- cult_gn %>% filter(id_depur %in% depur) %>% filter(is.na(gn)) %>% select(id_depur)
str(depur2)
cult_gn <- cult_gn %>% mutate(gn = if_else(id_depur %in% depur2$id_depur, "Ornamentales", gn))
depur3 <- cult_gn %>% filter(is.na(gn)) %>% filter(grepl("Semilla*", cult))
cult_gn <- cult_gn %>% mutate(gn = if_else(id_depur %in% depur3$id_depur, "Semillas_para_siembras", gn))
depur4 <- cult_gn %>% filter(is.na(gn)) %>% filter(grepl("*[Vv]ivero*", cult))
cult_gn <- cult_gn %>% mutate(gn = if_else(id_depur %in% depur4$id_depur, "Otros_cultivos", gn))
depur5 <- cult_gn %>% filter(is.na(gn)) %>% filter(grepl("*forraje*", cult))
cult_gn <- cult_gn %>% mutate(gn = if_else(id_depur %in% depur5$id_depur, "Forrajes", gn))
depur6 <- cult_gn %>% filter(is.na(gn)) %>% filter(grepl("*grano*|*aíz*", cult))
cult_gn <- cult_gn %>% mutate(gn = if_else(id_depur %in% depur6$id_depur, "Cereales", gn))
depur7 <- cult_gn %>% filter(is.na(gn))
depur7 <- edit(depur7) # depuración manual
cult_gn <- cult_gn %>% mutate(gn = replace(gn, is.na(gn), depur7$gn))

# Añadiendo la variable grupos naturaes (gn) a la tabla de cultivos
cul17 <- cultivos17 %>% left_join(select(cult_gn, gn, cult), by = c("nomcultivo" = "cult")) %>% 
  select(-idcultivo)
cul17 <- cul17 %>% mutate(gn = as.factor(gn))
str(cul17)

# ejemplo de uso de grupos naturales como factores
ggplot(cul17, aes(x = reorder(forcats::fct_infreq(gn), desc(forcats::fct_infreq(gn))))) +
  geom_bar() +
  coord_flip()

cul17 %>% 
  #mutate(gn = forcats::fct_lump(gn, n=5, other_level = "Otros")) %>% 
  group_by(gn) %>%
  summarise(val = sum(val))

cul17 %>%
  mutate(gn = forcats::fct_reorder(gn, sup_sem)) %>% 
  mutate(gn = forcats::fct_lump(gn, prop=.05, other_level = "Otros")) %>% 
  group_by(gn) %>% 
  summarise(sup_sem = sum(sup_sem)) %>%
  mutate(sup_sem = sup_sem/sum(sup_sem)) %>% 
  ggplot(aes(gn, sup_sem)) +
    geom_col() +
    coord_flip()
