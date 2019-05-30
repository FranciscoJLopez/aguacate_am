wld_trd_avc1 <- read.csv("~/Desktop/comtrade.csv", header = TRUE)
head(wld_trd_avc1)
unique(wld_trd_avc1$Year)

wld_trd_avc2 <- read.csv("~/Desktop/comtrade2.csv", header = TRUE)
head(wld_trd_avc2)
unique(wld_trd_avc2$Year)

wld_trd_avc <- rbind(wld_trd_avc1, wld_trd_avc2)
rm(wld_trd_avc1, wld_trd_avc2)

head(wld_trd_avc)

wld_trd_avc <- wld_trd_avc %>% 
  select(Year, Trade.Flow, Reporter, Reporter.ISO, Commodity.Code, Commodity,
         Qty.Unit, Netweight..kg., Trade.Value..US..) %>% 
  set_names(c("Año", "Flujo", "nom_pais", "País", "comm_code", "comm",
              "um", "vol_kg", "val_usd"))

wld_trd_avc %>% 
  filter(Año == 2017, Flujo == "Export") %>% 
  mutate(vol_part = vol_kg/sum(vol_kg)) %>% 
  select(País, vol_part) %>% 
  arrange(desc(vol_part))

wld_trd_avc %>% 
  filter(Año == 2017, Flujo == "Import", !is.na(vol_kg)) %>% 
  mutate(vol_part = vol_kg/sum(vol_kg)) %>% 
  select(País, vol_part) %>% 
  arrange(desc(vol_part)) %>% 
  mutate(vol_part = scales::percent(vol_part)) %>% str()

wld_trd_avc %>% 
  filter(Flujo == "Export") %>% 
  select(vol_kg, Año) %>% 
  group_by(Año) %>% 
  summarise(vol = sum(vol_kg, na.rm = T))

wld_trd_avc %>% 
  filter(Flujo == "Import", !is.na(vol_kg)) %>% 
  select(País, Año) %>% 
  group_by(Año) %>% 
  summarise(n = n()) %>% 
  summarise(ns = mean(n))


tema_gg <- theme(panel.background = element_blank(),
                 panel.grid = element_blank(),
                 text = element_text(family = "Lato"),
                 plot.title = element_text(face = "bold"),
                 plot.subtitle = element_text(face = "bold"),
                 axis.ticks.x = element_blank(),
                 axis.text = element_text(family = "Lato Light"),
                 axis.line.y = element_line(color = "lightgrey"))

str(wld_trd_avc)
wld_trd_avc %>%
  filter(Año == 2017, Flujo == "Import", !is.na(vol_kg), vol_kg != 0) %>% 
  select(País, vol_kg, val_usd) %>% 
  mutate(p_usd = val_usd / vol_kg) %>%
  select(País, vol_kg) %>% 
  arrange(desc(vol_kg)) %>% 
  mutate(part = vol_kg / sum(vol_kg), part_cum = cumsum(part)) %>% 
  mutate(País = as.character(País)) %>% 
  mutate(pais_otros = if_else(part_cum > 0.8, "Otros", País)) %>% 
  group_by(pais_otros) %>% 
  summarise(vol_kg = sum(vol_kg), part = sum(part)) %>%
  mutate(part_f = scales::percent(part), part_f2 = if_else(part < 0.072, "", part_f)) %>% 
  mutate(aux = if_else(pais_otros == "Otros", 1, 0)) %>% 
  arrange(aux, desc(vol_kg)) %>%
  mutate(aux2 = row_number()) %>%
  mutate(difer = if_else(part == max(part), T, F)) %>% 
  ggplot(aes(x = reorder(pais_otros, desc(aux2)), y = part)) + 
    geom_bar(stat = "identity", aes(fill = difer)) +
    scale_fill_manual(values = c("grey85","yellowgreen")) +
    labs(x = "", y = "",
         title = "Países importadores de aguacate en 2017",
         subtitle = "Porcentajes de participación") +
    geom_text(aes(label = part_f2),
            color = "white",
            hjust = 1,
            size = 8,
            fontface = "bold",
            nudge_y = -0.005) +
    coord_flip() +
    tema_gg +
    theme(legend.position = "none",
        axis.text.x = element_blank())


wld_trd_avc %>% 
  filter(Año == 2017, Flujo == "Import", !is.na(vol_kg)) %>% 
  summarise(vol_kg = scales::comma(sum(vol_kg)/1000))

library(tidyr)

wld_trd_avc %>% 
  filter(Flujo == "Import", !is.na(vol_kg), vol_kg != 0) %>% 
  group_by(País) %>% 
  nest() %>% 
  mutate(long = purrr::map_dbl(data, ~nrow(.x))) %>% 
  filter(long == 10) %>% 
  mutate(tcap = purrr::map_dbl(data, ~ exp(lm(log(vol_kg) ~ Año, .x)$coefficient[2])-1)) %>% 
  left_join((wld_trd_avc %>% 
               filter(Año == 2017, Flujo == "Import", !is.na(val_usd), val_usd != 0) %>% 
               select(País, val_usd) %>% 
               mutate(peso = val_usd / filter(., País == "USA") %>% select(val_usd) %>% pull()))) %>% 
  mutate(tcapr = tcap * peso) %>%   
  arrange(desc(tcap))


# principales re-exportadores
wld_trd_avc %>% 
  filter(Año == 2017, Flujo == "Re-Import", !is.na(vol_kg)) %>% 
  select(País, vol_kg) %>% 
  arrange(desc(vol_kg))

# 

wld_trd_all <-  read.csv("~/Desktop/comtrade3.csv", header = TRUE)
wld_trd_all <- wld_trd_all %>% 
  select(Year, Trade.Flow, Reporter, Reporter.ISO, Commodity.Code, Commodity,
         Qty.Unit, Alt.Qty.Unit, Trade.Value..US..) %>% 
  set_names(c("Año", "Flujo", "nom_pais", "País", "comm_code", "comm",
              "um", "vol_kg", "val_usd"))

wld_trd_all %>%
  filter(grepl("^08", comm_code), Flujo == "Export", !is.na(vol_kg)) %>% 
  group_by(comm) %>% 
  summarise(vol = sum(vol_kg), val = sum(val_usd))
  mutate(precio = val_usd/vol_kg) %>% 
  select(comm, precio) %>% 
  arrange(desc(precio))
