library(extrafont)
# font_import() # use this function first if R was updated
loadfonts(device = "win")
library(tidyverse)

# generating tables of avocado global commerce
wld_trd_avc1 <- read.csv("~/OneDrive/R/repos/aguacate_am/aux_doc/comtrade.csv", header = TRUE)
head(wld_trd_avc1)
unique(wld_trd_avc1$Year)

wld_trd_avc2 <- read.csv("~/OneDrive/R/repos/aguacate_am/aux_doc/comtrade2.csv", header = TRUE)
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

# preparing common elements from custom plots
tema_gg <- theme(panel.background = element_blank(),
                 panel.grid = element_blank(),
                 text = element_text(family = "Lato"),
                 plot.title = element_text(face = "bold"),
                 plot.subtitle = element_text(face = "bold"),
                 axis.ticks.x = element_blank(),
                 axis.text = element_text(family = "Lato Light"),
                 axis.line.y = element_line(color = "lightgrey"))

# main importers/exporters
str(wld_trd_avc)
wld_trd_avc %>%
  filter(Año == 2017, Flujo == "Export", !is.na(vol_kg), vol_kg != 0) %>% 
  select(País, vol_kg, val_usd) %>% 
  mutate(p_usd = val_usd / vol_kg) %>%
  select(País, val_usd) %>% 
  arrange(desc(val_usd)) %>% 
  mutate(part = val_usd / sum(val_usd), part_cum = cumsum(part)) %>% 
  mutate(País = as.character(País)) %>% 
  mutate(pais_otros = if_else(part_cum > 0.88, "Otros", País)) %>% 
  group_by(pais_otros) %>% 
  summarise(val_usd = sum(val_usd), part = sum(part)) %>%
  mutate(part_f = scales::percent(part), part_f2 = if_else(part < 0.06, "", part_f)) %>% 
  mutate(aux = if_else(pais_otros == "Otros", 1, 0)) %>% 
  arrange(aux, desc(val_usd)) %>%
  mutate(aux2 = row_number()) %>%
  mutate(difer = if_else(part == max(part), T, F)) %>% 
  ggplot(aes(x = reorder(pais_otros, desc(aux2)), y = part)) + 
    geom_bar(stat = "identity", aes(fill = difer)) +
    scale_fill_manual(values = c("grey85","yellowgreen")) +
    labs(x = "", y = "",
         title = "Países exporatdores de aguacate en 2017",
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

# single data: total imports/exports
wld_trd_avc %>% 
  filter(Año == 2017, Flujo == "Import", !is.na(vol_kg)) %>% 
  summarise(vol_kg = scales::comma(sum(vol_kg)/1000))

# TCAP: average annual rate of growth
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
  filter(Año == 2017, Flujo == "Re-Export", !is.na(vol_kg)) %>% 
  select(País, vol_kg) %>% 
  arrange(desc(vol_kg))


# retreiving data for selected importer countries
data01 <- read.csv("https://comtrade.un.org/api/get?max=50000&type=C&freq=M&px=HS&ps=2017&r=251&p=all&rg=1%2C2&cc=080440&uitoken=8458e2b28e295012d38a1c8075ea8e83&fmt=csv")
data02 <- read.csv("https://comtrade.un.org/api/get?max=50000&type=C&freq=M&px=HS&ps=2016&r=251&p=all&rg=1%2C2&cc=080440&uitoken=8458e2b28e295012d38a1c8075ea8e83&fmt=csv")
data03 <- read.csv("https://comtrade.un.org/api/get?max=50000&type=C&freq=M&px=HS&ps=2015&r=251&p=all&rg=1%2C2&cc=080440&uitoken=8458e2b28e295012d38a1c8075ea8e83&fmt=csv")
data04 <- read.csv("~/OneDrive/R/repos/aguacate_am/aux_doc/data04.csv")
data05 <- read.csv("https://comtrade.un.org/api/get?max=50000&type=C&freq=M&px=HS&ps=2013&r=251&p=all&rg=1%2C2&cc=080440&uitoken=8458e2b28e295012d38a1c8075ea8e83&fmt=csv")
data06 <- read.csv("https://comtrade.un.org/api/get?max=50000&type=C&freq=M&px=HS&ps=2012&r=251&p=all&rg=1%2C2&cc=080440&uitoken=8458e2b28e295012d38a1c8075ea8e83&fmt=csv")
data07 <- read.csv("https://comtrade.un.org/api/get?max=50000&type=C&freq=M&px=HS&ps=2011&r=251&p=all&rg=1%2C2&cc=080440&uitoken=8458e2b28e295012d38a1c8075ea8e83&fmt=csv")
data08 <- read.csv("https://comtrade.un.org/api/get?max=50000&type=C&freq=M&px=HS&ps=2010&r=251&p=all&rg=1%2C2&cc=080440&uitoken=8458e2b28e295012d38a1c8075ea8e83&fmt=csv")
data09 <- read.csv("~/OneDrive/R/repos/aguacate_am/aux_doc/comtrade_m_10_5.csv")
data10 <- read.csv("~/OneDrive/R/repos/aguacate_am/aux_doc/comtrade_m_11_5.csv")
data11 <- read.csv("~/OneDrive/R/repos/aguacate_am/aux_doc/comtrade_m_12_5.csv")
data12 <- read.csv("~/OneDrive/R/repos/aguacate_am/aux_doc/comtrade_m_13_5.csv")
data13 <- read.csv("~/OneDrive/R/repos/aguacate_am/aux_doc/comtrade_m_14_5.csv")
data14 <- read.csv("~/OneDrive/R/repos/aguacate_am/aux_doc/comtrade_m_15_5.csv")
data15 <- read.csv("~/OneDrive/R/repos/aguacate_am/aux_doc/comtrade_m_16_5.csv")
data16 <- read.csv("~/OneDrive/R/repos/aguacate_am/aux_doc/comtrade_m_17_5.csv")

top6_df <- purrr::map_df(ls(pattern = "data[0-9]+"), ~rbind(get(.x)))
unique(top6_df$Period)
names(top6_df)
head(top6_df)

top6_df <- top6_df %>% select(Year, Period, Trade.Flow, Reporter.Code, Reporter,
                   Partner.Code, Partner, Commodity.Code, Commodity, Netweight..kg.,
                   Trade.Value..US..) %>% 
            set_colnames(c("Año", "Periodo", "Flujo", "Fuente_id", "Fuente",
                         "Socio_id", "Socio", "comm_id", "comm", "vol_kg", "val_usd"))

top6_df <- tidyr::separate(top6_df, Periodo, into = c(NA, "Mes"), sep = -2)

country_id <- read.csv("~/OneDrive/R/repos/aguacate_am/aux_doc/country_code.txt", sep = "\t", header = F) %>% 
  tidyr::separate(V1, into = c("codigo", "País"), sep = "(?<=[A-Z])\\s") %>% 
  mutate(País = stringr::str_trim(.$País))

top6_df <- top6_df %>% left_join(country_id, by = c("Fuente" = "País")) %>% 
  mutate(Fuente_cod = codigo) %>% select(-codigo, -Fuente_id) %>% 
  left_join(country_id, by = c("Socio" = "País")) %>% 
  mutate(Socio_cod = codigo) %>% select(-codigo, -Socio_id) %>% 
  mutate(Mes = as.integer(Mes))

unique(top6_df$Fuente_cod)
str(top6_df)

# Vamos a empezar el análisis por país
top6_df %>% 
  filter(Fuente_cod == "FRA", Flujo == "Imports", Socio == "World") %>% 
  select(Año, Mes, vol_kg, val_usd) %>% 
  arrange(Año, Mes) %>% 
  select(-vol_kg) %>% 
  mutate(periodo = as.Date(paste0(Año, "-", Mes, "-", 1))) %>% 
  ggplot(aes(periodo, val_usd)) +
  geom_line()

# verifying that world partner records and the ones from the sum of the rest of the partners are equal
# verificando que el socio world y la suma del resto de los socias sean iguales
top6_df %>% 
  filter(Fuente_cod == "GBR", Flujo == "Imports", Socio == "World") %>% 
  select(Año, Mes, vol_kg, val_usd) %>% 
  arrange(Año, Mes) %>% 
  select(-vol_kg) %>% 
  group_by(Año) %>% 
  summarise(ann_val = sum(val_usd)) %>% 
  cbind(top6_df %>% 
           filter(Fuente_cod == "GBR", Flujo == "Imports", Socio != "World") %>% 
           select(Año, Mes, vol_kg, val_usd) %>% 
           arrange(Año, Mes) %>% 
           select(-vol_kg) %>% 
           group_by(Año) %>% 
           summarise(ann_val2 = sum(val_usd)) %>% 
           select(ann_val2))

# Checking that each country has the same number of rows, otherwise the following time series
# analysis will not be correct
top6_df %>% 
  filter(Flujo == "Imports", Socio == "World") %>% 
  select(Año, Mes, Fuente_cod, val_usd) %>% 
  arrange(Año, Mes) %>%
  group_by(Fuente_cod) %>%
  tidyr::nest()

# Inspecting NLD case
top6_df %>% 
  filter(Fuente_cod == "NLD", Flujo == "Imports", Año == 2017) %>% 
  distinct(Mes)

# RESULTS
# 2014 France records were missing, bad download; It was corrected
# 2017 Netherlands records are incomplete; they are published until July

# Once corrected and verifyed we start the process
top6_mod <- top6_df %>% 
  filter(Flujo == "Imports", Socio == "World") %>% 
  select(Año, Mes, Fuente_cod, val_usd) %>% 
  arrange(Año, Mes) %>%
  mutate(periodo = as.Date(paste0(Año, "-", Mes, "-1"))) %>% 
  group_by(Fuente_cod) %>%
  tidyr::nest()

library(purrr)
library(broom)
top6_mod %>%
  mutate(ej = map(data, ~.x["val_usd"]/1000)) %>% tidyr::unnest(ej) 

ej <- top6_mod %>%
  mutate(data = map(top6_mod$data, ~.x %>% mutate(val_usd = val_usd/1000000))) %>% 
  mutate(data = map(.$data, ~.x %>% mutate(tiempo = 1:nrow(.x)))) %>% 
  mutate(mod_trend = map(.$data, ~lm(val_usd ~ tiempo + I(tiempo^2), .x))) %>% 
  mutate(pred = map(.$mod_trend, ~augment(.x))) %>%
  mutate(pred = map2(.$pred, top6_mod$data, ~.x %>% cbind(.y$periodo))) %>%
  mutate(pred = map(.$pred, ~.x %>% mutate(val_dtrd = .resid + mean(val_usd)))) %>%
  mutate(val_ts = map(.$pred, ~ts(.x$val_dtrd, start = c(2008,1), frequency = 12))) %>% 
  tidyr::unnest(pred) %>% 
  ggplot(aes(x = `.y$periodo`, y = val_dtrd)) +
    geom_line() +
    facet_wrap(~Fuente_cod, ncol = 3, scales = "free_y")

# actual monthly series and their trends 
top6_mod %>% 
  mutate(data = map(top6_mod$data, ~.x %>% mutate(val_usd = val_usd/1000000))) %>%
  tidyr::unnest(data) %>% 
  ggplot(aes(periodo, val_usd)) +
    geom_line(aes(color = "yellowgreen"), size = .5) +
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 2),
                aes(color = "lightgrey"),
                linetype = 1,
                size = .65,
                se = FALSE) +
    scale_color_manual(labels = c("Tendencia", "Valor"),
                       values = c("yellowgreen", "lightgrey"),
                       name = "") +
    labs(x = "", y = "",
         title = "Valor de las importaciones mensuales de aguacate\nen países seleccionados, 2008-2017",
         subtitle = "Millones de dólares",
         caption = "Fuente: UN Comtrade Database") +
        facet_wrap(~Fuente_cod, ncol = 3, scales = "free_y") +
    theme(legend.position = "bottom") +
    tema_gg

# getting seasonal indexes
top6_mod %>% 
  mutate(data = map(top6_mod$data, ~.x %>% mutate(val_usd = val_usd/1000000))) %>% 
  mutate(val_ts = map(.$data, ~ts(.x$val_usd, start= c(2008,1), frequency = 12))) %>% 
  mutate(indices_est = map(.$val_ts, ~unique(decompose(.x)$seasonal))) %>% 
  mutate(indices_est =  map(.$indices_est,  ~data.frame(mes = c("ene", "feb", "mar", "abr",
                                           "may", "jun", "jul", "ago",
                                           "sep", "oct", "nov", "dic"), ind = .x,
                                           orden = 1:12))) %>%
  mutate(indices_est = map(.$indices_est, ~.x %>% mutate(difer = if_else(ind %in% c(max(ind), min(ind)), T, F),
                                                         difer_t1 = if_else(ind %in% c(max(ind), min(ind)), as.character(round(ind, 1)), ""),
                                                         difer_t2 = if_else(ind == min(ind), as.character(round(ind, 1)), "")))) %>% 
  unnest(indices_est) %>% 
  ggplot(aes(reorder(mes, orden), ind)) + 
  geom_bar(stat = "identity", aes(fill = difer)) +
  scale_fill_manual(values = c("grey", "yellowgreen")) +
  labs(x = "", y = "",
       title = "Estacionalidad de las importaciones mensuales de aguacate\nen países seleccionados, 2008-2017",
       subtitle = "Índice estacional",
       caption = "Fuente: UN Comtrade Database") +
  facet_wrap(~Fuente_cod, ncol = 2, scales = "free_y") +
  geom_text(aes(label = difer_t1),
            color = "white",
            vjust = 1,
            nudge_y = -.05,
            fontface = "bold",
            size = 3,
            family = "Lato") +
  geom_text(aes(label = difer_t2),
            color = "white",
            vjust = "inward",
            #nudge_y = 1,
            fontface = "bold",
            size = 3,
            family = "Lato") +
  tema_gg +
  theme(legend.position = "none")

# plot of volatility
top6_vol <- top6_mod %>% 
  mutate(data = map(top6_mod$data, ~.x %>% mutate(val_usd = val_usd/1000000))) %>% 
  mutate(val_ts = map(.$data, ~ts(.x$val_usd, start= c(2008,1), frequency = 12))) %>% 
  mutate(res = map(.$val_ts, ~as.vector(decompose(.x, type = "multiplicative")$random))) %>%
  mutate(plot_df = map2(.$res, .$data, ~data.frame(res = .x, val_usd = .y$val_usd, periodo = .y$periodo))) %>%
  mutate(plot_df = map(.$plot_df, ~.x %>% mutate(val_res = res, #* mean(val_usd, na.rm = T),
                                                 mean_val_res = mean(val_res, na.rm = T),
                                                lim_sup = mean_val_res + sd(val_res, na.rm = T),
                                                lim_inf = mean_val_res - sd(val_res, na.rm = T)))) %>% 
  #mutate(plot_df = map(.$plot_df, ~.x %>% mutate(val_sd = sd(val_res, na.rm = T)))) %>% 
  #mutate(plot_df = map(.$plot_df, ~.x %>% mutate(est_dif = if_else(val_res > lim_sup, val_res - lim_sup,
                                                 #if_else(val_res < lim_inf, abs(val_res - lim_inf), 0))))) %>% 
  #mutate(plot_df = map(.$plot_df, ~.x %>% filter(!is.na(est_dif), est_dif != 0))) %>%
  #mutate(metric_steady = map_dbl(.$plot_df, ~sum(.x$est_dif) / sum(.x$mean_val_res)))
  unnest(plot_df)
top6_vol %>% # an spcecific data frame was created to simplify the ploting task
  ggplot(aes(x = periodo)) + #geom_histogram(aes(res)) + facet_wrap(~Fuente_cod, ncol = 3)
  geom_rect(aes(xmin = min(periodo),
                xmax = max(periodo),
                ymin = lim_inf,
                ymax = lim_sup),
            fill = "grey95",
            alpha = .5) +
  geom_line(aes(y = val_res),
            color = "grey",
            size = .5) +
  geom_line(aes(y = mean_val_res),
            color = "yellowgreen",
            size = .75,
            linetype = 2) +
  labs(x = "", y = "",
       title = "Volatilidad de las importaciones mensuales de aguacate\nen países seleccionados, 2008-2017",
       subtitle = "Millones de dólares",
       caption = "UN Comtrade Database") +
  tema_gg +
  facet_wrap(~Fuente_cod, ncol = 3)
  
# Constructing seasonality and volatility metrics
top6_mod %>% 
  mutate(data = map(top6_mod$data, ~.x %>% mutate(val_usd = val_usd/1000000))) %>% 
  mutate(val_ts = map(.$data, ~ts(.x$val_usd, start= c(2008,1), frequency = 12))) %>% 
  mutate(indices_est = map(.$val_ts, ~unique(decompose(.x)$seasonal))) %>% 
  mutate(indices_est =  map(.$indices_est,  ~data.frame(mes = c("ene", "feb", "mar", "abr",
                                                                "may", "jun", "jul", "ago",
                                                                "sep", "oct", "nov", "dic"), ind = .x,
                                                        orden = 1:12))) %>% 
  unnest(indices_est) %>% #mutate(rango_lag = lag(ind, n = -1))
  group_by(Fuente_cod) %>% 
  summarise(fact_est = sd(ind)) %>% 
  cbind(top6_mod %>% 
          mutate(data = map(top6_mod$data, ~.x %>% mutate(val_usd = val_usd/1000000))) %>% 
          mutate(val_ts = map(.$data, ~ts(.x$val_usd, start= c(2008,1), frequency = 12))) %>% 
          mutate(val_dt_ds = map(.$val_ts, ~data_frame(val_dt_ds = decompose(.x, type = "multiplicative")$random))) %>% 
          unnest(val_dt_ds) %>% 
          group_by(Fuente_cod) %>% 
          summarise(volat = sd(val_dt_ds, na.rm = T))) %>% 
  set_names(c("País", "Estacionalidad", "Fuente", "Volatilidad")) %>% 
  select(-Fuente) %>% 
  mutate(Estacionalidad = round(Estacionalidad, 2), Volatilidad = round(Volatilidad, 2))

# data frame of imports sources
top6_mod2 <- top6_df %>% 
  filter(Flujo == "Imports", Socio_cod != "World", Año == 2017) %>% 
  select(Año, Fuente_cod, Socio_cod, val_usd) %>% 
  group_by(Fuente_cod, Socio_cod) %>%
  summarise(val_usd = sum(val_usd, na.rm = T)) %>% 
  group_by(Fuente_cod) %>%
  tidyr::nest()

# preparing data frame for making plots
top6_plot2 <- top6_mod2 %>% 
  mutate(data = map(top6_mod2$data, ~.x %>%
                      mutate(part = val_usd / sum(val_usd, na.rm = T)) %>% 
                      arrange(desc(part)))) %>% 
  mutate(data = map(.$data, ~.x[1:5,])) %>%
  mutate(data = map(.$data, ~.x %>% mutate(dif = if_else(val_usd == max(val_usd), T, F)))) %>%
  mutate(data = map(.$data, ~.x %>% mutate(part_cum = cumsum(part)))) %>% 
  unnest(data) %>% 
  mutate(orden = nrow(.):1) %>% 
  mutate(part_f = scales::percent(part),
         dif2 = if_else(Fuente_cod == "USA", if_else(part_cum <= 0.9, part_f, ""),
                                                     if_else(part_cum <= 0.7, part_f, ""))) 
# making plots
top6_plot2 %>% 
  ggplot(aes(orden, val_usd)) + 
  geom_bar(stat = "identity", aes(fill = dif)) +
  scale_fill_manual(values = c("grey85", "yellowgreen")) +
  geom_text(aes(label = dif2),
            color = "white",
            hjust = 1,
            size = 4,
            fontface = "bold",
            nudge_y = -0.5) +
  labs(x = "", y = "",
       title = "Origen de las importaciones de aguacate en países seleccionados",
       subtitle = "Participación porcentual en 2017",
       caption = "Fuente: UN Comtrade Database") +
  coord_flip() +
  facet_wrap(~Fuente_cod, ncol = 3, scales = "free") +
  scale_x_continuous(breaks = top6_plot2$orden, labels = top6_plot2$Socio_cod, expand = c(0,0)) +
  tema_gg +
  theme(axis.text.x = element_blank(),
        legend.position = "none")

# imports concentration data
top6_mod2 %>% 
  mutate(data = map(top6_mod2$data, ~.x %>%
                      mutate(part = val_usd / sum(val_usd, na.rm = T)) %>% 
                      arrange(desc(part)))) %>% 
  mutate(data = map(.$data, ~.x[1:5,])) %>%
  mutate(part = map_dbl(.$data, ~sum(.x$part)))

# Exports: main players
data_01 <- read.csv("https://comtrade.un.org/api/get?max=50000&type=C&freq=M&px=HS&ps=2010&r=484,604,152,528,724&p=all&rg=1%2C2&cc=080440&uitoken=8458e2b28e295012d38a1c8075ea8e83&fmt=csv")
data_02 <- read.csv("https://comtrade.un.org/api/get?max=50000&type=C&freq=M&px=HS&ps=2011&r=484,604,152,528,724&p=all&rg=1%2C2&cc=080440&uitoken=8458e2b28e295012d38a1c8075ea8e83&fmt=csv")
data_03 <- read.csv("https://comtrade.un.org/api/get?max=50000&type=C&freq=M&px=HS&ps=2012&r=484,604,152,528,724&p=all&rg=1%2C2&cc=080440&uitoken=8458e2b28e295012d38a1c8075ea8e83&fmt=csv")
data_04 <- read.csv("https://comtrade.un.org/api/get?max=50000&type=C&freq=M&px=HS&ps=2013&r=484,604,152,528,724&p=all&rg=1%2C2&cc=080440&uitoken=8458e2b28e295012d38a1c8075ea8e83&fmt=csv")
data_05 <- read.csv("https://comtrade.un.org/api/get?max=50000&type=C&freq=M&px=HS&ps=2014&r=484,604,152,528,724&p=all&rg=1%2C2&cc=080440&uitoken=8458e2b28e295012d38a1c8075ea8e83&fmt=csv")
data_06 <- read.csv("https://comtrade.un.org/api/get?max=50000&type=C&freq=M&px=HS&ps=2015&r=484,604,152,528,724&p=all&rg=1%2C2&cc=080440&uitoken=8458e2b28e295012d38a1c8075ea8e83&fmt=csv")
data_07 <- read.csv("https://comtrade.un.org/api/get?max=50000&type=C&freq=M&px=HS&ps=2016&r=484,604,152,528,724&p=all&rg=1%2C2&cc=080440&uitoken=8458e2b28e295012d38a1c8075ea8e83&fmt=csv")
data_08 <- read.csv("https://comtrade.un.org/api/get?max=50000&type=C&freq=M&px=HS&ps=2017&r=484,604,152,528,724&p=all&rg=1%2C2&cc=080440&uitoken=8458e2b28e295012d38a1c8075ea8e83&fmt=csv")
# there are not complete info from Kenya
#data_08 <- read.csv("https://comtrade.un.org/api/get?max=50000&type=C&freq=M&px=HS&ps=2010&r=404&p=all&rg=1%2C2&cc=080440&uitoken=8458e2b28e295012d38a1c8075ea8e83&fmt=csv")
#data_09 <- read.csv("https://comtrade.un.org/api/get?max=50000&type=C&freq=M&px=HS&ps=2011&r=404&p=all&rg=1%2C2&cc=080440&uitoken=8458e2b28e295012d38a1c8075ea8e83&fmt=csv")
#data_10 <- read.csv("https://comtrade.un.org/api/get?max=50000&type=C&freq=M&px=HS&ps=2012&r=404&p=all&rg=1%2C2&cc=080440&uitoken=8458e2b28e295012d38a1c8075ea8e83&fmt=csv")
#data_11 <- read.csv("https://comtrade.un.org/api/get?max=50000&type=C&freq=M&px=HS&ps=2013&r=404&p=all&rg=1%2C2&cc=080440&uitoken=8458e2b28e295012d38a1c8075ea8e83&fmt=csv")
#data_12 <- read.csv("https://comtrade.un.org/api/get?max=50000&type=C&freq=M&px=HS&ps=2014&r=404&p=all&rg=1%2C2&cc=080440&uitoken=8458e2b28e295012d38a1c8075ea8e83&fmt=csv")
#data_13 <- read.csv("https://comtrade.un.org/api/get?max=50000&type=C&freq=M&px=HS&ps=2015&r=404&p=all&rg=1%2C2&cc=080440&uitoken=8458e2b28e295012d38a1c8075ea8e83&fmt=csv")
#data_14 <- read.csv("https://comtrade.un.org/api/get?max=50000&type=C&freq=M&px=HS&ps=2016&r=404&p=all&rg=1%2C2&cc=080440&uitoken=8458e2b28e295012d38a1c8075ea8e83&fmt=csv")
#data_15 <- read.csv("https://comtrade.un.org/api/get?max=50000&type=C&freq=M&px=HS&ps=2017&r=404&p=all&rg=1%2C2&cc=080440&uitoken=8458e2b28e295012d38a1c8075ea8e83&fmt=csv")


top6x_df <- purrr::map_df(ls(pattern = "data_[0-9]+"), ~rbind(get(.x)))
unique(top6x_df$Period)
names(top6x_df)
head(top6x_df)

top6x_df <- top6x_df %>% select(Year, Period, Trade.Flow, Reporter.Code, Reporter,
                              Partner.Code, Partner, Commodity.Code, Commodity, Netweight..kg.,
                              Trade.Value..US..) %>% 
  magrittr::set_colnames(c("Año", "Periodo", "Flujo", "Fuente_id", "Fuente",
                 "Socio_id", "Socio", "comm_id", "comm", "vol_kg", "val_usd"))

top6x_df <- tidyr::separate(top6x_df, Periodo, into = c(NA, "Mes"), sep = -2)

top6x_df <- top6x_df %>% left_join(country_id, by = c("Fuente" = "País")) %>% 
  mutate(Fuente_cod = codigo) %>% select(-codigo, -Fuente_id) %>% 
  left_join(country_id, by = c("Socio" = "País")) %>% 
  mutate(Socio_cod = codigo) %>% select(-codigo, -Socio_id) %>% 
  mutate(Mes = as.integer(Mes))

unique(top6x_df$Fuente_cod)
str(top6x_df)

top6x_df %>% filter(is.na(Fuente_cod))

# Checking that each country has the same number of rows, otherwise the following time series
# analysis will not be correct
top6x_df01 <- top6x_df %>% 
  filter(Flujo == "Exports", Socio == "World") %>% 
  select(Año, Mes, Fuente_cod, val_usd) %>% 
  mutate(Periodo = as.Date(paste0(Año, "-", Mes, "-1"))) %>%
  select(-Año, -Mes) %>% 
  arrange(Periodo) %>%
  group_by(Fuente_cod) %>%
  tidyr::nest() 

# adding the missing data in Peru table (2011-11); the value was assumed to be cero
# info from Netherlands has the sames status than its imports: registers until 2017-07
top6x_df01$data[[1]] <- top6x_df01$data[[1]] %>% 
                      rbind(data.frame(val_usd = 0, Periodo = "2011-11-01")) %>% 
                      arrange(Periodo)

top6x_df %>% 
  filter(Flujo == "Exports", Socio == "World", Fuente_cod == "PER") %>% 
  select(Año, Mes, Fuente_cod, val_usd)

# trends
# actual monthly series and their trends 
top6x_df01 %>% 
  mutate(data = map(top6x_df01$data, ~.x %>% mutate(val_usd = val_usd/1000000))) %>%
  tidyr::unnest(data) %>% 
  ggplot(aes(Periodo, val_usd)) +
  geom_line(aes(color = "yellowgreen"), size = .5) +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2),
              aes(color = "lightgrey"),
              linetype = 1,
              size = .65,
              se = FALSE) +
  scale_color_manual(labels = c("Tendencia", "Valor"),
                     values = c("yellowgreen", "lightgrey"),
                     name = "") +
  labs(x = "", y = "",
       title = "Valor de las exportaciones mensuales de aguacate\nen países seleccionados, 2008-2017",
       subtitle = "Millones de dólares",
       caption = "Fuente: UN Comtrade Database") +
  facet_wrap(~Fuente_cod, ncol = 3, scales = "free_y") +
  theme(legend.position = "bottom") +
  tema_gg
