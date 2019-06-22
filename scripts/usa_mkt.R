library(tidyverse)
library(extrafont)
# font_import() # use this function first if R was updated
loadfonts(device = "win")

names(wld_trd_avc)

# extra series: annual USA avocado production in tonnes
pdn_avo_usa_edos <- read.csv("~/OneDrive/R/repos/aguacate_am/raw_data/pd_usa_edos.csv",
                             header = T,
                             stringsAsFactors = F)
pdn_avo_usa_edos <- pdn_avo_usa_edos %>% 
  select(Year, State, Data.Item, Value)

str(pdn_avo_usa_edos) # checking type of the variables

pdn_avo_usa_edos <- pdn_avo_usa_edos %>% 
  mutate_at(vars(Value), ~as.numeric(stringr::str_remove_all(., ",")))

pdn_avo_usa <- read.csv("clipboard", header = T, sep = "\t")

# avocado production by state
pdn_avo_usa_edos %>% 
  mutate(item_id = rep(1:4, length.out = nrow(.))) %>% 
  filter(Year == 2017, item_id == 3) %>% 
  #summarise(total = sum(Value)) %>% 
  #mutate(total = scales::comma(total)) %>% 
  mutate(st_id = c("CA", "FL", "HI")) %>% 
  select(st_id, Value) %>% 
  mutate(part = scales::percent(Value / sum(Value)),
         difer = if_else(st_id == "CA", T, F),
         difer2 = if_else(st_id %in% c("CA", "FL"), part, "")) %>% 
  ggplot(aes(x = reorder(st_id, Value), y = Value)) +
    geom_bar(stat = "identity", aes(fill = difer)) +
    scale_fill_manual(values = c("grey85","yellowgreen")) +
    geom_text(aes(label = difer2),
            color = "white",
            hjust = 1,
            size = 7,
            fontface = "bold",
            nudge_y = -0.005) +
    labs(x = "", y = "",
         title = "Estados productores de aguacate en 2017",
         subtitle = "Porcentajes de participación en la producción\nde los Estados Unidos",
         caption = "Fuente: National Agricultural Statistics Service") +
    coord_flip() +
    tema_gg +
    theme(legend.position = "none",
            axis.text.x = element_blank())



# actual series and their trends
wld_trd_avc %>% 
  filter(País == "USA", Flujo == "Import") %>% 
  select(Año, vol_kg) %>% 
  arrange(Año) %>% 
  mutate(mp = vol_kg/1000) %>% 
  select(-vol_kg) %>% 
  cbind(wld_trd_avc %>% 
          filter(País == "USA", Flujo == "Export") %>% 
          select(Año, vol_kg) %>% 
          arrange(Año) %>%
          mutate(xp = vol_kg/1000) %>% 
          select(xp),
        pdn_avo_usa %>% 
          select(pdn_ton) %>% 
          rename(pd = pdn_ton)) %>% # mutate(pd_rel = pd /mp)
  mutate(ca = pd - xp + mp,
         dp = ca + xp,
         cn = mp - xp) %>%
  mutate_at(vars(-Año), ~./1000) %>%  # thousends of tonnes
  mutate(periodo = as.Date(paste0(Año, "-1-1"))) %>%
  mutate(upper = if_else(Año == 2012, ca, as.numeric(NA)),
         lower = if_else(Año == 2012, cn, as.numeric(NA))) %>%
  mutate(ca_t = if_else(Año == 2009, ca, as.numeric(NA)),
         cn_t = if_else(Año == 2016, cn, as.numeric(NA)),
         pd_t = if_else(Año == 2012, (ca + cn) / 2, as.numeric(NA))) %>% 
  ggplot(aes(x = periodo)) +
    geom_line(aes(y = ca), size = 1, col = "yellowgreen") +
    geom_line(aes(y = cn), size = 1, col = "grey85") +
    geom_linerange(aes(ymax = upper, ymin = lower), size = 1, linetype = 2, col = "grey80") +
    geom_text(aes(y = ca_t + 50, label = "ca"), size = 10, family = "Lato") +
    geom_text(aes(y = cn_t - 30, label = "cn"), size = 10, family = "Lato") +
    geom_text(aes(y = pd_t - 30, label = "pd "), size = 10, family = "Lato", hjust = "right") +
    labs(x = "", y = "") +
    tema_gg
  
  mutate_at(vars(-periodo, -Año), ~xts::diff.xts(log(.))) %>% 
  gather("key", "value", mp:cn) %>%  
  ggplot(aes(x = periodo, y = value)) +
    geom_line(aes(col = "yellowgreen"), size = 0.5) +
    geom_smooth(aes(col = "ligthgray"),
              method = "lm",
              formula = y ~ poly(x, 2),
              linetype = 1,
              size = .65,
              se = FALSE) +
    scale_color_manual(labels = c("Tendencia", "Valor"),
                       values = c("yellowgreen", "lightgrey"),
                       name = "") +
    labs(x = "", y = "",
         title = "Evolución de variables seleccionadas en el mercado\nestadounidense del aguacate, 2008-2017",
         subtitle = "Miles de toneladas",
         caption = "Fuente: UN Comtrade Database") +
    facet_wrap(~key, ncol = 3, scales = "free_y") +
    theme(legend.position = "bottom") +
    tema_gg


wld_trd_avc %>% 
  filter(País == "USA", Flujo == "Import") %>% 
  select(Año, vol_kg) %>% 
  arrange(Año) %>% 
  mutate(mp = vol_kg/1000) %>% 
  select(-vol_kg) %>% 
  cbind(wld_trd_avc %>% 
          filter(País == "USA", Flujo == "Export") %>% 
          select(Año, vol_kg) %>% 
          arrange(Año) %>%
          mutate(xp = vol_kg/1000) %>% 
          select(xp),
        pdn_avo_usa %>% 
          select(pdn_ton)) %>%
          rename(pd = pdn_ton) %>% 
  mutate(ca = pd - xp + mp,
         dp = pd + mp,
         cn = mp - xp) %>%
  mutate(ca2 = ca) %>% 
  mutate(periodo = as.Date(paste0(Año, "-1-1"))) %>% 
  mutate_at(vars(-periodo, -Año, -ca2), ~xts::diff.xts(log(.))) %>% #summarise(tc_pd = mean(ca, na.rm = T))
  mutate(mean_ca =  exp(lm(log(ca2) ~ Año, data = .)$coef[2])-1,
         mean_ca1aux = if_else(Año <= 2012, ca2, as.numeric(NA)),
         mean_ca1 = if_else(!is.na(mean_ca1aux), exp(lm(log(mean_ca1aux) ~ Año, data = .)$coef[2])-1, as.numeric(NA)),
         mean_ca2aux = if_else(Año >= 2013, ca2, as.numeric(NA)),
         mean_ca2 = if_else(!is.na(mean_ca2aux), exp(lm(log(mean_ca2aux) ~ Año, data = .)$coef[2])-1, as.numeric(NA)),
         ca1_txt = if_else(Año == 2009, mean_ca1, as.numeric(NA)),
         ca2_txt = if_else(Año == 2016, mean_ca2, as.numeric(NA))) %>% 
  ggplot(aes(x = periodo)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_line(aes(y = ca), col = "yellowgreen", size = 1) +
  #geom_line(aes(y = mean_ca), col = "grey90", linetype = 2, size = .5) +
  geom_line(aes(y = mean_ca1), col = "grey80", linetype = 2, size = 1) +
  geom_line(aes(y = mean_ca2), col = "grey80", linetype = 2, size = 1) +
  geom_text(aes(y = ca1_txt+.05, label = scales::percent(ca1_txt)), hjust = "right", family = "Lato") +
  geom_text(aes(y = ca2_txt+.05, label = scales::percent(ca2_txt)), family = "Lato") +
  labs(x = "", y = "",
       title = "Variación porcentual anual del consumo aparente\nde aguacate en los Estados Unidos, 2009-2017",
       caption = "Fuente: UN Comtrade y NASS") +
  tema_gg
  geom_line(aes(y = cn), col = "yellowgreen")
  
  gather("key", "value", mp:cn) %>%
  group_by(key) %>% 
  nest() %>% 
  mutate(TCAP = map_dbl(.$data, ~exp(lm(log(value) ~ Año, .x)$coef[2])-1)) %>% 
  mutate(val_17 = map_dbl(.$data, ~.x %>% filter(Año == 2017) %>% select(value) %>% pull())) %>% 
  mutate(wt = val_17 / max(val_17), TCAPr = TCAP * wt) %>%
  rename(Variable = key) %>% 
  select(Variable, TCAP, TCAPr) %>% 
  mutate_at(vars(-Variable), ~scales::percent(.))
  
