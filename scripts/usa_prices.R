# prices analisys
# wholesale

# making main table
report1 <- read.csv("~/OneDrive/R/repos/aguacate_am/raw_data/report1.csv", header = T, stringsAsFactors = F)
report2 <- read.csv("~/OneDrive/R/repos/aguacate_am/raw_data/report2.csv", header = T, stringsAsFactors = F)
report3 <- read.csv("~/OneDrive/R/repos/aguacate_am/raw_data/report3.csv", header = T, stringsAsFactors = F)
report4 <- read.csv("~/OneDrive/R/repos/aguacate_am/raw_data/report4.csv", header = T, stringsAsFactors = F)
report5 <- read.csv("~/OneDrive/R/repos/aguacate_am/raw_data/report5.csv", header = T, stringsAsFactors = F)
report6 <- read.csv("~/OneDrive/R/repos/aguacate_am/raw_data/report6.csv", header = T, stringsAsFactors = F)

head(report1)
str(report1)
avo_pr_df <- purrr::map_df(ls(pattern = "report[0-9]"), ~rbind(get(.x)))
head(avo_pr_df)
str(avo_pr_df)

# cleaning the house
rm(list = ls(pattern = "report[0-9]"))

# exploring data frame
for(i in (1:ncol(avo_pr_df))) print(unique(avo_pr_df[,i]))
avo_pr_df$City.Name

lista <- list()
for(i in (1:ncol(avo_pr_df))) {
  lista[i][[1]] <- summary(avo_pr_df[,i])
  names(lista)[i] <- colnames(avo_pr_df)[i]
  print(lista)
}

# TO DO LIST 
# City.Name: create a column to flag usa terminals
# Package: homologate units
# Origin: verify the empty origing
# Origin.District: verify the empty origing

# deleting empty variables
avo_pr_df <- avo_pr_df %>% 
  select(-Commodity.Name, -Sub.Variety, -Grade, -Color, -Environment,
         -Quality, -Condition, -Appearance, -Storage, -Crop, -Repack)

# tagging american cities
usa_dest <- avo_pr_df %>% 
  select(City.Name) %>% 
  distinct() %>% 
  mutate(origen = "NA") %>%
  edit()
usa_dest <- usa_dest %>% 
  filter(origen == "USA") %>% 
  select(City.Name)

usa_orig <- avo_pr_df %>% 
  select(Origin) %>% 
  distinct() %>% 
  mutate(origen = "NA") %>% 
  edit()
usa_orig <- usa_orig %>% 
  filter(origen == "USA") %>% 
  select(Origin)

# checking registers with empty origin
avo_pr_df %>% 
  select(Origin, Origin.District) %>% 
  filter(Origin == "")
avo_pr_df <- avo_pr_df %>% 
  filter(Origin != "")

avo_pr_df %>% 
  select(Origin, Origin.District) %>% 
  filter(Origin.District == "")

# checking usefulness of Origin.District variable
avo_pr_df %>% 
  count(Origin.District) %>% 
  mutate(part = n / sum(n))
avo_pr_df %>% 
  count(Origin)
# it is just 4% of data related

# removing Origin.District
avo_pr_df <- avo_pr_df %>% select(-Origin.District)

# checking usefulness of Trans.Mode variable
avo_pr_df %>% 
  count(Trans.Mode) %>% 
  mutate(part = n / sum(n))
# removing Trans.Mode
avo_pr_df <- avo_pr_df %>% select(-Trans.Mode)

# preparing Package for homologation
avo_pr_df <- avo_pr_df %>% 
  filter(Package != "")

avo_pr_df %>% 
  count(Package)

pk_filtro <- avo_pr_df %>% 
  select(Package) %>% 
  distinct() %>% 
  edit()

pk_filtro <- pk_filtro %>% 
  mutate(pk_kg = peso_inicial * to_kg)

names(avo_pr_df)
head(avo_pr_df)

glimpse(avo_pr_df)

avo_pr_df %>% 
  select(Unit.of.Sale) %>% 
  distinct()
avo_pr_df %>% 
  count(Unit.of.Sale)
avo_pr_df %>% 
  select(Unit.of.Sale, Package) %>% 
  filter(Unit.of.Sale == "PER KG", Package == "cartons") %>% 
  count()
avo_pr_df %>% 
  count(Package)

avo_pr_df 
  sum(is.na(avo_pr_df$Mostly.Low))

# last tuned
avo_pr_df <- avo_pr_df %>% 
  mutate(usd_um = (Mostly.Low + Mostly.High) / 2) %>%
  select(-Low.Price, -High.Price, -Mostly.Low, -Mostly.High) %>%
  left_join(select(pk_filtro, Package, pk_kg)) %>% 
  mutate(usd_kg = usd_um/pk_kg) %>% 
  mutate(Unit.of.Sale = if_else(is.na(Unit.of.Sale), "NA", if_else(Unit.of.Sale == "", "NA", Unit.of.Sale))) %>% 
  mutate(usd_kg = ifelse(Unit.of.Sale != "PER KG", usd_kg, usd_um)) %>%
  select(-usd_um, -pk_kg) %>%
  mutate(origen = if_else(Origin %in% usa_orig$Origin, "USA", Origin)) %>% 
  mutate(fecha = lubridate::mdy(Date),
         anio = lubridate::year(fecha),
         mes = lubridate::month(fecha),
         semana = lubridate::week(fecha)) %>% 
  select(-Date) %>% 
  mutate(destino = if_else(City.Name %in% usa_dest$City.Name, "USA", City.Name))

# outliers search
avo_pr_df %>% 
  ggplot(aes(usd_kg)) + 
  geom_histogram()

avo_pr_df %>% 
  filter(usd_kg > 10)
avo_pr_df <- avo_pr_df %>% 
  filter(usd_kg != max(usd_kg))

avo_pr_df %>% 
  filter(usd_kg < 0.6)

# last view to data "Excellent tool just found"
summarytools::view(summarytools::dfSummary(avo_pr_df))


# let's the analysis begin
avo_pr_df %>% 
  filter(destino == "USA", !(anio %in% c(2018, 2019))) %>% 
  select(usd_kg) %>%
  #summarytools::dfSummary(.)  
  ggplot(aes(usd_kg)) +
  geom_density(col = "yellowgreen", fill = "grey85", alpha = 0.25) +
  geom_vline(xintercept = median(avo_pr_df %>%
                                   filter(destino == "USA", !(anio %in% c(2018, 2019))) %>%
                                   select(usd_kg) %>% 
                                   pull()), linetype = 2, col = "yellowgreen") +
  labs(x = "", y = "") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        text = element_text(family = "Lato"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "bold"),
        #axis.ticks.y = element_blank(),
        axis.text = element_text(family = "Lato Light"),
        axis.line = element_line(color = "lightgrey"))


plt01 <- avo_pr_df %>% 
  filter(destino == "USA", !(anio %in% c(2018, 2019))) %>% 
  group_by(City.Name) %>% 
  summarise(avg = median(usd_kg),
            fq = n()) %>%
  mutate(ranking = rank(-avg, ties.method = "random")) %>%
  mutate(aux = if_else(ranking > 5, "OTROS", City.Name)) %>%
  group_by(aux) %>% 
  summarise(avg = mean(avg),
            fq = sum(fq)) %>% 
  mutate(part = scales::percent(fq/sum(fq))) %>% #arrange(desc(fq))
  arrange(desc(avg)) %>%
  mutate(col_aux = if_else(avg == max(avg), T, F)) %>% 
  ggplot(aes(reorder(aux, avg), avg)) +
  geom_bar(stat = "identity", aes(fill = col_aux)) +
  geom_text(aes(label = round(avg, 2)),
            col = "white",
            hjust = 1,
            size = 5,
            fontface = "bold",
            nudge_y = -.1) +
  scale_fill_manual(values = c("lightgrey", "yellowgreen")) +
  scale_x_discrete(aes(values = reorder(aux, avg)),
                   labels = c("Otros", "ATL", "DAL", "BAL", "COL", "SFR")) +
  labs(x = "", y = "",
       title = "Precio según mercado destino",
       subtitle = "usd/kg") +
  coord_flip() +
  tema_gg +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_blank())

plt02 <- avo_pr_df %>% 
  filter(destino == "USA", !(anio %in% c(2018, 2019))) %>% 
  group_by(Type) %>% 
  summarise(avg = median(usd_kg),
            fq = n()) %>%
  mutate(ranking = rank(-avg)) %>%
  mutate(aux = if_else(ranking > 5, "OTROS", Type)) %>%
  group_by(aux) %>% 
  summarise(avg = mean(avg),
            fq = sum(fq)) %>% 
  mutate(part = scales::percent(fq/sum(fq))) %>% #arrange(desc(fq))
  arrange(desc(avg)) %>%
  mutate(col_aux = if_else(avg == max(avg), T, F)) %>% 
  ggplot(aes(reorder(aux, avg), avg)) +
  geom_bar(stat = "identity", aes(fill = col_aux)) +
  geom_text(aes(label = round(avg, 2)),
            col = "white",
            hjust = 1,
            size = 5,
            fontface = "bold",
            nudge_y = -.1) +
  scale_fill_manual(values = c("lightgrey", "yellowgreen")) +
  scale_x_discrete(aes(values = reorder(aux, avg)),
                   labels = c("No orgánico", "Orgánico")) +
  labs(x = "", y = "",
       title = "Precio según tipo",
       subtitle = "usd/kg") +
  coord_flip() +
  tema_gg +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_blank())

plt03 <- avo_pr_df %>% 
  filter(destino == "USA", !(anio %in% c(2018, 2019))) %>% 
  group_by(Variety) %>% 
  summarise(avg = median(usd_kg),
            fq = n()) %>%
  filter(Variety != "") %>% 
  mutate(ranking = rank(-avg)) %>%
  mutate(aux = if_else(ranking > 5, "Otros", Variety)) %>%
  group_by(aux) %>% 
  summarise(avg = mean(avg),
            fq = sum(fq)) %>% 
  mutate(part = scales::percent(fq/sum(fq))) %>% #arrange(desc(fq))
  arrange(desc(avg)) %>%
  mutate(col_aux = if_else(avg == max(avg), T, F)) %>%
  ggplot(aes(reorder(aux, avg), avg)) +
  geom_bar(stat = "identity", aes(fill = col_aux)) +
  geom_text(aes(label = round(avg, 2)),
            col = "white",
            hjust = 1,
            size = 5,
            fontface = "bold",
            nudge_y = -.1) +
  scale_fill_manual(values = c("lightgrey", "yellowgreen")) +
  scale_x_discrete(aes(values = reorder(aux, avg)),
                   labels = c("Otros", "Pinkerton", "Reed", "Hass", "Fuerte", "Lamb Hass")) +
  labs(x = "", y = "",
       title = "Precio según variedad",
       subtitle = "usd/kg") +
  coord_flip() +
  tema_gg +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_blank())

plt04 <- avo_pr_df %>% 
  filter(destino == "USA", !(anio %in% c(2018, 2019))) %>% 
  group_by(Item.Size) %>% 
  summarise(avg = median(usd_kg),
            fq = n()) %>%
  filter(avg <= 4.5) %>% 
  mutate(ranking = rank(-avg)) %>%
  mutate(aux = if_else(ranking > 5, "Otros", Item.Size)) %>%
  group_by(aux) %>% 
  summarise(avg = mean(avg),
            fq = sum(fq)) %>% 
  mutate(part = scales::percent(fq/sum(fq))) %>% #arrange(desc(fq))
  arrange(desc(avg)) %>%
  mutate(col_aux = if_else(avg == max(avg), T, F)) %>%
  ggplot(aes(reorder(aux, avg), avg)) +
  geom_bar(stat = "identity", aes(fill = col_aux)) +
  geom_text(aes(label = round(avg, 2)),
            col = "white",
            hjust = 1,
            size = 5,
            fontface = "bold",
            nudge_y = -.1) +
  scale_fill_manual(values = c("lightgrey", "yellowgreen")) +
  #scale_x_discrete(aes(values = reorder(aux, avg)),
   #                labels = c("Otros", "Fuerte", "Hass", "Reed", "Lamb Hass")) +
  labs(x = "", y = "",
       title = "Precio según tamaño",
       subtitle = "usd/kg") +
  coord_flip() +
  tema_gg +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_blank())

gridExtra::grid.arrange(plt01, plt02, plt03, plt04, ncol = 2)


# price according to the origin
avo_pr_df %>% 
  filter(destino == "USA", !(anio %in% c(2018, 2019))) %>% 
  group_by(origen) %>% 
  summarise(avg = median(usd_kg),
            fq = n()) %>%
  filter(avg <= 3.5) %>% 
  mutate(ranking = rank(-avg)) %>%
  mutate(aux = if_else(ranking > 5, "Otros", origen)) %>%
  group_by(aux) %>% 
  summarise(avg = mean(avg),
            fq = sum(fq)) %>% 
  mutate(part = scales::percent(fq/sum(fq))) %>% #arrange(desc(fq))
  arrange(desc(avg)) %>%
  mutate(col_aux = if_else(avg == max(avg), T, F)) %>%
  ggplot(aes(reorder(aux, avg), avg)) +
  geom_bar(stat = "identity", aes(fill = col_aux)) +
  geom_text(aes(label = round(avg, 2)),
            col = "white",
            hjust = 1,
            size = 7,
            fontface = "bold",
            nudge_y = -.1) +
  scale_fill_manual(values = c("lightgrey", "yellowgreen")) +
  scale_x_discrete(aes(values = reorder(aux, avg)),
                  labels = c("Otros", "DOM", "USA", "CHL", "PER", "MEX")) +
  labs(x = "", y = "",
       title = "Precio promedio según su origen",
       subtitle = "usd/kg") +
  coord_flip() +
  tema_gg +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_blank())

avo_pr_df %>% 
  filter(destino == "USA", origen %in% c("MEXICO", "USA")) %>% 
  ggplot(aes(usd_kg)) +
  #geom_histogram() +
  geom_density(aes(fill = origen), alpha = 0.25)

# time factor
avo_pr_df %>% 
  filter(destino == "USA") %>% #, !(anio %in% c(2018, 2019))) %>% 
  group_by(anio) %>% 
  summarise(central = median(usd_kg)) %>% 
  ggplot(aes(anio, central)) +
  geom_line()
  
avo_pr_df %>% 
  filter(destino == "USA") %>% #, !(anio %in% c(2018, 2019))) %>% 
  mutate(anio_mes = as.Date(paste0(anio, "-", mes, "-1"))) %>% 
  group_by(anio_mes) %>% 
  summarise(central = median(usd_kg)) %>%
  mutate(central_bgn = if_else(anio_mes >= as.Date("2018-01-01"), as.numeric(NA), central),
         central_end = if_else(anio_mes >= as.Date("2018-01-01"), central, as.numeric(NA)),
         central_1 = avo_pr_df %>% 
                        filter(destino == "USA", anio < 2018) %>% 
                        summarise(central = median(usd_kg)) %>% 
                        pull(), as.numeric(NA)) %>%
  ggplot(aes(anio_mes, central)) +
  geom_line(aes(y = central_bgn), col = "grey85", size = 1) +
  geom_line(aes(y = central_end), col = "darkgrey", size = 1) +
  geom_smooth(col = "yellowgreen", se = F, size = 1) +
  geom_line(aes(y = central_1), col = "yellowgreen", size = .5, linetype = 2) +
  labs(x = "", y = "",
       title = "Evolución del precio del aguacate al mayoreo\nen el mercado norteamericano, 2008-may2019",
       subtitle = "usd/kg",
       caption = "Fuente: AMS") +
  tema_gg

meses <- data.frame(mes = 1:12,let = c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic"))

avo_pr_df %>% 
  filter(destino == "USA") %>%
  left_join(meses) %>% 
  mutate(aux = if_else(mes %in% c(5,6), T, F)) %>% 
  ggplot(aes(group = let, x = reorder(let, mes), y = usd_kg)) +
  geom_boxplot(outlier.alpha = 0.25,
               aes(fill = aux, color = aux),
               alpha = .375) +
  scale_fill_manual(values = c("lightgrey", "yellowgreen")) +
  scale_color_manual(values = c("lightgrey", "yellowgreen")) +
  labs(x = "", y = "",
       title = "Distribución mensual de los precios al mayoreo del aguacate\nen los Estados Unidos, 2008-may2019",
       subtitle = "usd/kg",
       caption = "Fuente: AMS") +
  tema_gg +
  theme(legend.position = "none")

avo_pr_df %>%
  filter(destino == "USA") %>% 
  group_by(mes) %>% 
  summarise(central = median(usd_kg))

avo_pr_df %>% 
  filter(destino == "USA", mes %in% c(5,6)) %>%
  mutate(aux = if_else(semana %in% c(21,22,23), T, F)) %>% 
  ggplot(aes(group = semana, x = as.factor(semana), y = usd_kg)) +
  geom_boxplot(outlier.alpha = 0.25,
               aes(fill = aux, color = aux),
               alpha = .375) +
  scale_fill_manual(values = c("lightgrey", "yellowgreen")) +
  scale_color_manual(values = c("lightgrey", "yellowgreen")) +
  labs(x = "", y = "",
       title = "Distribución semanal de los precios al mayoreo del aguacate en los Estados Unidos\ndurante los meses de mayo y junio, 2008-may2019",
       subtitle = "usd/kg",
       caption = "Fuente: AMS") +
  tema_gg +
  theme(legend.position = "none")

avo_pr_df %>%
  filter(destino == "USA", mes %in% c(5,6)) %>% 
  group_by(semana) %>% 
  summarise(central = median(usd_kg))

avo_pr_df %>% 
  filter(destino == "USA", anio == 2018) %>% #, origen %in% c("MEXICO", "USA")) %>%
  mutate(anio_mes = as.Date(paste0(anio, "-", mes, "-1"))) %>% 
  group_by(mes) %>% 
  summarise(avg = median(usd_kg)) %>% 
  ggplot(aes(mes, avg)) +
  geom_line()

windows <- read.table("clipboard", header = T, sep = "\t", stringsAsFactors = F)
str(windows)

windows_df <- windows %>% 
  rbind(
top6_df %>% 
  filter(Flujo == "Imports", Fuente_cod == "USA", Socio_cod == "MEX") %>%
  group_by(Mes) %>% 
  summarise(vol_kg = mean(vol_kg, na.rm = T)) %>% 
  select(Mes, vol_kg) %>% 
  arrange(Mes) %>% 
  mutate(variable = "mp_mx",
         part = vol_kg/sum(vol_kg)) %>%
  rename(mes = Mes) %>% 
  select(-vol_kg))

avo_pr_df %>% 
  filter(destino == "USA", origen %in% c("MEXICO", "USA")) %>% 
  group_by(origen, mes) %>% 
  summarise(central = median(usd_kg)) %>% 
  ungroup() %>% 
  group_by(origen) %>% 
  nest() %>% 
  mutate(data = map(.$data, ~.x %>% mutate(ranking = percent_rank(central)))) %>% 
  unnest(data) %>% 
  ggplot(aes(x = mes, y = ranking, col = origen)) +
  geom_line()

windows_df %>% 
  group_by(variable) %>% 
  nest() %>% 
  mutate(data = map(.$data, ~.x %>% mutate(ranking = percent_rank(part)))) %>% 
  unnest(data) %>% 
  filter(variable != "pd_mx") %>% 
  ggplot(aes(group = variable, col = variable, x = as.factor(mes), y = ranking)) +
  geom_line()

usa_mkt_18 <- read.table("clipboard", header = T, sep = "\t", stringsAsFactors = F)
usa_mkt_18_df <- usa_mkt_18 %>%
  mutate_if(is.character, ~as.numeric(stringr::str_remove_all(., ","))) %>% 
  gather("key", "value", -mes)
str(usa_mkt_18_df)

usa_mkt_18_df %>%
  filter(!(key %in% c("x_all_part", "usa"))) %>% 
  ggplot(aes(group = key, x = as.factor(mes), y = value, col = key)) +
  geom_line()
# 2018 avocado production in tons: 185770 (source: NASS)

mp_usa_1518 <- read.table("~/OneDrive/R/repos/aguacate_am/raw_data/mp_monthly_usa_1518.csv", sep = ",", header = T, stringsAsFactors = F)
mp_usa_1518_df <- mp_usa_1518 %>% 
  select(-HS.Code, -Product, -UOM, -Partner.Code) %>% 
  mutate(Year = str_extract(Year, "[0-9]{4}"),
         soc_grupos = if_else(Partner == "Mexico", Partner, "Otros")) %>% 
  gather(key = "let", value = "ton", ene:dic) %>% 
  group_by(soc_grupos, Year, let) %>% 
  summarise(ton = sum(ton))

mdist_usa_1518 <- read.table("clipboard", header = T, sep = "\t", stringsAsFactors = F)

pdn_usa_1518 <- pdn_avo_usa %>% 
  filter(año %in% c(2015:2017)) %>% 
  add_row(año = 2018, pdn_ton = 185770)

disp_usa_1518 <- pdn_usa_1518 %>% 
  expand(año = 2015:2018, mes = 1:12) %>% 
  left_join(pdn_usa_1518) %>% 
  left_join(mdist_usa_1518 %>% 
              mutate(mes = 1:12)) %>% 
  mutate(ton = pdn_ton * dist,
         soc_grupos = "USA") %>% 
  select(soc_grupos, año, let, ton) %>% 
  rename(Year = año) %>%
  mutate(Year = as.character(Year)) %>% 
  full_join(mp_usa_1518_df)

disp_usa_1518 %>% 
  group_by(Year, let) %>% 
  nest() %>% 
  mutate(data = map(.$data, ~.x %>% mutate(ton_tot = sum(ton)))) %>% 
  unnest(data) %>% 
  mutate(wt = ton / ton_tot) %>% 
  left_join(meses) %>% 
  arrange(soc_grupos, Year, mes) %>%
  cbind(avo_pr_df %>% 
          filter(anio %in% c(2015:2018)) %>% 
          mutate(origen = if_else(!(origen %in% c("MEXICO", "USA")), "Otros", origen)) %>% 
          group_by(origen, anio, mes) %>% 
          summarise(central = median(usd_kg)) %>%
          rename(Mes = mes) %>% 
          as.data.frame()) %>% #filter(Year == 2018, mes == 4) %>% summarise(sum(wt))
  group_by(Year, mes) %>%
  summarise(usd_kg = sum(wt * central)) %>%
  ungroup() %>%
  mutate(an_me = as.Date(paste0(Year, "-", mes, "-1"))) %>%
  mutate(med = median(usd_kg),
         usd_kg_bg = if_else(an_me < as.Date("2018-01-01"), usd_kg, as.numeric(NA)),
         usd_kg_ed = if_else(an_me >= as.Date("2018-01-01"), usd_kg, as.numeric(NA))) %>% 
  ggplot(aes(x = an_me, y = usd_kg)) + 
  geom_line(aes(y = usd_kg_bg), col = "grey85", size = 1) +
  geom_line(aes(y = usd_kg_ed), col = "darkgrey", size = 1) +
  geom_line(aes(y = med), col = "yellowgreen", size = 1, linetype = 2) +
  labs(x = "", y = "",
       title = "Evolución del precio del aguacate al mayoreo en el mercado\nnorteamericano considerando volúmenes comerciados, 2015-2018",
       subtitle = "usd/kg",
       caption = "Fuente: AMS, FAS, NASS y Californian Avocado Comission") +
  tema_gg

meses

pr_uwt <- avo_pr_df %>% 
  filter(destino == "USA", anio %in% c(2015:2018)) %>% 
  group_by(anio, mes) %>% 
  summarise(usd_kg = median(usd_kg))

cor.test(pr_wt$usd_kg, pr_uwt$usd_kg)

avo_pr_df %>% 
  filter(destino == "USA", origen %in% c("USA", "MEXICO")) %>%
  mutate(origen = if_else(origen == "MEXICO", "MEX", origen)) %>% 
  group_by(origen, semana) %>% 
  summarise(usd_kg = median(usd_kg)) %>%
  mutate(aux_t = if_else(semana == 3, origen, as.character(NA))) %>% 
  ggplot(aes(x = semana, y = usd_kg)) +
  geom_line(aes(color = origen), size = 1) +
  geom_text(aes(label = aux_t), vjust = "bottom", family = "Lato",
            fontface = "bold", nudge_y = .08, hjust = "left", size = 5) +
  scale_color_manual(values = c("yellowgreen", "grey85")) +
  labs(x = "", y = "") +
  scale_x_continuous(breaks = seq(0, 51, 3)) +
  tema_gg +
  theme(legend.position = "none",
        legend.title = element_blank())

pr_rt_usa <- read.table("clipboard", header = T, sep = "\t", stringsAsFactors = F)

# extracting average size
avo_pr_df %>% 
  filter(destino == "USA", Item.Size != "") %>% 
  group_by(Item.Size) %>% 
  summarise(n = n()) %>%
  arrange(desc(n)) %>% 
  mutate(size = str_extract(Item.Size, "[0-9]{2}") %>% as.numeric()) %>% 
  summarise(size_avg = sum(n * size, na.rm = T) / sum(n))

head(pr_rt_usa)
pr_rt_usa$Unit %>% unique()

# average retail price
pr_rt_usa %>% 
  mutate(conv = if_else(Unit == "each", .269, 0.453592),
         usd_kg = Weighted.Avg.Price / conv) %>%
  group_by(Organic) %>% 
  summarise(usd_kg_rt = mean(usd_kg)) %>% 
  cbind(avo_pr_df %>% 
          filter(destino == "USA", anio == 2018) %>% 
          group_by(Type) %>% 
          summarise(n = n()) %>% 
          mutate(part = n / sum(n)) %>% 
          select(part) %>% 
          as.data.frame()) %>% 
  summarise(usd_kg_rt = sum(usd_kg_rt * part))

# median price wholesale
avo_pr_df %>% 
  filter(destino == "USA", anio == 2018) %>% 
  summarise(usd_kg = median(usd_kg))


str(pr_rt_usa)
