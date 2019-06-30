# mexican market dynamics
# production
agt_nal %>% 
  filter(anio %in% c(2008:2017)) %>% 
  select(anio, idestado, idvariedad, volumenproduccion) %>% 
  group_by(anio) %>% 
  summarise(vol_ton = sum(volumenproduccion))

# imports
wld_trd_avc %>% 
  filter(Flujo == "Import", País == "MEX") %>% 
  select(Año, vol_kg)

# exports
wld_trd_avc %>% 
  filter(Flujo == "Export", País == "MEX") %>% 
  select(Año, vol_kg)

# fusioning exports, imports and production in tonnes
wld_trd_avc %>% 
  filter(Flujo == "Export", País == "MEX") %>% 
  select(Año, vol_kg) %>% 
  arrange(Año) %>%
  rename(x_kg = vol_kg) %>% 
  left_join(wld_trd_avc %>% 
              filter(Flujo == "Import", País == "MEX") %>% 
              select(Año, vol_kg)) %>% 
  rename(m_kg = vol_kg) %>% 
  mutate(m_kg = if_else(is.na(m_kg), 0, as.double(m_kg))) %>% 
  mutate_at(vars(-Año), ~./1000) %>% 
  rename(xp = x_kg, mp = m_kg) %>% 
  cbind(agt_nal %>% 
          filter(anio %in% c(2008:2017)) %>% 
          select(anio, idestado, idvariedad, volumenproduccion) %>% 
          group_by(anio) %>% 
          summarise(pd = sum(volumenproduccion)) %>% 
          select(pd)) %>% 
  mutate(ca = pd + mp - xp,
         dp = pd + mp,
         xn = xp - mp) %>%
  mutate(mp = if_else(mp == 0, 1, mp)) %>%
  gather(key = "key", value = "value", -Año) %>% 
  group_by(key) %>% 
  nest() %>% 
  mutate(TCAP = map_dbl(.$data, ~exp(lm(log(value) ~ Año, data = .x)$coef[2])-1),
         max_aux = map_dbl(.$data, ~max(.x)),
         TCAPr = TCAP * max_aux / max(max_aux))

# prices
agt_nal %>% 
  filter(anio %in% c(2008:2017)) %>% 
  select(anio, volumenproduccion, preciomediorural) %>% 
  group_by(anio) %>% 
  summarise(pr = sum(preciomediorural * volumenproduccion) / sum(volumenproduccion)) %>% 
  mutate_at(vars(pr), ~./1000)

inpp_df <- readxl::read_xls("~/OneDrive/R/repos/aguacate_am/raw_data/inpp_mx_07my19.xls",
                            range = "F5:G154", col_names = T)

# prices in real terms
inpp_df <- inpp_df %>% 
  separate(Periodo, sep = "/", into = c("anio", "mes"))

inpp_df %>% 
  group_by(anio) %>% 
  summarise(i_mean = mean(Valor)) %>% 
  mutate(factor = i_mean / inpp_df %>%
                            group_by(anio) %>% 
                            summarise(i_mean = mean(Valor)) %>% 
                            filter(anio == "2017") %>% 
                            select(i_mean) %>% 
                            pull()) %>% 
  filter(!(anio %in% c("2007","2018", "2019"))) %>% 
  cbind(agt_nal %>% 
          filter(anio %in% c(2008:2017)) %>% 
          select(anio, volumenproduccion, preciomediorural) %>% 
          group_by(anio) %>% 
          summarise(pr = sum(preciomediorural * volumenproduccion) / sum(volumenproduccion)) %>% 
          mutate_at(vars(pr), ~./1000) %>% 
          select(pr)) %>% 
  mutate(pr_r = pr / factor) %>% 
  select(pr, pr_r) %>% 
  summarise_all(~exp(lm(log(.) ~ c(1:10))$coef[2])-1)
  
# export prices
pr_xp <- readxl::read_excel("~/OneDrive/R/repos/aguacate_am/raw_data/annual_commerce.xlsx",
                            range = "B11:D43", col_names = T)

pr_xp %>% 
  spread(key = concepto, value = valor) %>% 
  mutate(pr_xp = Valor / Volumen)

# exchange rate
tc_fix <- readxl::read_excel("~/OneDrive/R/repos/aguacate_am/raw_data/tc_fix_08jun19.xlsx",
                             range = "A18:B156", col_names = T)

# export prices in mexican pesos per kg
tc_fix %>% 
  mutate(anio = lubridate::year(Fecha)) %>% 
  group_by(anio) %>% 
  summarise(tc = mean(tc)) %>% 
  filter(!(anio %in% c(2018, 2019))) %>% 
  cbind(pr_xp %>% 
          spread(key = concepto, value = valor) %>% 
          mutate(pr_xp = Valor / Volumen) %>% 
          filter(anio %in% c(2008:2017)) %>% 
          select(pr_xp)) %>% 
  mutate(pr_xp_mxp = tc * pr_xp,
         pr_xp_scom = pr_xp_mxp / 1.3) %>% 
  cbind(agt_nal %>% 
          filter(anio %in% c(2008:2017), idestado == 14) %>% 
          select(anio, volumenproduccion, preciomediorural) %>% 
          group_by(anio) %>% 
          summarise(pr = sum(preciomediorural * volumenproduccion) / sum(volumenproduccion)) %>% 
          mutate_at(vars(pr), ~./1000) %>% 
          select(pr)) %>% 
  summarise_at(vars(-anio), ~exp(lm(log(.) ~ c(1:10))$coef[2])-1)

# generating a list which serves as data base from other projects
mex_mkt <- list(agt_mun, agt_nal, cul17, cult_gn, cultivos17, df_gn, gn,
                id_agt, id_cult, id_edos, id_gpo, id_mdo, id_mod, id_tipo,
                id_var, inpp_df, tc_fix, wld_trd_avc, pr_xp)
names(mex_mkt) <- c("agt_mun", "agt_nal", "cul17", "cult_gn", "cultivos17", "df_gn", "gn",
                  "id_agt", "id_cult", "id_edos", "id_gpo", "id_mdo", "id_mod", "id_tipo",
                  "id_var", "inpp_df", "tc_fix", "wld_trd_avc", "pr_xp")
names(mex_mkt)
save(mex_mkt, file = "~/OneDrive/R/objetos/mex_mkt.RData")
rm(mex_mkt)
