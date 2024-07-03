# SETUP ------------------------------------------------------------------------

# cleaning environment
rm(list=ls())
options(scipen = 999)

# loading packages
if(!require("pacman")) install.packages("pacman")

p_load(tidyverse,
       rio,
       janitor,
       intsvy,
       GDAtools,
       Hmisc,
       paletteer,
       gt)


# LOADING DATA -----------------------------------------------------------------

# loading data
pisa <- import("Data/pisa_oecd.csv")


# selecting brazilian data
pisa <- pisa %>% 
  filter(CNT == "BRA")


# EXAMINING PERFORMANCE VS ESCS ------------------------------------------------

# calculating final weights ESCS deciles
fw.decile <- wtd.quantile(pisa$ESCS,
                          probs = seq(0,1,.1),
                          weights = pisa$W_FSTUWT)

# locating replicated weights
names(pisa) # 40 to 119

# calculating replicated weights ESCS deciles
for(w in 1:80) {
  if(w == 1) {
    rw.decile <- NULL
  }
  rw.decile <- rbind(rw.decile, wtd.quantile(pisa$ESCS,
                                             probs = seq(0,1,.1),
                                             weights = pisa[39+w]))
}

# calculating ESCS quantiles standard error following PISA Data Analysis Manual
for(j in 1:ncol(rw.decile)) {
  if(j == 1) {
    decile.se <- NULL
  }
  decile.se <- rbind(decile.se,
                     sqrt(.05*(sum((rw.decile[,j]-fw.decile[j])^2))))
}

# framing together
escs.decile <- data.frame(Value = fw.decile[2:10],
                          StandardError = round(decile.se[2:10],4),
                          t.value = (fw.decile/decile.se)[2:10])


# cutting ESCS into classes
pisa <- pisa %>% 
  mutate(ESCS.decile = cut(ESCS,
                           breaks = fw.decile,
                           include.lowest = T,
                           labels = F))

# checking the cut
wtable(pisa$ESCS.decile,
       weights = pisa$W_FSTUWT,
       stat = "prop") # exactly 10% for each decile


# calculating academic proficiency per class of ESCS
# reading
escs.score.read <- pisa.mean.pv(pvlabel = "READ",
                                by = "ESCS.decile",
                                pisa) %>% 
  select(ESCS.decile, Mean, s.e.) %>% 
  rename(read.mean = Mean,
         read.se = s.e.) %>% 
  mutate(ESCS.decile = factor(ESCS.decile,
                              levels = 1:10,
                              labels = c("0-10%",
                                         "10-20%",
                                         "20-30%",
                                         "30-40%",
                                         "40-50%",
                                         "50-60%",
                                         "60-70%",
                                         "70-80%",
                                         "80-90%",
                                         "90-100%"))) %>% 
  drop_na()
  

# mathematics
escs.score.math <- pisa.mean.pv(pvlabel = "MATH",
                                by = "ESCS.decile",
                                pisa) %>% 
  select(ESCS.decile, Mean, s.e.) %>% 
  rename(math.mean = Mean,
         math.se = s.e.) %>% 
  mutate(ESCS.decile = factor(ESCS.decile,
                              levels = 1:10,
                              labels = c("0-10%",
                                         "10-20%",
                                         "20-30%",
                                         "30-40%",
                                         "40-50%",
                                         "50-60%",
                                         "60-70%",
                                         "70-80%",
                                         "80-90%",
                                         "90-100%"))) %>% 
  drop_na()

# science
escs.score.scie <- pisa.mean.pv(pvlabel = "SCIE",
                                by = "ESCS.decile",
                                pisa) %>% 
  select(ESCS.decile, Mean, s.e.) %>% 
  rename(scie.mean = Mean,
         scie.se = s.e.) %>% 
  mutate(ESCS.decile = factor(ESCS.decile,
                              levels = 1:10,
                              labels = c("0-10%",
                                         "10-20%",
                                         "20-30%",
                                         "30-40%",
                                         "40-50%",
                                         "50-60%",
                                         "60-70%",
                                         "70-80%",
                                         "80-90%",
                                         "90-100%"))) %>% 
  drop_na()


# joining data
escs.proficiency <- left_join(left_join(escs.score.read,
                                        escs.score.math),
                              escs.score.scie)

# visualizing table
gt(escs.proficiency) %>% 
  tab_header(title = md("**Mean Proficiency per ESCS**")) %>% 
  tab_spanner(label = "Reading",
              columns = c(read.mean, read.se)) %>% 
  tab_spanner(label = "Mathematics",
              columns = c(math.mean, math.se)) %>% 
  tab_spanner(label = "Sciences",
              columns = c(scie.mean, scie.se)) %>% 
  cols_align(ESCS.decile, align = "left") %>% 
  cols_label(ESCS.decile = "ESCS Decile",
             read.mean = "Mean",
             read.se = "SE",
             math.mean = "Mean",
             math.se = "SE", 
             scie.mean = "Mean",
             scie.se = "SE") %>% 
  tab_footnote("SE = Standard Error",
               locations = cells_column_labels(columns = read.se)) %>% 
  data_color(columns = c(read.mean, math.mean, scie.mean),
             colors = scales::col_numeric(palette = "viridis",
                                          domain = c(0,502)))

# exporting
export(escs.proficiency, "Results/6. ESCS Proficiency.csv")

# graphically
escs.proficiency %>% 
  pivot_longer(cols = c(read.mean, math.mean, scie.mean),
               names_to = "Area",
               values_to = "Mean") %>% 
  ggplot(aes(ESCS.decile, Mean)) +
  geom_point(aes(color = Area)) +
  geom_line(aes(group = Area,
                color = Area)) +
  theme_bw() +
  labs(title = "Mean proficiency score per ESCS decile",
       x = "ESCS Decile",
       y = "Mean Score") +
  scale_y_continuous(limits=c(0,600)) +
  scale_color_paletteer_d("nbapalettes::bulls_holiday")

# deleting temporary files
rm(decile.se,
   escs.decile,
   escs.proficiency,
   escs.score.read,
   escs.score.math,
   escs.score.scie,
   rw.decile,
   fw.decile,
   j,
   w)


# EXAMINING PERFORMANCE PER SCHOOL TYPE ----------------------------------------

# ASSOCIACAO ENTRE NSE E REDE ESCOLAR ------------------------------------------

pisa.mean("ESCS",
          data = pisa2018,
          by = "SCHLTYPE") %>% 
  mutate(SCHLTYPE = case_when(SCHLTYPE == 1 ~ "Privada",
                              SCHLTYPE == 3 ~ "Pública"),
         Mean = round(Mean, 2),
         s.e. = round(s.e., 2),
         SD = round(SD, 2),
         s.e = round(s.e, 2)) %>% 
  rename(`Rede Escolar` = SCHLTYPE,
         `Média (EP)` = Mean,
         `Desvio Padrão (EP)` = SD,
         n = Freq) %>%
  gt() %>% 
  tab_header(title = md("**Média de NSE por tipo de escola**")) %>% 
  tab_source_note(source = md("Fonte: Dados do PISA 2018. *Elaboração própria*.")) %>% 
  cols_merge(columns = c(`Média (EP)`, s.e.),
             pattern = "{1} ({2})") %>% 
  cols_merge(columns = c(`Desvio Padrão (EP)`, s.e),
             pattern = "{1} ({2})") %>% 
  tab_footnote(footnote = md("*EP = Erro padrão*"),
               locations = cells_column_labels(columns = `Média (EP)`))

# PROFICIENCIA POR REDE ESCOLAR ------------------------------------------------

pisa.mean.pv(paste0("PV",1:10,"READ"),
             data = pisa2018,
             by = "SCHLTYPE") %>%  
  rename(`Rede Escolar` = SCHLTYPE,
         `Média (EP)` = Mean,
         `Desvio Padrão (EP)` = SD,
         n = Freq) %>% 
  mutate(`Rede Escolar` = case_when(`Rede Escolar` == 1 ~ "Privada",
                                    `Rede Escolar` == 3 ~ "Pública")) %>% 
  gt() %>% 
  tab_header(title = md("**Média de Leitura por REDE**")) %>% 
  tab_source_note(source = md("Fonte: Dados do PISA 2018. *Elaboração própria*.")) %>% 
  cols_merge(columns = c(`Média (EP)`, s.e.),
             pattern = "{1} ({2})") %>% 
  cols_merge(columns = c(`Desvio Padrão (EP)`, s.e),
             pattern = "{1} ({2})") %>% 
  tab_footnote(footnote = md("*EP = Erro padrão*"),
               locations = cells_column_labels(columns = `Média (EP)`))

pisa.mean.pv(paste0("PV",1:10,"MATH"),
             data = pisa2018,
             by = "SCHLTYPE") %>%  
  rename(`Rede Escolar` = SCHLTYPE,
         `Média (EP)` = Mean,
         `Desvio Padrão (EP)` = SD,
         n = Freq) %>% 
  mutate(`Rede Escolar` = case_when(`Rede Escolar` == 1 ~ "Privada",
                                    `Rede Escolar` == 3 ~ "Pública")) %>% 
  gt() %>% 
  tab_header(title = md("**Média de Matemática por NSE**")) %>% 
  tab_source_note(source = md("Fonte: Dados do PISA 2018. *Elaboração própria*.")) %>% 
  cols_merge(columns = c(`Média (EP)`, s.e.),
             pattern = "{1} ({2})") %>% 
  cols_merge(columns = c(`Desvio Padrão (EP)`, s.e),
             pattern = "{1} ({2})") %>% 
  tab_footnote(footnote = md("*EP = Erro padrão*"),
               locations = cells_column_labels(columns = `Média (EP)`))

pisa.mean.pv(paste0("PV",1:10,"SCIE"),
             data = pisa2018,
             by = "SCHLTYPE") %>%  
  rename(`Rede Escolar` = SCHLTYPE,
         `Média (EP)` = Mean,
         `Desvio Padrão (EP)` = SD,
         n = Freq) %>% 
  mutate(`Rede Escolar` = case_when(`Rede Escolar` == 1 ~ "Privada",
                                    `Rede Escolar` == 3 ~ "Pública")) %>% 
  gt() %>% 
  tab_header(title = md("**Média de Ciências por NSE**")) %>% 
  tab_source_note(source = md("Fonte: Dados do PISA 2018. *Elaboração própria*.")) %>% 
  cols_merge(columns = c(`Média (EP)`, s.e.),
             pattern = "{1} ({2})") %>% 
  cols_merge(columns = c(`Desvio Padrão (EP)`, s.e),
             pattern = "{1} ({2})") %>% 
  tab_footnote(footnote = md("*EP = Erro padrão*"),
               locations = cells_column_labels(columns = `Média (EP)`))

# PROFICIENCIA POR REDE E POR NSE ----------------------------------------------

pisapriv <- pisa2018 %>% 
  filter(SCHLTYPE == 1)

pisapub <- pisa2018 %>% 
  filter(SCHLTYPE == 3)


priv <- pisa.mean.pv(paste0("PV",1:10,"MATH"),
                     data = pisapriv,
                     by = "ESCSper")

pub <-  pisa.mean.pv(paste0("PV",1:10,"MATH"),
                     data = pisapub,
                     by = "ESCSper")

full_join(priv,pub, by = "ESCSper") %>% 
  select(ESCSper, 
         Mean.x, s.e..x, SD.x, s.e.x,
         Mean.y, s.e..y, SD.y, s.e.y) %>% 
  gt() %>% 
  tab_header(title = md("**Média de Matemática por Rede e por NSE**")) %>% 
  cols_merge(columns = c(Mean.x, s.e..x),
             pattern = "{1} ({2})") %>% 
  cols_merge(columns = c(Mean.y, s.e..y),
             pattern = "{1} ({2})") %>% 
  cols_merge(columns = c(SD.x, s.e.x),
             pattern = "{1} ({2})") %>% 
  cols_merge(columns = c(SD.y, s.e.y),
             pattern = "{1} ({2})") %>% 
  tab_spanner(label = md("**Privada**"),
              columns = c(Mean.x, SD.x)) %>% 
  tab_spanner(label = md("**Pública**"),
              columns = c(Mean.y, SD.y)) %>% 
  tab_footnote(footnote = md("*EP = Erro padrão*"),
               locations = cells_column_labels(Mean.x)) %>% 
  tab_source_note(source = md("Fonte: Dados do PISA 2018. *Elaboração própria*.")) %>% 
  cols_label(ESCSper = "Decil de NSE",
             Mean.x = "Média (EP)",
             SD.x = "Desvio Padrão (EP)",
             Mean.y = "Média (EP)",
             SD.y = "Desvio Padrão (EP)")



