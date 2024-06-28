# SETUP ------------------------------------------------------------------------

# cleaning environment
rm(list=ls())
options(scipen = 999)


# loading packages
if(!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               janitor,
               rio,
               PNADcIBGE,
               gt)


# LOADING DATA -----------------------------------------------------------------

pnad2018 <- read_pnadc("Raw Data/PNADC_012018.txt",
                       input = "Raw Data/input_PNADC_trimestral.txt")

# selecting variables
pnad2018 <- pnad2018 %>% 
  dplyr::select(birth.year = V20082,
                student = V3002,
                private = V3002A,
                grade = V3006,
                sex = V2007,
                race = V2010,
                weight = V1028,
                UPA,
                Estrato,
                V1008)


















# checking birth year in PNADC sample
pnad2018 %>% 
  filter(birth.year %in% 1990:2018) %>% 
  mutate(correct.age = ifelse(birth.year == 2002, 1, 0)) %>% 
  ggplot(aes(birth.year)) +
  geom_bar(aes(group = correct.age,
               fill = correct.age),
           show.legend = F) +
  labs(title = "Birth Year distribution in Brazil (1990-2018)",
       y = "Frequency",
       x = "Birth Year") +
  coord_flip() +
  theme_bw()
# PISA sample has only students who born in 2002


# filtering 2002 students



# criando renda domiciliar per capita
# rendimento mensal efetivo VD4020 total

renda <- pnad2018 %>% 
  mutate(hum = 1) %>% 
  group_by(UPA, Estrato, V1008) %>% 
  summarise(rendadom = sum(VD4020, na.rm = T),
            ndom = sum(hum),
            V1016) %>% 
  mutate(rendadompc = rendadom/ndom)

pnad2018 <- pnad2018 %>% 
  mutate(rendadompc = renda$rendadompc)

# copiando a base e selecionando variaveis
pnad2018.o <- pnad2018 %>% 
  dplyr::select(anonasc = V20082,
                escdummy = V3002,
                rede = V3002A,
                seg = V3003A,
                serie = V3006,
                escdummyt0 = V3008,
                rendadompc,
                sexo = V2007,
                raca = V2010,
                peso = V1028,
                UF)


# isolando 2002 na pnad e selecionando variaveis
pnad2018 <- pnad2018 %>% 
  filter(V20082 == 2002) %>% 
  dplyr::select(anonasc = V20082,
                escdummy = V3002,
                rede = V3002A,
                seg = V3003A,
                serie = V3006,
                escdummyt0 = V3008,
                rendadompc,
                sexo = V2007,
                raca = V2010,
                peso = V1028,
                UF)

# EXAMINANDO MATRICULAS BRUTAS -------------------------------------------------

temp <- pnad2018.o %>% 
  filter(anonasc %in% 2000:2012) %>% 
  drop_na(rede) %>% 
  mutate(rede = factor(rede,
                       levels = c(1,2),
                       labels = c("Privada", "Pública")))


weighted.table(temp$anonasc,
               temp$rede,
               weights = temp$peso,
               stat = "rprop")


temp %>% 
  dplyr::select(anonasc, rede, peso) %>%
  group_by(anonasc) %>% 
  summarise(peso.anonasc = sum(peso),
            rede,
            peso) %>% 
  ungroup() %>% 
  mutate(hum = 1,
         Matrículas = (peso/peso.anonasc)*100) %>% 
  rename(Rede = rede,
         `Ano de Nascimento` = anonasc) %>% 
  ggplot(aes(`Ano de Nascimento`, Matrículas)) +
  geom_col(aes(group = Rede,
               fill = Rede)) + 
  theme_bw() +
  labs(y = "Proporção de Matrículas (%)")


# VARIACAO DE RENDA ------------------------------------------------------------

# checando detis de renda
wtd.quantile(pnad2018$rendadompc,
             probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9),
             weights = pnad2018$peso)
#    10%        20%        30%        40%        50%        60%        70%        80%        90% 
#0.00000   83.33333  200.00000  300.00000  400.00000  500.00000  680.00000  940.00000 1425.00000 

# recodificando renda domiciliar percapita para ter classes de resposta
pnad2018 <- pnad2018 %>% 
  mutate(rendadompcq = case_when(rendadompc <= 0 ~ 1,
                                 rendadompc > 0 & rendadompc <= 83.33333 ~ 2,
                                 rendadompc > 83.33333 & rendadompc <= 200 ~ 3,
                                 rendadompc > 200 & rendadompc <= 300 ~ 4,
                                 rendadompc > 300 & rendadompc <= 400 ~ 5,
                                 rendadompc > 400 & rendadompc <= 500 ~ 6,
                                 rendadompc > 500 & rendadompc <= 680 ~ 7,
                                 rendadompc > 680 & rendadompc <= 940 ~ 8,
                                 rendadompc > 940 & rendadompc <= 1425 ~ 9,
                                 rendadompc > 1425 ~ 10))


# resultado em tabela com peso
renda10 <- weighted.table(pnad2018$rendadompcq, pnad2018$rede,
                          weights = pnad2018$peso,
                          stat = "rprop",
                          na.rm = T) %>% 
  as.data.frame() %>% 
  mutate(`Faixa de Renda` = c("0-10%",
                              "10-20%",
                              "20-30%",
                              "30-40%",
                              "40-50%",
                              "50-60%",
                              "60-70%",
                              "70-80%",
                              "80-90%",
                              "90-100%"),
         Privada = `1`,
         Pública = `2`) %>% 
  dplyr::select(!c(`1`,`2`))

renda10 %>% 
  gt() %>% 
  tab_header(title = md("**Porcentagem de Matrículas por Rede e Faixa de Renda Domiciliar Per Capita**")) %>% 
  tab_source_note(source = md("Fonte: Dados da PNADC 2018-2. *Elaboração própria*."))

# resultado em grafico
g.renda10 <- renda10 %>% 
  pivot_longer(c(Privada, Pública),names_to = "Rede") %>% 
  ggplot(aes(`Faixa de Renda`, value)) +
  geom_point(aes(group = Rede,
                 color =  Rede)) +
  geom_line(aes(group = Rede,
                color = Rede)) +
  theme_bw() +
  labs(title = "Matrículas de alunos de 15 anos por rede e renda domiciliar per capita",
       x = "Decis de Renda Domiciliar per Capita",
       y = "Matrículas (%)")


# REFAZENDO PARA OS PERCENTIS 90 A 100 -----------------------------------------


teste <- pnad2018 %>% 
  filter(rendadompcq == 10)

wtd.quantile(teste$rendadompc,
             probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9),
             weights = teste$peso)
#     10%      20%      30%      40%      50%      60%      70%      80%      90% 
#1640.000 1766.667 1900.000 2066.667 2375.000 2600.000 3000.000 3800.000 5750.000


teste <- teste %>% 
  mutate(rendadompcq2 = case_when(rendadompc <= 1500 ~ 1,
                                  rendadompc > 1500 & rendadompc <= 1600 ~ 2,
                                  rendadompc > 1600 & rendadompc <= 1750 ~ 3,
                                  rendadompc > 1750 & rendadompc <= 1975 ~ 4,
                                  rendadompc > 1975 & rendadompc <= 2133.333 ~ 5,
                                  rendadompc > 2133.333 & rendadompc <= 2480 ~ 6,
                                  rendadompc > 2480 & rendadompc <= 2750 ~ 7,
                                  rendadompc > 2750 & rendadompc <= 3333.333 ~ 8,
                                  rendadompc > 3333.333 & rendadompc <= 5000 ~ 9,
                                  rendadompc > 5000 ~ 10))

renda90 <- weighted.table(teste$rendadompcq2, teste$rede,
                          weights = teste$peso,
                          stat = "rprop",
                          na.rm=T) %>% 
  as.data.frame() %>% 
  mutate(`Faixa de Renda` = c("90-91%",
                              "91-92%",
                              "92-93%",
                              "93-94%",
                              "94-95%",
                              "95-96%",
                              "96-97%",
                              "97-98%",
                              "98-99%",
                              "99-100%"),
         Privada = `1`,
         Pública = `2`) %>% 
  dplyr::select(!c(`1`,`2`))

renda90 %>% 
  gt() %>% 
  tab_header(title = md("**Porcentagem de Matrículas por Rede e Faixa de Renda Domiciliar Per Capita**")) %>% 
  tab_source_note(source = md("Fonte: Dados da PNADC 2018-2. *Elaboração própria*."))


g.renda90 <- renda90 %>% 
  pivot_longer(c(Privada, Pública), 
               names_to = "Rede") %>% 
  ggplot(aes(`Faixa de Renda`, value)) +
  geom_point(aes(group = Rede,
                 color = Rede)) +
  geom_line(aes(group = Rede,
                color = Rede)) +
  scale_y_continuous(limits = c(0,100)) +
  theme_bw() +
  labs(title = "Matrículas de alunos de 15 anos por rede e renda domiciliar per capita",
       x = "Percentis de Renda Domiciliar per Capita (90% a 100%)",
       y = "Matrículas (%)")


# juntando os dois graficos
plot_grid(g.renda10, g.renda90,
          align = "hv",
          ncol = 1)

# VARIACAO DE RACA -------------------------------------------------------------

pnad2018 <- pnad2018 %>% 
  mutate(raca = factor(raca,
                       levels = c(1,2,3,4,5,9),
                       labels = c("Branca", "Preta", "Amarela",
                                  "Parda", "Indígena", "Não Respondeu")))
pnad2018 <- pnad2018 %>% 
  mutate(rede = factor(rede,
                       levels = c(1,2),
                       labels = c("Privada", "Pública")))

teste <- teste %>% 
  mutate(raca = factor(raca,
                       levels = c(1,2,3,4,5,9),
                       labels = c("Branca", "Preta", "Amarela",
                                  "Parda", "Indígena", "Não Respondeu")))
teste <- teste %>% 
  mutate(rede = factor(rede,
                       levels = c(1,2),
                       labels = c("Privada", "Pública")))



raca10 <- weighted.table(pnad2018$rede, pnad2018$raca,
                         weight = pnad2018$peso,
                         stat = "cprop",
                         na.rm = T) %>% 
  as.data.frame() %>% 
  dplyr::select(!`Não Respondeu`)


raca10 %>% 
  rownames_to_column() %>% 
  gt() %>% 
  tab_header(title = md("**Porcentagem de Matrículas por Rede e Raça**")) %>% 
  tab_source_note(source = md("Fonte: Dados da PNADC 2018-2. *Elaboração própria*."))

raca10 %>% 
  rownames_to_column() %>% 
  rename(Rede = rowname) %>% 
  pivot_longer(c(Branca, Preta, Amarela, Parda, Indígena),
               names_to = "Raça",
               values_to = "Matrícula") %>% 
  ggplot(aes(Raça, Matrícula)) +
  geom_col(aes(group = Rede,
               fill = Rede),
           width = 0.5) +
  theme_bw() +
  labs(title = "Matrículas por Rede e Raça")