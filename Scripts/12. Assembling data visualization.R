# SETUP ------------------------------------------------------------------------

# cleaning environment
rm(list=ls())
options(scipen=999)

# loading packages
if(!require("pacman")) install.packages("pacman")

p_load(tidyverse,
       rio,
       janitor,
       gt,
       paletteer)

# FIXED EFFECTS ----------------------------------------------------------------

## Loading data ----
fe.null <- import("Results/10. Fixed Effects in Null Model.csv")
fe.m2 <- import("Results/13. Fixed Effects in Model 2.csv")
fe.m3 <- import("Results/16. Fixed Effects in Model 3.csv")
fe.m4 <- import("Results/19. Fixed Effects in Model 4.csv")
fe.m5 <- import("Results/22. Fixed Effects in Model 5.csv")
fe.m6 <- import("Results/25. Fixed Effects in Model 6.csv")
fe.m7 <- import("Results/28. Fixed Effects in Model 7.csv")

## Joining data ----

# Identifying data
fe.null <- fe.null %>% 
  mutate(model = 'Null')

fe.m2 <- fe.m2 %>% 
  mutate(model = 'Mod2')

fe.m3 <- fe.m3 %>% 
  mutate(model = 'Mod3')

fe.m4 <- fe.m4 %>% 
  mutate(model = 'Mod4')

fe.m5 <- fe.m5 %>% 
  mutate(model = 'Mod5')

fe.m6 <- fe.m6 %>% 
  mutate(model = 'Mod6')

fe.m7 <- fe.m7 %>% 
  mutate(model = 'Mod7')

# Binding all
fe <- rbind(fe.null, 
            fe.m2, 
            fe.m3, 
            fe.m4, 
            fe.m5, 
            fe.m6, 
            fe.m7)

## Visualizing ----
fe %>%
  mutate(Variable = case_when(Variable == "(Intercept)" ~ "Intercept",
                              Variable == "SEXMale" ~ "SEX - Male",
                              Variable == "GRADEBehind" ~ "GRADE - Behind",
                              Variable == "GRADEAhead" ~ "GRADE - Ahead",
                              Variable == "ISCEDOVocational" ~ "ISCEDO - Vocational",
                              Variable == "SCHLTYPEPrivateInd" ~ "SCHLTYPE - Private",
                              TRUE ~ Variable),
         Coef = round(Coef, 2),
         S.E. = round(S.E., 3)) %>% 
  select(!c(t.value, p.value)) %>% 
  pivot_wider(names_from = model,
              values_from = c(Coef, S.E.)) %>% 
  group_by(Area) %>% 
  gt(rowname_col = "Variable") %>% 
  tab_row_group(label = md("**Sciences**"),
                rows = Area == "Sciences") %>% 
  tab_row_group(label = md("**Mathematics**"),
                rows = Area == "Mathematics") %>% 
  tab_row_group(label = md("**Reading**"),
                rows = Area == "Reading") %>% 
  sub_missing(missing_text = "-") %>% 
  cols_merge(columns = c(Coef_Null, S.E._Null),
             pattern = "{1} ({2})") %>% 
  cols_merge(columns = c(Coef_Mod2, S.E._Mod2),
             pattern = "{1} ({2})") %>% 
  cols_merge(columns = c(Coef_Mod3, S.E._Mod3),
             pattern = "{1} ({2})") %>% 
  cols_merge(columns = c(Coef_Mod4, S.E._Mod4),
             pattern = "{1} ({2})") %>% 
  cols_merge(columns = c(Coef_Mod5, S.E._Mod5),
             pattern = "{1} ({2})") %>% 
  cols_merge(columns = c(Coef_Mod6, S.E._Mod6),
             pattern = "{1} ({2})") %>% 
  cols_merge(columns = c(Coef_Mod7, S.E._Mod7),
             pattern = "{1} ({2})") %>% 
  cols_label(Coef_Null = md("**Null Model**"),
             Coef_Mod2 = md("**Model 2**"),
             Coef_Mod3 = md("**Model 3**"),
             Coef_Mod4 = md("**Model 4**"),
             Coef_Mod5 = md("**Model 5**"),
             Coef_Mod6 = md("**Model 6**"),
             Coef_Mod7 = md("**Model 7**")) %>% 
  tab_footnote(footnote = md("*All values are in the 'Estimate (Standard Error)' format*"),
               locations = cells_column_labels(Coef_Null)) %>% 
  tab_footnote(footnote = md("*p≥0.1*"),
               locations = cells_body(columns = c(Coef_Mod4, Coef_Mod7),
                                      rows = 25),
               placement = "right") %>% 
  tab_footnote(footnote = md("*p≥0.1*"),
               locations = cells_body(columns = Coef_Mod4,
                                      rows = 26),
               placement = "right") %>% 
  tab_footnote(footnote = md("*p≥0.05*"),
               locations = cells_body(columns = Coef_Mod4,
                                      rows = 27),
               placement = "right") %>% 
  tab_footnote(footnote = md("*p≥0.01*"),
               locations = cells_body(columns = Coef_Mod7,
                                      rows = 26),
               placement = "right") %>% 
  tab_header(title = md("**Fixed Effects**"))



# ICC --------------------------------------------------------------------------

## Loading data ----
icc.null <- import("Results/11. ICC in null model.csv")
icc.m2 <- import("Results/14. ICC in Model 2.csv")
icc.m3 <- import("Results/17. ICC in Model 3.csv")
icc.m4 <- import("Results/20. ICC in Model 4.csv")
icc.m5 <- import("Results/23. ICC in Model 5.csv")
icc.m6 <- import("Results/26. ICC in Model 6.csv")
icc.m7 <- import("Results/29. ICC in Model 7.csv")


## Joining data ----
# identifying models
icc.null <- icc.null %>% 
  mutate(model = "Null Model")

icc.m2 <- icc.m2 %>% 
  mutate(model = "Model 2")

icc.m3 <- icc.m3 %>% 
  mutate(model = "Model 3")

icc.m4 <- icc.m4 %>% 
  mutate(model = "Model 4")

icc.m5 <- icc.m5 %>% 
  mutate(model = "Model 5")

icc.m6 <- icc.m6 %>% 
  mutate(model = "Model 6")

icc.m7 <- icc.m7 %>% 
  mutate(model = "Model 7")

# Binding all
icc <- rbind(icc.null,
             icc.m2,
             icc.m3,
             icc.m4,
             icc.m5,
             icc.m6,
             icc.m7)


## Visualizing ----
icc %>% 
  mutate(ICC = round(ICC, 2)) %>% 
  pivot_wider(names_from = Area,
              values_from = ICC) %>% 
  gt(rowname_col = "model") %>% 
  tab_header(title = md("**Intraclass Correlation Coefficient**")) %>% 
  cols_label(Reading = md("**Reading**"),
             Mathematics = md("**Mathematics**"),
             Sciences = md("**Sciences**")) %>% 
  data_color(columns = c(Reading, Mathematics, Sciences),
             colors = scales::col_numeric(palette = "viridis",
                                      domain = c(.13,.45)))


# RANDOM EFFECTS  --------------------------------------------------------------

## Loading data ----
re.null <- import("Results/12. Random Effects in Null Model.csv")
re.m2 <- import("Results/15. Random Effects in Model 2.csv")
re.m3 <- import("Results/18. Random Effects in Model 3.csv")
re.m4 <- import("Results/21. Random Effects in Model 4.csv")
re.m5 <- import("Results/24. Random Effects in Model 5.csv")
re.m6 <- import("Results/27. Random Effects in Model 6.csv")
re.m7 <- import("Results/30. Random Effects in Model 7.csv")


## Joining data ----
# identifying models
re.null <- re.null %>% 
  mutate(model = 1)

re.m2 <- re.m2 %>% 
  mutate(model = 2)

re.m3 <- re.m3 %>% 
  mutate(model = 3)

re.m4 <- re.m4 %>% 
  mutate(model = 4)

re.m5 <- re.m5 %>% 
  mutate(model = 5)

re.m6 <- re.m6 %>% 
  mutate(model = 6)

re.m7 <- re.m7 %>% 
  mutate(model = 7)

# Binding all
re <- rbind(re.null,
            re.m2,
            re.m3,
            re.m4,
            re.m5,
            re.m6,
            re.m7)

## Visualizing ----
# importing pisa dataset
pisa <- import("Data/pisa_bra.csv")

# creating schooltype dataset
sch.type <- pisa %>% 
  group_by(CNTSCHID) %>% 
  summarise(Type = unique(SCHLTYPE)) %>% 
  ungroup() %>% 
  mutate(Type = case_when(Type == "Public" ~ "Public",
                          Type == "PrivateInd" ~ "Private",
                          Type == "PrivateDep" ~ "Private")) %>% 
  rename("school" = CNTSCHID)


left_join(re, sch.type) %>% 
  mutate(model = factor(model,
                        levels = 1:7,
                        labels = c("Null",
                                   "Modelo 2",
                                   "Modelo 3",
                                   "Modelo 4",
                                   "Modelo 5",
                                   "Modelo 6",
                                   "Modelo 7")),
         area = case_when(area == "Reading" ~ 1,
                          area == "Mathematics" ~ 2,
                          area == "Sciences" ~ 3),
         area = factor(area,
                       levels = 1:3,
                       labels = c("Leitura",
                                  "Mathemática",
                                  "Ciências"))) %>% 
  ggplot(aes(condval, school)) +
  geom_errorbarh(aes(xmin=condval-2*condsd,
                     xmax=condval+2*condsd),
                 alpha = .3) +
  geom_point(aes(color = Type)) +
  theme_bw() +
  scale_y_discrete(name = "Escolas",
                   labels = " ",
                   breaks = NULL) +
  scale_x_continuous(name = "Efeitos Aleatórios",
                     limits = c(-250,250)) +
  facet_grid(rows = vars(factor(model)),
             cols = vars(factor(area))) +
  scale_color_manual(values =c("darkred", "darkblue"))
