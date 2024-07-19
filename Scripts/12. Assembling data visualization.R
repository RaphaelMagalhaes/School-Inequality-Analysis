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
fe.m2 <- import("Results/12. Fixed Effects in Model 2.csv")
fe.m3 <- import("Results/14. Fixed Effects in Model 3.csv")
fe.m4 <- import("Results/16. Fixed Effects in Model 4.csv")
fe.m5 <- import("Results/18. Fixed Effects in Model 5.csv")
fe.m6 <- import("Results/20. Fixed Effects in Model 6.csv")
fe.m7 <- import("Results/22. Fixed Effects in Model 7.csv")

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
icc.null <- import("Results/11. ICC estimation in null model.csv")
icc.m2 <- import("Results/13. ICC Estimation in Model 2.csv")
icc.m3 <- import("Results/15. ICC estimation in Model 3.csv")
icc.m4 <- import("Results/17. ICC in Model 4.csv")
icc.m5 <- import("Results/19. ICC in Model 5.csv")
icc.m6 <- import("Results/21. ICC in Model 6.csv")
icc.m7 <- import("Results/23. ICC in Model 7.csv")


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


# Visualizing
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