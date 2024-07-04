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


# EXAMINING ESCS PER SCHOOL TYPE -----------------------------------------------

# recoding school type
pisa <- pisa %>% 
  mutate(SCHLTYPE = factor(SCHLTYPE,
                           levels = c(1,3),
                           labels = c("Private",
                                      "Public")))

# calculating mean ESCS per school type
escs.type <- pisa.mean(variable = "ESCS",
                       data = pisa,
                       by = "SCHLTYPE") %>% 
  select(SCHLTYPE, Mean, s.e.) %>% 
  drop_na()

# exporting
export(escs.type, "Results/7. ESCS School Type.csv")

# visualizing table
gt(escs.type) %>% 
  tab_header(title = md("**Mean ESCS per School Type**")) %>% 
  cols_align(columns = SCHLTYPE,
             align = "left") %>% 
  cols_label(SCHLTYPE = "School Type",
             s.e. = "Standard Error") %>% 
  fmt_number(columns = c(Mean, s.e.),
             decimals = 3)

# graphically
pisa %>% 
  drop_na(SCHLTYPE) %>% 
  ggplot(aes(ESCS, after_stat(count))) +
  geom_density(aes(fill = SCHLTYPE),
               position = "stack") +
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +
  theme_bw() +
  labs(title = "ESCS Distribution per School Type (Stacked)",
       y = "Frequency") +
  guides(fill = guide_legend(title = "School Type"))



# deleting temporary data
rm(escs.type)


# EXAMINING PERFORMANCE PER SCHOOL TYPE ----------------------------------------

# calculating mean performance for each PISA major domain
# Reading
type.score.read <- pisa.mean.pv(pvlabel = "READ",
                                data = pisa,
                                by = "SCHLTYPE") %>% 
  select(SCHLTYPE, Mean, s.e.) %>% 
  rename(read.mean = Mean,
         read.se = s.e.) %>% 
  drop_na()

# Mathematics
type.score.math <- pisa.mean.pv(pvlabel = "MATH",
                                data = pisa,
                                by = "SCHLTYPE") %>% 
  select(SCHLTYPE, Mean, s.e.) %>% 
  rename(math.mean = Mean,
         math.se = s.e.) %>% 
  drop_na()

# Sciences
type.score.scie <- pisa.mean.pv(pvlabel = "SCIE",
                                data = pisa,
                                by = "SCHLTYPE") %>% 
  select(SCHLTYPE, Mean, s.e.) %>% 
  rename(scie.mean = Mean,
         scie.se = s.e.) %>% 
  drop_na()


# joining tables
type.score <- left_join(left_join(type.score.read, 
                                  type.score.math),
                        type.score.scie)


# saving table
export(type.score, "Results/8. School Type Proficiency.csv")

# visualizing table
gt(type.score) %>% 
  tab_header(title = md("**Academic Performance per School Type**")) %>% 
  tab_spanner(label = "Reading",
              columns = c(read.mean, read.se)) %>% 
  tab_spanner(label = "Mathematics",
              columns = c(math.mean, math.se)) %>% 
  tab_spanner(label = "Sciences",
              columns = c(scie.mean, scie.se)) %>% 
  cols_align(columns = SCHLTYPE,
             align = "left") %>% 
  cols_label(SCHLTYPE = "School Type",
             read.mean = "Mean",
             read.se = "SE",
             math.mean = "Mean",
             math.se = "SE",
             scie.mean = "Mean",
             scie.se = "SE") %>% 
  tab_footnote(footnote = md("*SE = Standard Error*"),
               locations = cells_column_labels(read.se))

# graphically
type.score %>% 
  pivot_longer(cols = c(read.mean, math.mean, scie.mean),
               values_to = "Mean",
               names_to = "Area") %>% 
  ggplot(aes(Area, Mean)) +
  geom_col(aes(fill = SCHLTYPE),
           position = "dodge") +
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +
  guides(fill = guide_legend(title = "School Type")) +
  labs(title = "Mean Proficiency per School Type",
       x = "Major Area",
       y = "Mean Score") +
  scale_x_discrete(labels = c("Mathematics", "Reading", "Sciences")) +
  theme_bw()


# EXAMINING THE TRIPLE CORRELATION ---------------------------------------------

# separating databases
pisa.priv <- pisa %>% 
  filter(SCHLTYPE == "Private")

pisa.pub <- pisa %>% 
  filter(SCHLTYPE == "Public")


# calculating means per escs recile

## Private
# Reading
priv.mean.read <- pisa.mean.pv(pvlabel = "READ",
                               pisa.priv,
                               by = "ESCS.decile") %>% 
  select(ESCS.decile, 
         read.mean = Mean, 
         read.se = s.e.) %>% 
  drop_na(ESCS.decile)

# Mathematics
priv.mean.math <- pisa.mean.pv(pvlabel = "MATH",
                               pisa.priv,
                               by = "ESCS.decile") %>% 
  select(ESCS.decile,
         math.mean = Mean,
         math.se = s.e.) %>% 
  drop_na(ESCS.decile)

# Science
priv.mean.scie <- pisa.mean.pv(pvlabel = "SCIE",
                               pisa.priv,
                               by = "ESCS.decile") %>% 
  select(ESCS.decile,
         scie.mean = Mean,
         scie.se = s.e.) %>% 
  drop_na(ESCS.decile)


## Public
# Reading
pub.mean.read <- pisa.mean.pv(pvlabel = "READ",
                               pisa.pub,
                               by = "ESCS.decile") %>% 
  select(ESCS.decile, 
         read.mean = Mean, 
         read.se = s.e.) %>% 
  drop_na(ESCS.decile)

# Mathematics
pub.mean.math <- pisa.mean.pv(pvlabel = "MATH",
                               pisa.pub,
                               by = "ESCS.decile") %>% 
  select(ESCS.decile,
         math.mean = Mean,
         math.se = s.e.) %>% 
  drop_na(ESCS.decile)

# Science
pub.mean.scie <- pisa.mean.pv(pvlabel = "SCIE",
                               pisa.pub,
                               by = "ESCS.decile") %>% 
  select(ESCS.decile,
         scie.mean = Mean,
         scie.se = s.e.) %>% 
  drop_na(ESCS.decile)



# joining tables
priv <- left_join(left_join(priv.mean.read,
                            priv.mean.math),
                  priv.mean.scie) %>% 
  as.data.frame() %>% 
  mutate(Type = "Private")

pub <- left_join(left_join(pub.mean.read,
                           pub.mean.math),
                 pub.mean.scie) %>% 
  as.data.frame() %>% 
  mutate(Type = "Public")


type.escs.score <- rbind(priv, pub)


# saving
export(type.escs.score, "Results/9. Full descriptive correlation.csv")


# visualizing table
type.escs.score %>% 
  pivot_wider(names_from = Type,
              values_from = c(read.mean,
                              read.se,
                              math.mean,
                              math.se,
                              scie.mean,
                              scie.se)) %>% 
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
  gt() %>% 
  tab_header(title = md("**Academic Performance per 
                        School Type and ESCS**")) %>% 
  cols_merge(columns = c(read.mean_Private, read.se_Private),
             pattern = "{1} ({2})") %>% 
  cols_merge(columns = c(read.mean_Public, read.se_Public),
             pattern = "{1} ({2})") %>% 
  cols_merge(columns = c(math.mean_Private, math.se_Private),
             pattern = "{1} ({2})") %>%
  cols_merge(columns = c(math.mean_Public, math.se_Public),
             pattern = "{1} ({2})") %>% 
  cols_merge(columns = c(scie.mean_Private, scie.se_Private),
             pattern = "{1} ({2})") %>% 
  cols_merge(columns = c(scie.mean_Public, scie.se_Public),
             pattern = "{1} ({2})") %>% 
  data_color(columns = c(read.mean_Private,
                         read.mean_Public,
                         math.mean_Private,
                         math.mean_Public,
                         scie.mean_Private,
                         scie.mean_Public),
             scales::col_numeric(palette = "viridis",
                                 domain = c(0,538))) %>% 
  cols_align(columns = ESCS.decile,
             align = "left") %>% 
  tab_spanner(label = "Reading",
              columns = c(read.mean_Private,
                          read.mean_Public)) %>% 
  tab_spanner(label = "Mathematics",
              columns = c(math.mean_Private,
                          math.mean_Public)) %>% 
  tab_spanner(label = "Science",
              columns = c(scie.mean_Private,
                          scie.mean_Public)) %>% 
  cols_label(ESCS.decile = "ESCS Decile",
             read.mean_Private = "Private",
             read.mean_Public = "Public",
             math.mean_Private = "Private",
             math.mean_Public = "Public",
             scie.mean_Private = "Private",
             scie.mean_Public = "Public") %>% 
  cols_align(columns = c(read.mean_Private,
                         read.mean_Public,
                         math.mean_Private,
                         math.mean_Public,
                         scie.mean_Private,
                         scie.mean_Public),
             align = "center") %>% 
  tab_footnote(footnote = md("*All values are in the format 'Mean (Standard Error)'*"))
  
  