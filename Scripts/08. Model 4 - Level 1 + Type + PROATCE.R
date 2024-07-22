# SETUP ------------------------------------------------------------------------

# cleaning environment
rm(list=ls())
options(scipen = 999)

# loading packages
if(!require("pacman")) install.packages("pacman")

p_load(tidyverse,
       janitor,
       rio,
       lme4,
       gt,
       paletteer)


# LOADING AND PREPARING DATA ---------------------------------------------------

pisa <- import("Data/pisa_bra.csv")

# recoding categorical variables
pisa <- pisa %>% 
  mutate(SEX = factor(SEX,
                      levels = c(1, 2),
                      labels = c("Female", "Male")),
         GRADE = case_when(GRADE == 7 ~ 1,
                           GRADE == 8 ~ 1,
                           GRADE == 9 ~ 1,
                           GRADE == 10 ~ 2,
                           GRADE == 11 ~ 3,
                           GRADE == 12 ~ 3,
                           GRADE == 13 ~ 3),
         GRADE = factor(GRADE,
                        levels = c(1,2,3),
                        labels = c("Behind", "Correct", "Ahead")),
         GRADE = relevel(GRADE,
                         ref = "Correct"),
         ISCEDO = factor(ISCEDO,
                         levels = c(1, 2, 3, 4),
                         labels = c("General", "Pre-Vocational",
                                    "Vocational", "Modular")),
         SCHLTYPE = factor(SCHLTYPE,
                           levels = c(1,2,3),
                           labels = c("PrivateInd", "PrivateDep", "Public")),
         SCHLTYPE = relevel(SCHLTYPE,
                            ref = "Public"))



# NULL MODELS ------------------------------------------------------------------

## Reading ----

# Reading null model with final weights
fwread <- lapply(1:10,
                 function(pv)
                   lmer(pisa[,0+pv] ~ SEX + GRADE + ISCEDO + 
                          DISCLIMA + ESCS + SCHLTYPE + PROATCE + (1|CNTSCHID),
                        data = pisa,
                        weights = std_wgt))

# Reading null model with replicated weights
rpread <- lapply(1:80,
                 function(w)
                   lapply(1:10,
                          function(pv)
                            lmer(pisa[,0+pv] ~ SEX + GRADE + ISCEDO + 
                                   DISCLIMA + ESCS + SCHLTYPE + PROATCE + (1|CNTSCHID),
                                 data = pisa,
                                 weights = pisa[,41+w])))


# saving variables names and pisa constant 
names <- names(fixef(fwread[[1]]))
k <- 0.05

# saving coefficients
for(pv in 1:10){
  if(pv == 1){
    coef.fw.pv <- NULL
  }
  coef.fw.pv <- rbind(coef.fw.pv, fixef(fwread[[pv]]))
  if(pv == 10) {
    coef.fw <- apply(coef.fw.pv, 2, mean)
  }
}


# saving coefficients from replicated regressions
for(pv in 1:10) {
  if(pv == 1) {
    coef.rp.pv <- NULL
  }
  coef.rp <- NULL
  for(w in 1:80){
    coef.rp <- rbind(coef.rp, fixef(rpread[[w]][[pv]]))
  }
  coef.rp.pv <- cbind(coef.rp.pv, coef.rp)
  if(pv == 10) {
    rm(coef.rp)
  }
}


# calculating coefficients variance
for(pv in 1:10){
  if(pv == 1){
    var.coef <- NULL
  }
  var.coef.pv <- NULL
  for(var in 1:length(names)){
    var.coef.pv <- rbind(var.coef.pv,
                         sum((coef.rp.pv[,(var+(length(names)*(pv-1)))]-coef.fw.pv[pv,var])^2))
  }
  var.coef <- cbind(var.coef, var.coef.pv)
  if(pv == 10) {
    var.coef <- apply(var.coef, 1, sum)*k/pv
    var.coef <- setNames(var.coef, names)
    rm(var.coef.pv)
  }
}


# calculating imputation variance
for (var in 1:length(names)) {
  if(var == 1) {
    var.imp <- NULL
  }
  var.imp <- rbind(var.imp, sum((coef.fw.pv[,var]-coef.fw[var])^2)/9)
  if (var == length(names)) {
    var.imp <- setNames(as.numeric(var.imp), names)
  }
}


# assembling and saving coefficients results
fe.read.m4 <- data.frame(Coef = coef.fw,
                         S.E. = sqrt(var.coef+(1.1*var.imp)),
                         `t-value` = coef.fw/sqrt(var.coef+(1.1*var.imp)),
                         `p-value` = round(1-pt(abs(coef.fw/sqrt(var.coef+(1.1*var.imp))), 
                                                df = Inf), 
                                           6),
                         Area = "Reading") %>% 
  rownames_to_column(var = "Variable")


# calculating icc
for(pv in 1:10){
  if(pv == 1){
    icc.read.m4 <- NULL
  }
  icc.read.m4 <- rbind(icc.read.m4,
                         as.data.frame(VarCorr(fwread[[pv]]))[1,"vcov"]/
                           sum(as.data.frame(VarCorr(fwread[[pv]]))[,"vcov"]))
  if(pv == 10){
    icc.read.m4 <- mean(icc.read.m4)
  }
}


# saving random effects
for(pv in 1:10){
  if(pv == 1){
    stats = NULL
    error = NULL
  }
  stats <- cbind(stats, data.frame(ranef(fwread[[pv]]))$condval)
  error <- cbind(error, data.frame(ranef(fwread[[pv]]))$condsd)
  if(pv == 10){
    re.read.m4 <- data.frame(model = "Type + PROATCE",
                             area = "Reading",
                             school = data.frame(ranef(fwread[[pv]]))$grp,
                             condval = apply(stats, 1, mean),
                             condsd = apply(error, 1, mean))
  }
}


# deleting temporary data
rm(fwread,
   rpread,
   coef.fw,
   coef.fw.pv,
   coef.rp.pv,
   var.coef,
   var.imp,
   error,
   stats)


## Mathematics ----

# Mathematics null model with final weights
fwmath <- lapply(1:10,
                 function(pv)
                   lmer(pisa[,10+pv] ~ SEX + GRADE + ISCEDO + 
                          DISCLIMA + ESCS + SCHLTYPE + PROATCE + (1|CNTSCHID),
                        data = pisa,
                        weights = std_wgt))

# Mathematics null model with replicated weights
rpmath <- lapply(1:80,
                 function(w)
                   lapply(1:10,
                          function(pv)
                            lmer(pisa[,10+pv] ~ SEX + GRADE + ISCEDO + 
                                   DISCLIMA + ESCS + SCHLTYPE + PROATCE + (1|CNTSCHID),
                                 data = pisa,
                                 weights = pisa[,41+w])))


# saving coefficients
for(pv in 1:10){
  if(pv == 1){
    coef.fw.pv <- NULL
  }
  coef.fw.pv <- rbind(coef.fw.pv, fixef(fwmath[[pv]]))
  if(pv == 10) {
    coef.fw <- apply(coef.fw.pv, 2, mean)
  }
}


# saving coefficients from replicated regressions
for(pv in 1:10) {
  if(pv == 1) {
    coef.rp.pv <- NULL
  }
  coef.rp <- NULL
  for(w in 1:80){
    coef.rp <- rbind(coef.rp, fixef(rpmath[[w]][[pv]]))
  }
  coef.rp.pv <- cbind(coef.rp.pv, coef.rp)
  if(pv == 10) {
    rm(coef.rp)
  }
}


# calculating coefficients variance
for(pv in 1:10){
  if(pv == 1){
    var.coef <- NULL
  }
  var.coef.pv <- NULL
  for(var in 1:length(names)){
    var.coef.pv <- rbind(var.coef.pv,
                         sum((coef.rp.pv[,(var+(length(names)*(pv-1)))]-coef.fw.pv[pv,var])^2))
  }
  var.coef <- cbind(var.coef, var.coef.pv)
  if(pv == 10) {
    var.coef <- apply(var.coef, 1, sum)*k/pv
    var.coef <- setNames(var.coef, names)
    rm(var.coef.pv)
  }
}


# calculating imputation variance
for (var in 1:length(names)) {
  if(var == 1) {
    var.imp <- NULL
  }
  var.imp <- rbind(var.imp, sum((coef.fw.pv[,var]-coef.fw[var])^2)/9)
  if (var == length(names)) {
    var.imp <- setNames(as.numeric(var.imp), names)
  }
}


# assembling and saving coefficients results
fe.math.m4 <- data.frame(Coef = coef.fw,
                         S.E. = sqrt(var.coef+(1.1*var.imp)),
                         `t-value` = coef.fw/sqrt(var.coef+(1.1*var.imp)),
                         `p-value` = round(1-pt(abs(coef.fw/sqrt(var.coef+(1.1*var.imp))), 
                                                df = Inf), 
                                           6),
                         Area = "Mathematics") %>% 
  rownames_to_column(var = "Variable")


# calculating icc
for(pv in 1:10){
  if(pv == 1){
    icc.math.m4 <- NULL
  }
  icc.math.m4 <- rbind(icc.math.m4,
                       as.data.frame(VarCorr(fwmath[[pv]]))[1,"vcov"]/
                         sum(as.data.frame(VarCorr(fwmath[[pv]]))[,"vcov"]))
  if(pv == 10){
    icc.math.m4 <- mean(icc.math.m4)
  }
}


# saving random effects
for(pv in 1:10){
  if(pv == 1){
    stats = NULL
    error = NULL
  }
  stats <- cbind(stats, data.frame(ranef(fwmath[[pv]]))$condval)
  error <- cbind(error, data.frame(ranef(fwmath[[pv]]))$condsd)
  if(pv == 10){
    re.math.m4 <- data.frame(model = "Type + PROATCE",
                             area = "Mathematics",
                             school = data.frame(ranef(fwmath[[pv]]))$grp,
                             condval = apply(stats, 1, mean),
                             condsd = apply(error, 1, mean))
  }
}


# deleting temporary data
rm(fwmath,
   rpmath,
   coef.fw,
   coef.fw.pv,
   coef.rp.pv,
   var.coef,
   var.imp,
   error,
   stats)




## Sciences ----

# Sciences null model with final weights
fwscie <- lapply(1:10,
                 function(pv)
                   lmer(pisa[,20+pv] ~ SEX + GRADE + ISCEDO + 
                          DISCLIMA + ESCS + SCHLTYPE + PROATCE + (1|CNTSCHID),
                        data = pisa,
                        weights = std_wgt))

# Sciences null model with replicated weights
rpscie <- lapply(1:80,
                 function(w)
                   lapply(1:10,
                          function(pv)
                            lmer(pisa[,20+pv] ~ SEX + GRADE + ISCEDO + 
                                   DISCLIMA + ESCS + SCHLTYPE + PROATCE + (1|CNTSCHID),
                                 data = pisa,
                                 weights = pisa[,41+w])))


# saving coefficients
for(pv in 1:10){
  if(pv == 1){
    coef.fw.pv <- NULL
  }
  coef.fw.pv <- rbind(coef.fw.pv, fixef(fwscie[[pv]]))
  if(pv == 10) {
    coef.fw <- apply(coef.fw.pv, 2, mean)
  }
}


# saving coefficients from replicated regressions
for(pv in 1:10) {
  if(pv == 1) {
    coef.rp.pv <- NULL
  }
  coef.rp <- NULL
  for(w in 1:80){
    coef.rp <- rbind(coef.rp, fixef(rpscie[[w]][[pv]]))
  }
  coef.rp.pv <- cbind(coef.rp.pv, coef.rp)
  if(pv == 10) {
    rm(coef.rp)
  }
}


# calculating coefficients variance
for(pv in 1:10){
  if(pv == 1){
    var.coef <- NULL
  }
  var.coef.pv <- NULL
  for(var in 1:length(names)){
    var.coef.pv <- rbind(var.coef.pv,
                         sum((coef.rp.pv[,(var+(length(names)*(pv-1)))]-coef.fw.pv[pv,var])^2))
  }
  var.coef <- cbind(var.coef, var.coef.pv)
  if(pv == 10) {
    var.coef <- apply(var.coef, 1, sum)*k/pv
    var.coef <- setNames(var.coef, names)
    rm(var.coef.pv)
  }
}


# calculating imputation variance
for (var in 1:length(names)) {
  if(var == 1) {
    var.imp <- NULL
  }
  var.imp <- rbind(var.imp, sum((coef.fw.pv[,var]-coef.fw[var])^2)/9)
  if (var == length(names)) {
    var.imp <- setNames(as.numeric(var.imp), names)
  }
}


# assembling and saving coefficients results
fe.scie.m4 <- data.frame(Coef = coef.fw,
                         S.E. = sqrt(var.coef+(1.1*var.imp)),
                         `t-value` = coef.fw/sqrt(var.coef+(1.1*var.imp)),
                         `p-value` = round(1-pt(abs(coef.fw/sqrt(var.coef+(1.1*var.imp))), 
                                                df = Inf), 
                                           6),
                         Area = "Sciences") %>% 
  rownames_to_column(var = "Variable")


# calculating icc
for(pv in 1:10){
  if(pv == 1){
    icc.scie.m4 <- NULL
  }
  icc.scie.m4 <- rbind(icc.scie.m4,
                       as.data.frame(VarCorr(fwscie[[pv]]))[1,"vcov"]/
                         sum(as.data.frame(VarCorr(fwscie[[pv]]))[,"vcov"]))
  if(pv == 10){
    icc.scie.m4 <- mean(icc.scie.m4)
  }
}


# saving random effects
for(pv in 1:10){
  if(pv == 1){
    stats = NULL
    error = NULL
  }
  stats <- cbind(stats, data.frame(ranef(fwscie[[pv]]))$condval)
  error <- cbind(error, data.frame(ranef(fwscie[[pv]]))$condsd)
  if(pv == 10){
    re.scie.m4 <- data.frame(model = "Type + PROATCE",
                             area = "Sciences",
                             school = data.frame(ranef(fwscie[[pv]]))$grp,
                             condval = apply(stats, 1, mean),
                             condsd = apply(error, 1, mean))
  }
}


# deleting temporary data
rm(fwscie,
   rpscie,
   coef.fw,
   coef.fw.pv,
   coef.rp.pv,
   var.coef,
   var.imp,
   error,
   stats)


# GRAPHICS AND TABLES ----------------------------------------------------------

## Fixed effects
fe <- rbind(fe.read.m4,
            fe.math.m4,
            fe.scie.m4)
export(fe, "Results/19. Fixed Effects in Model 4.csv")

# graphically
fe.graph <- left_join(left_join(fe.read.m4, 
                    fe.math.m4, 
                    by = "Variable"),
          fe.scie.m4,
          by = "Variable") %>% 
  select(!c(Area.x, Area.y, Area))

cbind(Variable = fe.graph$Variable, round(fe.graph[,2:13],2)) %>% 
  mutate(Variable = case_when(Variable == "(Intercept)" ~ "Intercept",
                              Variable == "SEXMale" ~ "SEX - Male",
                              Variable == "GRADEBehind" ~ "GRADE - Behind",
                              Variable == "GRADEAhead" ~ "GRADE - Ahead",
                              Variable == "ISCEDOVocational" ~ "ISCEDO - Vocational",
                              Variable == "SCHLTYPEPrivateInd" ~ "SCHLTYPE - Private",
                              TRUE ~ Variable)) %>% 
  gt(rowname_col = "Variable",
     row_group_as_column = T) %>% 
  tab_header(title = md("**Fixed Effects - Model 4**")) %>% 
  cols_merge(columns = c(Coef.x, S.E..x),
             pattern = "{1} ({2})") %>%
  cols_merge(columns = c(Coef.y, S.E..y),
             pattern = "{1} ({2})") %>% 
  cols_merge(columns = c(Coef, S.E.),
             pattern = "{1} ({2})") %>%
  tab_footnote(footnote = md("*SE = Standard Error*"),
               locations = cells_column_labels(Coef.x)) %>% 
  tab_spanner(label = md("**Reading**"),
              column = c(Coef.x, t.value.x, p.value.x)) %>% 
  tab_spanner(label = md("**Mathematics**"),
              column = c(Coef.y, t.value.y, p.value.y)) %>% 
  tab_spanner(label = md("**Sciences**"),
              column = c(Coef, t.value, p.value)) %>% 
  cols_label(Coef.x = "Estimate (SE)",
             t.value.x = "T-Value",
             p.value.x = "P-Value",
             Coef.y = "Estimate (SE)",
             t.value.y = "T-Value",
             p.value.y = "P-Value",
             Coef = "Estimate (SE)",
             t.value = "T-Value",
             p.value = "P-Value")



## ICC
icc.m4 <- data.frame(Area = c("Reading",
                                "Mathematics",
                                "Sciences"),
                       ICC = c(icc.read.m4,
                               icc.math.m4,
                               icc.scie.m4))
export(icc.m4, "Results/20. ICC in Model 4.csv")

# graphically
icc.m4 %>% 
  mutate(ICC = round(ICC, 2)) %>% 
  group_by(Area) %>% 
  gt(row_group_as_column = T) %>% 
  tab_header(title = md("**ICC - Model 4**"))


## Random Effects
re.m4 <- rbind(re.read.m4,
               re.math.m4,
               re.scie.m4)
export(re.m4, "Results/21. Random Effects in Model 4.csv")

sch.type <- pisa %>% 
  group_by(CNTSCHID) %>% 
  summarise(Type = unique(SCHLTYPE)) %>% 
  ungroup() %>% 
  mutate(Type = case_when(Type == "Public" ~ "Public",
                          Type == "PrivateInd" ~ "Private",
                          Type == "PrivateDep" ~ "Private"),
         CNTSCHID = factor(CNTSCHID)) %>% 
  rename("school" = CNTSCHID)

left_join(re.m4, sch.type) %>% 
  arrange(condval) %>% 
  mutate(term = "Intercept") %>% 
  ggplot(aes(condval, school)) +
  geom_errorbarh(aes(xmin=condval-2*condsd,
                     xmax=condval+2*condsd),
                 alpha = .3) +
  geom_point(aes(color = Type)) +
  scale_y_discrete(labels = NULL) +
  scale_x_continuous(limits = c(-250,250)) +
  theme_bw() + 
  facet_grid(rows = vars(area)) +
  labs(title = "Random Effects - Model 4",
       y = "Schools",
       x = "Random Effects") +
  scale_color_paletteer_d("fishualize::Acanthurus_leucosternon")
