# SETUP ------------------------------------------------------------------------

# cleaning environment
rm(list=ls())
options(scipen=999)


# loading packages
if(!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               janitor,
               rio,
               lme4,
               performance,
               intsvy,
               ggcorrplot)


# LOADING DATA -----------------------------------------------------------------

pisa <- import("Data/pisa_oecd.csv")

# splitting sample by country
pisa.cnt <- lapply(1:length(unique(pisa$CNT)),
                   function(c)
                     pisa %>% 
                     filter(CNT == unique(pisa$CNT)[c]) %>% 
                     drop_na())

# transforming school id in factor
for(c in 1:length(unique(pisa$CNT))) {
  pisa.cnt[[c]] <- pisa.cnt[[c]] %>% 
    mutate(CNTSCHID = factor(CNTSCHID))
}


# NULL MODELS ------------------------------------------------------------------

# looking for PV columns
names(pisa.cnt[[1]])

# PV READ = columns 9 to 18
# PV MATH = columns 19 to 28
# PV SCIE = columns 29 to 38

## Reading ---------------------------------------------------------------------

# final weight null model
fwread <- lapply(1:length(unique(pisa$CNT)),
                 function(c)
                   lapply(1:10,
                          function(pv)
                            lmer(pisa.cnt[[c]][,8+pv]~1+(1|CNTSCHID),
                                 data = pisa.cnt[[c]],
                                 weights = std_wgt)))

# calculating country reading icc
for(c in 1:length(unique(pisa$CNT))) {
  if(c == 1){
    icc.read <- NULL
  }
  icc.read.c <- NULL
  for(pv in 1:10){
    icc.read.c <- rbind(icc.read.c, icc(fwread[[c]][[pv]])$ICC_adjusted)
  }
  icc.read <- rbind(icc.read, mean(icc.read.c))
}


# deleting temporary data
rm(fwread, icc.read.c)


## Mathematics -----------------------------------------------------------------

# final weight null model
fwmath <- lapply(1:length(unique(pisa$CNT)),
                 function(c)
                   lapply(1:10,
                          function(pv)
                            lmer(pisa.cnt[[c]][,18+pv]~1+(1|CNTSCHID),
                                 data = pisa.cnt[[c]],
                                 weights = std_wgt)))


# calculating country math icc
for(c in 1:length(unique(pisa$CNT))) {
  if(c == 1){
    icc.math <- NULL
  }
  icc.math.c <- NULL
  for(pv in 1:10) {
    icc.math.c <- rbind(icc.math.c, icc(fwmath[[c]][[pv]])$ICC_adjusted)
  }
  icc.math <- rbind(icc.math, mean(icc.math.c))
}


# deleting temporary data
rm(fwmath, icc.math.c)



## Sciences --------------------------------------------------------------------

# final weight null model
fwscie <- lapply(1:length(unique(pisa$CNT)),
                 function(c)
                   lapply(1:10,
                          function(pv)
                            lmer(pisa.cnt[[c]][,28+pv]~1+(1|CNTSCHID),
                                 data = pisa.cnt[[c]],
                                 weights = std_wgt)))

# calculating country science icc
for(c in 1:length(unique(pisa$CNT))) {
  if(c == 1){
    icc.scie <- NULL
  }
  icc.scie.c <- NULL
  for(pv in 1:10) {
    icc.scie.c <- rbind(icc.scie.c, icc(fwscie[[c]][[pv]])$ICC_adjusted)
  }
  icc.scie <- rbind(icc.scie, mean(icc.scie.c))
}


# deleting temporary data
rm(fwscie, icc.scie.c)


## Creating ICC data frame -----------------------------------------------------

# calculating mean performance points per country and subject area
mean.read <- pisa.mean.pv(pvlabel = "READ",
                          by = "CNT",
                          data = pisa) %>% 
  select(mean.read = Mean)


mean.math <- pisa.mean.pv(pvlabel = "MATH",
                          by = "CNT",
                          data = pisa) %>% 
  select(mean.math = Mean)


mean.scie <- pisa.mean.pv(pvlabel = "SCIE",
                          by = "CNT",
                          data = pisa) %>% 
  select(mean.scie = Mean)


# bringing all the information together
icc <- data.frame(CNT = unique(pisa$CNT),
                  icc.read = round(icc.read, 2),
                  mean.read = mean.read,
                  icc.math = round(icc.math,2),
                  mean.math = mean.math,
                  icc.scie = round(icc.scie,2),
                  mean.scie = mean.scie)


# checking correlation between icc and performance
ggcorrplot(cor(icc[,2:7]),
           p.mat = cor_pmat(icc[,2:7]),
           hc.order = T,
           lab = T,
           insig = "blank")

# icc and performance are only statistic correlated in reading
# the magnitude of the correlation is -0.47



# EXPORTING --------------------------------------------------------------------

export(icc, "Results/icc.csv")
