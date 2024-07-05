# SETUP ------------------------------------------------------------------------

# cleaning environment
rm(list=ls())
options(scipen=999)


# loading packages
if (!require("pacman")) install.packages("pacman")

p_load(tidyverse,
       janitor,
       rio)


# LOADING DATA -----------------------------------------------------------------

pisa.stu <- import("Raw Data/CY07_MSU_STU_QQQ.sav")
pisa.sch <- import("Raw Data/CY07_MSU_SCH_QQQ.sav")


# selecting variables
pisa.stu <- pisa.stu %>% 
  select(CNTRYID,
         CNT,
         CNTSCHID,
         CYC,
         NatCen,
         STRATUM,
         SUBNATIO,
         OECD,
         ADMINMODE,
         SEX = ST004D01T,
         GRADE = ST001D01T,
         ISCEDO,
         IMMIG,
         DISCLIMA,
         ESCS,
         PV1READ:PV10READ,
         PV1MATH:PV10MATH,
         PV1SCIE:PV10SCIE,
         W_FSTUWT,
         W_FSTURWT1:W_FSTURWT80)



pisa.sch <- pisa.sch %>% 
  select(CNTRYID,
         CNT,
         CNTSCHID,
         CYC,
         NatCen,
         STRATUM,
         SUBNATIO,
         OECD,
         ADMINMODE,
         PROATCE,
         SCHLTYPE)


# joining
pisa <- left_join(pisa.stu, pisa.sch,
                  by = c("CNTRYID", "CNT", "CNTSCHID", "CYC",
                         "NatCen", "STRATUM", "SUBNATIO", "OECD",
                         "ADMINMODE"))

# dropping irrelevant id variables
pisa <- pisa %>% 
  select(!c(CNTRYID, CYC, NatCen, STRATUM, SUBNATIO, ADMINMODE))


# SELECTING COUNTRIES ----------------------------------------------------------

# Latin America countries
LA.cnt <- c("ARG", "BRA", "CHL", "COL", "CRI", "DOM", "MEX", "PAN", "PER")

pisa <- pisa %>% 
  mutate(LA = ifelse(CNT %in% LA.cnt, 1, 0))

# selecting OECD and Latin America countries
pisa <- pisa %>% 
  filter(OECD == 1 | LA == 1)

# dropping irrelevant data
rm(pisa.sch, pisa.stu, LA.cnt)



# TREATING COUNTRIES -----------------------------------------------------------

# splitting sample by country
pisa.cnt <- lapply(1:length(unique(pisa$CNT)),
                   function(c)
                     pisa %>% 
                     filter(CNT == unique(pisa$CNT)[c]))
# 43 countries


# calculating school indicators in each country
for(c in 1:length(unique(pisa$CNT))) {
  pisa.cnt[[c]] <- pisa.cnt[[c]] %>% 
    group_by(CNTSCHID) %>% 
    mutate(mESCS = mean(ESCS, na.rm = T),
           mDISCLIMA = mean(DISCLIMA, na.rm = T)) %>% 
    ungroup()
}


# reshaping sample
for(c in 1:length(unique(pisa$CNT))){
  if(c == 1){
    pisa.agg <- NULL
  }
  pisa.agg <- rbind(pisa.agg, pisa.cnt[[c]])
  if(c == length(unique(pisa$CNT))) {
    pisa <- pisa.agg
    rm(pisa.agg)
  }
}


# checking NA in each country
pisa.cnt.drop <- lapply(1:length(unique(pisa$CNT)),
                        function(c)
                          pisa %>% 
                          filter(CNT == unique(pisa$CNT)[c]) %>% 
                          drop_na())


for(c in 1:length(unique(pisa$CNT))) {
  if(c == 1) {
    nsample <- NULL
    nsample.drop <- NULL
  }
  nsample <- rbind(nsample, nrow(pisa.cnt[[c]]))
  nsample.drop <- rbind(nsample.drop, nrow(pisa.cnt.drop[[c]]))
  if(c == length(unique(pisa$CNT))) {
    sample.reduc <- cbind(unique(pisa$CNT), nsample, nsample.drop) %>% 
      data.frame() %>% 
      rename(CNT = X1,
             sample = X2,
             sample.drop = X3) %>% 
      mutate(prop.info = round((as.numeric(sample.drop)/as.numeric(sample))*100,2),
             prop.drop = 100-prop.info)
  }
}

# sample.reduc contains original sample size, sample size after drop
# and proportions


# Creating country exclusion criteria

# countries with -30% missing info
sample.reduc %>% 
  filter(prop.drop < 30)
# 29 countries

# countries with less missing info than Brazil
sample.reduc %>% 
  filter(prop.drop <= 22.61)
# 22 countries

# countries with -20% missing info
sample.reduc %>% 
  filter(prop.drop < 20)
# 20 countries


# applying "less missing than brazil" criteria
cnt.na <- sample.reduc %>% 
  filter(prop.drop <= 22.61) %>% 
  dplyr::select(CNT)

# excludin other countries
pisa <- pisa %>% 
  filter(CNT %in% cnt.na$CNT)


# CHECKING INVARIABILITY -------------------------------------------------------

# splitting countries samples
pisa.cnt.drop <- lapply(1:length(unique(pisa$CNT)),
                        function(c)
                          pisa %>% 
                          filter(CNT == unique(pisa$CNT)[c]) %>% 
                          drop_na())

# saving independent variables names
names <- c("CNTSCHID",
           "DISCLIMA",
           "ESCS",
           "GRADE",
           "IMMIG",
           "ISCEDO",
           "PROATCE",
           "SCHLTYPE",
           "SEX",
           "mDISCLIMA",
           "mESCS")


# creating frequency tables
freqtab <- lapply(1:length(unique(pisa$CNT)),
                  function(c)
                    lapply(1:length(names),
                           function(var)
                             tabyl(pisa.cnt.drop[[c]], names[var])))


# looking for invariability
for(var in 1:length(names)) {
  if(var == 1){
    varcat.c <- NULL
  }
  varcat <- NULL
  for(c in 1:length(unique(pisa$CNT))){
    varcat <- rbind(varcat, nrow(freqtab[[c]][[var]]))
  }
  varcat.c <- cbind(varcat.c, varcat)
  if(var == length(names)) {
    colnames(varcat.c)<-names
    rm(varcat)
  }
}

varcat.c

# countries 7, 9, 15, 16 and 22 have only one value of ISCEDO or GRADE
# we choose not to drop them


# NORMALIZING VARIABLES --------------------------------------------------------

# splitting again
pisa.cnt <- lapply(1:length(unique(pisa$CNT)),
                   function(c)
                     pisa %>% filter(CNT == unique(pisa$CNT)[c]))

# normalizing sample weights for future modeling
for(c in 1:length(unique(pisa$CNT))) {
  pisa.cnt[[c]] <- pisa.cnt[[c]] %>% 
    mutate(n_wt = nrow(.),
           sum_wt = sum(W_FSTUWT),
           std_wgt = (W_FSTUWT*n_wt)/sum_wt)
}

# normalizing replicate weights for future modeling
for(c in 1:length(unique(pisa$CNT))) {
  if(c == 1){
    wt <- paste0("std_wgt_r", 1:80)
  }
  for(w in 1:80) {
    pisa.cnt[[c]][,wt[w]] <- (pisa.cnt[[c]][,32+w]*pisa.cnt[[c]]$n_wt)/
      pisa.cnt[[c]]$sum_wt
  }
  if(c == length(unique(pisa$CNT))) {
    rm(wt)
  }
}


# DROPPING MISSING AND EXPORTING -----------------------------------------------

# transforming into a dataframe
for(c in 1:length(unique(pisa$CNT))) {
  if(c == 1) {
    pisa.exp <- NULL
  }
  pisa.exp <- rbind(pisa.exp, pisa.cnt[[c]])
}


# excluding ID irrelevant variables
pisa.exp <- pisa.exp %>% 
  dplyr::select(!c(OECD, LA))


export(pisa.exp, "Data/pisa_oecd.csv")
