# SETUP ------------------------------------------------------------------------

# cleaning environment
rm(list=ls())
options(scipen = 999)


# loading packages
if(!require("pacman")) install.packages("pacman")

p_load(tidyverse,
       janitor,
       rio,
       PNADcIBGE,
       gt,
       GDAtools,
       paletteer,
       Hmisc,
       modelsummary)


# LOADING DATA -----------------------------------------------------------------

pnad2018 <- read_pnadc("Raw Data/PNADC_012018.txt",
                       input = "Raw Data/input_PNADC_trimestral.txt")

# selecting variables
pnad2018 <- pnad2018 %>% 
  dplyr::select(birth.year = V20082,
                student = V3002,
                schl.type = V3002A,
                sex = V2007,
                race = V2010,
                weight = V1028,
                income = VD4020,
                UPA,
                Estrato,
                house = V1008)

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

# calculating house income per capita
income.df <- pnad2018 %>% 
  mutate(count = 1) %>% 
  group_by(UPA, Estrato, house) %>% 
  summarise(house.income = sum(income, na.rm = T),
            house.pop = sum(count)) %>% 
  mutate(house.income.pc = house.income/house.pop) %>% 
  dplyr::select(!c(house.income, house.pop)) %>% 
  ungroup()




pnad2018 <- left_join(pnad2018, income.df)


# filtering 15-years-old students
pnad2018 <- pnad2018 %>% 
  filter(birth.year == 2002,
         student == 1)


# dropping irrelevant variables
pnad2018 <- pnad2018 %>% 
  dplyr::select(!c(student, income, Estrato, UPA, house, birth.year))


# deleting temporary data
rm(income.df)


# recoding variables
pnad2018 <- pnad2018 %>% 
  mutate(schl.type = as.numeric(schl.type),
         schl.type = factor(schl.type,
                            labels = c("Private", "Public"),
                            levels = c(1,2)),
         sex = as.numeric(sex),
         sex = factor(sex,
                      labels = c("Man", "Woman"),
                      levels = c(1,2)),
         race = case_when(race == 1 ~ 1,
                          race == 2 ~ 2,
                          race == 3 ~ 1,
                          race == 4 ~ 2,
                          race == 5 ~ 2),
         race = factor(race,
                       labels = c("White", "Non-White"),
                       levels = c(1,2)))



# ANALYZING SCHOOL ATTENDANCE --------------------------------------------------

# overall school attendance
school.attendance <- wtable(pnad2018$schl.type,
                            weight = pnad2018$weight,
                            stat = "prop") %>% 
  data.frame() %>% 
  rename(schl.type = Var1,
         prop = Freq)

# creating table visualization
gt(school.attendance) %>% 
  tab_header(title = md("School Attendance per School Type")) %>% 
  cols_label(schl.type = "School Type",
             prop = "Attendance (%)") %>% 
  cols_align(align = "left",
             columns = schl.type)

# saving school attendance
export(school.attendance, "Results/school.attendance.csv")

# graphically
pnad2018 %>% 
  mutate(attendance = sum(weight)) %>% 
  ggplot(aes(attendance)) +
  geom_bar(aes(group = schl.type,
               fill = schl.type)) +
  labs(title = "School Attendance in Private and Public Schools") +
  guides(fill = guide_legend(title = "School Type")) +
  scale_x_continuous(name = "",
                     breaks = NULL) +
  scale_y_continuous(name = "",
                     breaks = NULL) +
  geom_text(label = "87.6%",
            y = 3600,
            color = "white") +
  geom_text(label = "12.4%",
            y = 7400) +
  scale_fill_paletteer_d("nationalparkcolors::Acadia")+
  coord_flip() +
  theme_bw()



# EXAMINING INCOME VARIATION IN SCHOOL ATTENDANCE ------------------------------

# creating house income sextiles
wq.income <- wtd.quantile(pnad2018$house.income.pc,
                          probs = seq(0,1,.2),
                          weights = pnad2018$weight)


# creating income classes
pnad2018 <- pnad2018 %>% 
  mutate(house.income.quintiles = cut(house.income.pc,
                                     breaks = wq.income,
                                     include.lowest = T,
                                     ordered_result = T))

# checking attendance per house income class
income.schl.attendance <- wtable(pnad2018$schl.type,
                                 pnad2018$house.income.quintiles,
                                 weights = pnad2018$weight,
                                 stat = "cprop") %>% 
  as.data.frame() %>% 
  rename("0-20%" = "[0,75]", 
         "20-40%" = "(75,300]", 
         "40-60%" = "(300,550]", 
         "60-80%" = "(550,1e+03]", 
         "80-100%" = "(1e+03,3.58e+04]")

# exporting
export(income.schl.attendance, "Results/3. Income School Attendance.csv")

# creating table visualization
income.schl.attendance %>% 
  rownames_to_column() %>% 
  rename("School Type" = "rowname") %>% 
  gt() %>% 
  tab_spanner(label = md("**House Income Per Capita**"),
              columns = c("0-20%",
                          "20-40%",
                          "40-60%",
                          "60-80%",
                          "80-100%")) %>% 
  tab_header(title = md("Attendance Distribution per
                        School Type and House Income (%)"))

# graphically
ggplot(pnad2018, aes(house.income.quintiles)) +
  geom_bar(aes(fill = schl.type),
           position = "fill") +
  theme_bw() +
  scale_y_continuous(name = "",
                     breaks = NULL) +
  scale_x_discrete(name = "Quintiles of House Income per Capita",
                   labels = c("[0,75]" = "0-20%", 
                              "(75,300]" = "20-40%", 
                              "(300,550]" = "40-60%", 
                              "(550,1e+03]" = "60-80%", 
                              "(1e+03,3.58e+04]" = "80-100%")) +
  labs(title = "Private and Public School Attendance x House Income") +
  guides(fill=guide_legend(title = "School Type")) +
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +
  annotate("text", x = 1, y = 1, label = "4.9%") +
  annotate("text", x = 2, y = 1, label = "3.1%") +
  annotate("text", x = 3, y = 1, label = "6.4%") +
  annotate("text", x = 4, y = 1, label = "12.1%") +
  annotate("text", x = 5, y = 1, label = "37.3%") +
  annotate("text", x = 1, y = 0.5, label = "95.1%", color = "white") +
  annotate("text", x = 2, y = 0.5, label = "96.9%", color = "white") +
  annotate("text", x = 3, y = 0.5, label = "93.6%", color = "white") +
  annotate("text", x = 4, y = 0.5, label = "87.9%", color = "white") +
  annotate("text", x = 5, y = 0.5, label = "62.7%", color = "white") +
  coord_flip()

## checking in a more disagreggated way

# excluding 0 income
pnad.vintile <- pnad2018 %>% 
  filter(house.income.pc != 0)

wvintile.income <- wtd.quantile(pnad.vintile$house.income.pc,
                                   probs = seq(0,1,.05),
                                   weights = pnad.vintile$weight)

pnad.vintile <- pnad.vintile %>% 
  mutate(house.income.vintile = cut(house.income.pc,
                                    breaks = wvintile.income,
                                    include.lowest = T,
                                    ordered_result = T))

ggplot(pnad.vintile, aes(house.income.vintile)) +
  geom_bar(aes(fill = schl.type),
           position = "fill") +
  theme_bw() +
  scale_y_continuous(name = "",
                     breaks = c(0.5,0.75,.9),
                     labels = c("50%", "75%", "90%")) +
  scale_x_discrete(name = "Quantiles of House Income per Capita",
                   breaks = NULL) +
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +
  guides(fill = guide_legend(title = "School Type")) +
  geom_hline(yintercept = 0.5, color = "red") +
  geom_hline(yintercept = 0.75, color = "red") +
  geom_hline(yintercept = 0.9, color = "red") +
  labs(title = "Private and Public School Attendance x House Income")


# deleting temporary data
rm(income.schl.attendance,
   pnad.vintile,
   wvintile.income,
   wq.income)


# EXAMINING RACE VARIATION IN SCHOOL ATTENDANCE --------------------------------

# race x school attendance
race.attendance <- wtable(pnad2018$race,
                          pnad2018$schl.type,
                          weights = pnad2018$weight,
                          stat = "cprop") %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  rename("Ethnics" = rowname)


# exporting
export(race.attendance, "Results/4. Ethnic School Attendance.csv")


# visualizing table
gt(race.attendance) %>% 
  tab_header(title = md("School Attendance per School Type and Ethnics"))


# graphically
pnad2018 %>% 
  drop_na(race) %>% 
  ggplot(aes(schl.type)) +
  geom_bar(aes(fill = race),
           position = "fill") +
  theme_bw() +
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +
  scale_x_discrete(name = "School Type") +
  scale_y_continuous(breaks = NULL,
                     name = "") +
  guides(fill=guide_legend(title = "Ethnics")) +
  annotate("text", x = 1, y = .9, label = "59.1%") +
  annotate("text", x = 2, y = .9, label = "35.3%") + 
  annotate("text", x = 1, y = .1, label = "40.9%", color = "white") +
  annotate("text", x = 2, y = .1, label = "64.7%", color = "white") + 
  labs(title = "School Attendance per School Type and Ethnics") +
  coord_flip()


# deleting temporary files
rm(race.attendance)

# EXAMINING SEX VARIATION IN SCHOOL ATTENDANCE ---------------------------------

# sex x school attendance
sex.attendance <- wtable(pnad2018$sex,
       pnad2018$schl.type,
       weights = pnad2018$weight,
       stat = "cprop") %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  rename("Sex" = "rowname")

# saving
export(sex.attendance, "Results/5. Sex School Attendance.csv")


# visualzing table
gt(sex.attendance) %>% 
  tab_header(title = md("School Attendance per Sex and School Type"))


# graphically
ggplot(pnad2018, aes(schl.type)) +
  geom_bar(aes(fill = sex),
           position = "fill") +
  theme_bw() +
  scale_y_continuous(breaks = NULL,
                     name = "") +
  guides(fill = guide_legend(title = "Gender")) +
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +
  annotate("text", x = 1, y = .9, label = "52.4%") +
  annotate("text", x = 2, y = .9, label = "52.0%") + 
  annotate("text", x = 1, y = .1, label = "47.6%", color = "white") +
  annotate("text", x = 2, y = .1, label = "48.0%", color = "white") +
  coord_flip() +
  labs(title = "School Attendance per Sex and School Type",
       x = "School Type")
