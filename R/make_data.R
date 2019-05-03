# Make data to be used in the Shiny app

library(tidyverse)

source("R/rename_abs.R")

quals <- c(
  "No qualification",
  "Cert III/IV",
  "Dip/Ad-dip",
  "Bachelor",
  "Postgrad"
)


# Whole population ------------------------------------------------
data <- read_csv("data/education_occ_nostudy.csv", skip = 10) %>% 
  select(-X8)

names(data) <- rename_abs(data)

data <- data %>% 
  select(-person_place) %>% 
left_join(read_csv("data/occ4_list.csv")) %>% 
  group_by(occ, age, sex) %>% 
  mutate(total = sum(n),
         pc = 100 * n / total) %>% 
group_by(occ) %>% 
  mutate(occ_total = sum(n)) %>% 
# Drop low n nfd occupations
  mutate(is_nfd = grepl(", nfd", occ)) %>% 
  filter(!(is_nfd && occ_total < 500)) %>% 
# Create factors
  mutate(qual = factor(qual, levels = quals)) %>% 
# Create observation_weight for alpha in chart
  mutate(observation_weight = (n/occ_total) / max(n/occ_total))



write_rds(data, "data/education_occ_nostudy.rds")




# Aus-citizens only; not studying ------------------------------------------------
data1 <- read_csv("data/education_occ_aus_nostudy.csv", skip = 10) %>% 
  select(-X9)

names(data1) <- rename_abs(data1)

data1 <- data1 %>% 
  select(-person_place,
         -citizen,
         -student_status) %>% 
  left_join(read_csv("data/occ4_list.csv")) %>% 
  group_by(occ, age, sex) %>% 
  mutate(total = sum(n),
         pc = 100 * n / total) %>% 
  group_by(occ) %>% 
  mutate(occ_total = sum(n)) %>% 
  # Drop low n nfd occupations
  mutate(is_nfd = grepl(", nfd", occ)) %>% 
  filter(!(is_nfd && occ_total < 500)) %>% 
  # Create factors
  mutate(qual = factor(qual, levels = quals)) %>% 
  # Create observation_weight for alpha in chart
  mutate(observation_weight = (n/occ_total) / max(n/occ_total))



write_rds(data1, "data/occ_education_aus.rds")



# Additional: occupations by their weighted 50%+ education

skill_classification <-
data %>% 
  select(occ, age, qual, occ_group, n) %>% 
  filter(!is.na(occ_group)) %>% 
  mutate(age_weight = case_when(
      age == "25-34" ~ 1.0,
      age == "35-44" ~ 0.5,
      age == "45-54" ~ 0.2,
      age == "55-64" ~ 0.1,
      age == "Over 60" ~ 0),
    nw = n * age_weight,
    qual = if_else(qual == "Postgrad", factor("Bachelor", levels = quals), qual)) %>% 
  group_by(occ_group, occ, qual) %>% 
  summarise(n = sum(nw, na.rm = T)) %>% 
  mutate(pc = 100 * n / sum(n))

sc_pc <- skill_classification %>% 
  select(-n) %>% 
  spread(key = qual, value = pc)

sc_n <- skill_classification %>% 
  select(-pc) %>% 
  spread(key = qual, value = n)


write_csv(sc_pc, "output/skill_classification.csv")
write_csv(sc_n, "output/skill_classification_n.csv")

  
