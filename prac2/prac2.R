pacman::p_load(tidyverse, tidymodels, vip)

rbc0 <- readxl::read_xlsx('./data/rbc_clean.xlsx')
rbc0

inspectdf::inspect_na(rbc0)

colnames(rbc0)[1] <- 'id'
rbc0

rbc0$fitness <- as.double(rbc0$fitness)
inspectdf::inspect_na(rbc0)
rbc1 <- rbc0 %>% filter(complete.cases(.))

rbc1

rbc1 <- rbc1 %>%
  filter(fitness <= 100, RBC > 0, country <= 3)

rbc1 <- mutate(rbc1, sex=ifelse(rbc1$sex=='male', 'M', rbc1$sex))
rbc1

sex_c <- rbc1 %>%
  count(sex) %>%
  arrange(-n)
sex_c

country_c <- rbc1 %>%
  count(country) %>%
  arrange(-n)
country_c

inspectdf::inspect_num(rbc1)

