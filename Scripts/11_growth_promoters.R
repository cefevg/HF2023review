library(readxl)
library(tidyverse)
library(writexl)

# load entry DMSOs and detect outliers

# estimate efficacies

dmsos <- outlier.data %>%
  filter(Well_outlier == "N") %>%
  mutate(SAMPLE_NAME = "DMSO") %>%
  select(RUN_ID, PLATE_NAME, median.growth, Inoculum)

plate.data <- cmpds.data %>%
  select(RUN_ID, PLATE_NAME, Surface, RESPONSE, SAMPLE_NAME, Inoculum) %>%
  left_join(dmsos) %>%
  mutate(Efficacy = 100*(median.growth - Surface)/median.growth)

plate.data %>%
  filter(Efficacy > 0) %>%
  ggplot(aes(x = RESPONSE, y = Efficacy)) +
  geom_point()

growth.proms <- plate.data %>%
  group_by(Inoculum, SAMPLE_NAME) %>%
  summarise(n = n(), med.efficacy = median(RESPONSE),
            sd.eff = sd(RESPONSE),
            conf.int = ifelse(sd.eff == 0, med.efficacy, t.test(RESPONSE, mu = 0, alternative = "less", conf.level = 0.99)$conf.int[2])) %>%
  filter(conf.int < 0)

write_xlsx(growth.proms, "C:/Users/GMCDP/OneDrive - Bayer/Desktop/Biology/Primary/Hit-Fisher CT/2021-23/results/growth_promoters.xlsx")

growth.proms %>%
  filter(Inoculum != "PHAKPA") %>%
  ggplot(aes(x = med.efficacy)) +
  geom_density() +
  facet_wrap(~Inoculum) +
  theme_bw()

growth.proms %>%
  group_by(Inoculum) %>%
  summarise(min = min(med.efficacy))