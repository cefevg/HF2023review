library(readxl)
library(tidyverse)
library(writexl)

surf.p <- read_xlsx("C:/Users/GMCDP/OneDrive - Bayer/Desktop/Biology/Primary/Hit-Fisher CT/2021-23/results/cmpd_activity.xlsx")

comparison <- surf.p %>%
  left_join(efficacy.confint, by = c("SAMPLE_NAME", "Inoculum")) %>%
  select(SAMPLE_NAME, Inoculum, pval, p.eff, med.efficacy)

comparison %>%
  filter(p.eff < 0.1 & med.efficacy > 0 & Inoculum != "PHAKPA") %>%
  ggplot(aes(x = pval, y = p.eff, col = med.efficacy)) +
  geom_point() +
  facet_wrap(~Inoculum) +
  theme_bw()

write_xlsx(comparison, "C:/Users/GMCDP/OneDrive - Bayer/Desktop/Biology/Primary/Hit-Fisher CT/2021-23/results/cmpd_results.xlsx")
