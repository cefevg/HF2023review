library(tidyverse)


# dmso model with plate + run

cleaned.refs <- happy.refs %>%
  filter(clean.refs == "Y")

summary(aov(Surface ~ PLATE_NAME*RUN_NAME*SAMPLE_NAME*DOSE, data = cleaned.refs[cleaned.refs$Inoculum=="BOTRCI",]))

cleaned.refs <- cleaned.refs %>%
  mutate(Date = as.Date(ifelse(substr(RUN_NAME, 3, 3) == "-", 
                               substr(RUN_NAME, 4, 9),
                               substr(RUN_NAME, 3, 8)), format = "%y%m%d"))

cleaned.refs %>%
  filter(Inoculum != "PHAKPA") %>%
  group_by(Inoculum, RUN_NAME, PLATE_NAME, SAMPLE_NAME, DOSE, Date) %>%
  summarise(Run_meand = mean(Surface)) %>%
  group_by(Inoculum, RUN_NAME, SAMPLE_NAME, DOSE, Date) %>%
  summarise(Run_mean = mean(Run_meand), Run_sd = sd(Run_meand)) %>%
  filter(Inoculum == "SEPTTR" & SAMPLE_NAME == "Cyprodinil" & DOSE == 0.5) %>%
  ggplot(aes(x = Date, y = Run_mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=Run_mean-Run_sd, ymax=Run_mean+Run_sd)) +
  geom_vline(xintercept = as.Date("2023-09-18"), linetype = "dashed", col = "red") +
  facet_wrap(~Inoculum*SAMPLE_NAME*DOSE, scales = "free") +
  theme_bw()

happy.refs %>%
  filter(clean.refs == "Y" & Inoculum != "PHAKPA") %>%
  group_by(Inoculum, RUN_NAME, PLATE_NAME, SAMPLE_NAME, DOSE)%>%
  summarise(Run_meand = mean(Surface)) %>%
  group_by(Inoculum, RUN_NAME, SAMPLE_NAME, DOSE)%>%
  summarise(Run_mean = mean(Run_meand), Run_sd = sd(Run_meand)) %>%
  ggplot(aes(x = as.factor(RUN_NAME), y = Run_mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=Run_mean-Run_sd, ymax=Run_mean+Run_sd)) +
  facet_wrap(~Inoculum, scales = "free") +
  facet_wrap(~Inoculum*SAMPLE_NAME*DOSE, scales = "free") +
  theme_bw()
