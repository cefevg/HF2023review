library(tidyverse)


# dmso model with plate + run

cleaned.data <- happy.data %>%
  filter(clean.data == "Y")

#summary(aov(Surface ~ PLATE_NAME*RUN_NAME, data = cleaned.data[cleaned.data$Inoculum=="BOTRCI",]))

cleaned.data <- cleaned.data %>%
  mutate(Date = as.Date(ifelse(substr(RUN_NAME, 3, 3) == "-", 
                               substr(RUN_NAME, 4, 9),
                               substr(RUN_NAME, 3, 8)), format = "%y%m%d"))

cleaned.data %>%
  filter(Inoculum != "PHAKPA") %>%
  group_by(Inoculum, RUN_NAME, PLATE_NAME, Date) %>%
  summarise(Run_meand = mean(Surface)) %>%
  group_by(Inoculum, RUN_NAME, Date) %>%
  summarise(Run_mean = mean(Run_meand), Run_sd = sd(Run_meand)) %>%
  ggplot(aes(x = Date, y = Run_mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=Run_mean-Run_sd, ymax=Run_mean+Run_sd)) +
  geom_vline(xintercept = as.Date("2023-09-18"), linetype = "dashed", col = "red") +
  facet_wrap(~Inoculum, scales = "free") +
  theme_bw()

happy.data %>%
  filter(clean.data == "Y" & Inoculum != "PHAKPA") %>%
  group_by(Inoculum, RUN_NAME, PLATE_NAME) %>%
  summarise(Run_meand = mean(Surface)) %>%
  group_by(Inoculum, RUN_NAME) %>%
  summarise(Run_mean = mean(Run_meand), Run_sd = sd(Run_meand)) %>%
  ggplot(aes(x = as.factor(RUN_NAME), y = Run_mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=Run_mean-Run_sd, ymax=Run_mean+Run_sd)) +
  facet_wrap(~Inoculum, scales = "free") +
  theme_bw()
