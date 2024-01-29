library(tidyverse)

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

efficacy.confint <- plate.data %>%
  group_by(Inoculum, SAMPLE_NAME) %>%
  summarise(n = n(), med.efficacy = median(RESPONSE),
            sd.eff = sd(RESPONSE),
            conf.int = ifelse(sd.eff == 0, med.efficacy, t.test(RESPONSE, mu = 50, alternative = "greater", conf.level = 0.8)$conf.int[1]))

recap <- efficacy.confint %>%
  group_by(Inoculum) %>%
  summarise(total = n(),
            actives = sum(conf.int > 50),
            ratio = 100*actives/total)

recap2 <- efficacy.confint %>%
  group_by(SAMPLE_NAME) %>%
  summarise(actives = sum(conf.int > 50))

sum(recap2$actives>3)
