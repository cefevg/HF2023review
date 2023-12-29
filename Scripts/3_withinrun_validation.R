library(tidyverse)

# Experimental validation review

dmso.plates <- happy.data %>%
  filter(clean.data == "Y") %>%
  group_by(Inoculum, RUN_NAME, PLATE_NAME) %>%
  summarise(mean.growth = mean(Surface), median.growth = median(Surface),
            sd.growth = sd(Surface), mad.growth = mad(Surface)) %>%
  mutate(cv.growth = sd.growth/mean.growth)

dmso.plates %>%
  ggplot(aes(x = as.factor(PLATE_NAME), y = mean.growth)) +
  geom_point() +
  facet_wrap(~Inoculum, scales = "free") +
  theme_bw()

dmso.plates %>%
  ggplot(aes(x = as.factor(PLATE_NAME), y = cv.growth)) +
  geom_point() +
  facet_wrap(~Inoculum, scales = "free") +
  theme_bw()