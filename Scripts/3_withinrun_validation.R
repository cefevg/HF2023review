library(tidyverse)

# Experimental validation review


dmso.plates <- happy.data %>%
  filter(clean.data == "Y") %>%
  group_by(Inoculum, RUN_NAME, PLATE_NAME) %>%
  summarise(mean.growth = mean(Surface), median.growth = median(Surface),
            sd.growth = sd(Surface), mad.growth = mad(Surface)) %>%
  mutate(cv.growth = sd.growth/mean.growth)
