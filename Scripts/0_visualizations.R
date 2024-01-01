# Visualizations

## DMSO

# Mean per plate and cv per plate with no well-outliers, then tagging plate-outliers

outlier.data %>%
  filter(Outlier=="N") %>%
  ggplot(aes(x = as.factor(PLATE_NAME), y = mean.growth)) +
  geom_point() +
  facet_wrap(~Inoculum, scales = "free") +
  theme_bw()

outlier.data %>%
  filter(Outlier=="N") %>%
  ggplot(aes(x = as.factor(PLATE_NAME), y = cv.growth)) +
  geom_point() +
  facet_wrap(~Inoculum, scales = "free") +
  theme_bw()

happy.data %>%
  group_by(Inoculum, RUN_NAME, PLATE_NAME) %>%
  summarise(mean.growth = mean(Surface), median.growth = median(Surface),
            sd.growth = sd(Surface), mad.growth = mad(Surface),
            outlier = ifelse(all(plate_meanoutlier=="Y"), "Mean_outlier",
                             ifelse(all(plate_cvoutlier=="Y"), "CV_outlier", "N"))) %>%
  mutate(cv.growth = sd.growth/mean.growth) %>%
  ggplot(aes(x = as.factor(PLATE_NAME), y = mean.growth, col = as.factor(outlier))) +
  geom_point() +
  facet_wrap(~Inoculum, scales = "free") +
  theme_bw()

# Mean per plate and cv per plate without outliers

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


## Refs

inoc <- "BOTRCI"

outlier.refsdata %>%
  filter(RefsOutlier=="N" & Inoculum == inoc) %>%
  ggplot(aes(x = as.factor(PLATE_NAME), y = mean.growth, col = as.factor(DOSE))) +
  geom_point() +
  facet_wrap(~SAMPLE_NAME, scales = "free") +
  theme_bw()

outlier.refsdata %>%
  filter(RefsOutlier=="N" & Inoculum == inoc) %>%
  group_by(RUN_NAME, PLATE_NAME, SAMPLE_NAME, DOSE) %>%
  summarise(mean.eff = mean(RESPONSE)) %>%
  ggplot(aes(x = as.factor(PLATE_NAME), y = mean.eff, col = as.factor(DOSE))) +
  geom_point() +
  facet_wrap(~SAMPLE_NAME, scales = "free") +
  theme_bw()

outlier.refsdata %>%
  filter(RefsOutlier=="N" & Inoculum == inoc) %>%
  ggplot(aes(x = as.factor(PLATE_NAME), y = cv.growth, col = as.factor(DOSE))) +
  geom_point() +
  facet_wrap(~SAMPLE_NAME, scales = "free") +
  theme_bw()

happy.refs %>%
  group_by(Inoculum, RUN_NAME, PLATE_NAME, SAMPLE_NAME, DOSE) %>%
  summarise(mean.growth = mean(Surface), median.growth = median(Surface),
            sd.growth = sd(Surface), mad.growth = mad(Surface),
            outlier = ifelse(all(platerefs_meanoutlier=="Y"), "Mean_outlier",
                             ifelse(all(platerefs_cvoutlier=="Y"), "CV_outlier", "N"))) %>%
  mutate(cv.growth = sd.growth/mean.growth) %>%
  filter(Inoculum == inoc) %>%
  ggplot(aes(x = as.factor(PLATE_NAME), y = mean.growth, col = as.factor(outlier))) +
  geom_point() +
  facet_wrap(~SAMPLE_NAME*DOSE, scales = "free") +
  theme_bw()

# Mean per plate and cv per plate without outliers

refs.plates %>%
  filter(Inoculum == inoc) %>%
  ggplot(aes(x = as.factor(PLATE_NAME), y = mean.growth, col = as.factor(DOSE))) +
  geom_point() +
  facet_wrap(~SAMPLE_NAME, scales = "free") +
  theme_bw()

refs.plates %>%
  filter(Inoculum == inoc) %>%
  ggplot(aes(x = as.factor(PLATE_NAME), y = cv.growth, col = as.factor(DOSE))) +
  geom_point() +
  facet_wrap(~SAMPLE_NAME, scales = "free") +
  theme_bw()
