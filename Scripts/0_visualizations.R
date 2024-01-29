# Visualizations

## DMSO

# Well outliers example

outlier.data %>%
  filter(Inoculum == "BOTRCI" & RUN_NAME == "IM230703-3-CONF") %>%
  ggplot(aes(x = as.factor(PLATE_NAME), y = Surface, col = Well_outlier)) +
  geom_point() +
  scale_y_continuous(limits = c(0, 0.5)) +
  theme_bw()

outlier_detection(dmso.data, dmso.plates, 7) %>%
  ggplot(aes(x = Well_outlier)) +
  geom_bar()

table(outlier_detection(dmso.data, dmso.plates, 7)$Well_outlier)

outlier_detection(dmso.data, dmso.plates, 10) %>%
  group_by(Inoculum) %>%
  summarise(ratio = sum(Well_outlier=="Y")/n())

# Plate outliers

outlier.data %>%
  filter(Inoculum == "BOTRCI" & RUN_NAME == "IM230703-3-CONF" & Well_outlier == "N") %>%
  ggplot(aes(x = as.factor(PLATE_NAME), y = Surface)) +
  geom_boxplot() +
  geom_hline(yintercept = run.data$acceptrun.highthres[run.data$RUN_NAME=="IM230703-3-CONF" & run.data$Inoculum=="BOTRCI"], linetype = "dashed") +
  geom_hline(yintercept = run.data$acceptrun.lowthres[run.data$RUN_NAME=="IM230703-3-CONF" & run.data$Inoculum=="BOTRCI"], linetype = "dashed") +
  geom_hline(yintercept = run.data$mean.run[run.data$RUN_NAME=="IM230703-3-CONF" & run.data$Inoculum=="BOTRCI"]) +
  scale_y_continuous(limits = c(0.15, 0.45)) +
  theme_bw()

out.plates %>%
  filter(Inoculum == "FUSACU" & RUN_NAME == "IM221024-7-ENT") %>%
  ggplot(aes(x = as.factor(PLATE_NAME), y = mean.clean)) +
  geom_point() +
  geom_errorbar(aes(ymin = plate.lowconf, ymax = plate.upconf)) +
  geom_hline(yintercept = run.data$acceptrun.highthres[run.data$RUN_NAME=="IM221024-7-ENT" & run.data$Inoculum=="FUSACU"], linetype = "dashed") +
  geom_hline(yintercept = run.data$acceptrun.lowthres[run.data$RUN_NAME=="IM221024-7-ENT" & run.data$Inoculum=="FUSACU"], linetype = "dashed") +
  geom_hline(yintercept = run.data$mean.run[run.data$RUN_NAME=="IM221024-7-ENT" & run.data$Inoculum=="FUSACU"]) +
  geom_hline(yintercept = run.data$cv.highthres[run.data$RUN_NAME=="IM221024-7-ENT" & run.data$Inoculum=="FUSACU"], linetype = "3313") +
  geom_hline(yintercept = run.data$cv.lowthres[run.data$RUN_NAME=="IM221024-7-ENT" & run.data$Inoculum=="FUSACU"], linetype = "3313") +
  theme_bw()

out.plates %>%
  filter(Inoculum == "FUSACU" & RUN_NAME == "IM230717-2-ENT") %>%
  ggplot(aes(x = as.factor(PLATE_NAME), y = plate.cv)) +
  geom_point() +
  geom_hline(yintercept = 0.2) +
  theme_bw()

table(out.plates$plate_meanoutlier)


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


## Standards

# Well outliers example

outlier.refsdata %>%
  filter(Inoculum == "BOTRCI" & RUN_NAME == "IM230703-3-CONF" & SAMPLE_NAME == "Cyprodinil" & DOSE == 2) %>%
  ggplot(aes(x = as.factor(PLATE_NAME), y = Surface, col = Well_refsoutlier)) +
  geom_point() +
  scale_y_continuous(limits = c(0, 0.5)) +
  theme_bw() 

outlier_detection(refs.data,refs.plates, 7) %>%
  ggplot(aes(x = Well_refsoutlier)) +
  geom_bar()

# Plate outliers

outlier.refsdata %>%
  filter(Inoculum == "BOTRCI" & RUN_NAME == "IM230703-3-CONF" & Well_refsoutlier == "N" & SAMPLE_NAME == "Cyprodinil" & DOSE == 2) %>%
  ggplot(aes(x = as.factor(PLATE_NAME), y = Surface)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 0.45)) +
  facet_wrap(~SAMPLE_NAME*DOSE) +
  theme_bw()

outlier.refsdata %>%
  filter(Inoculum == "BOTRCI" & RUN_NAME == "IM230703-3-CONF" & Well_refsoutlier == "N" & SAMPLE_NAME == "HAMBRA" & DOSE == 1.25) %>%
  ggplot(aes(x = as.factor(PLATE_NAME), y = Surface)) +
  geom_boxplot() +
  geom_hline(yintercept = run.refsdata$acceptrun.highthres[run.refsdata$RUN_NAME=="IM230703-3-CONF" & run.refsdata$Inoculum=="BOTRCI" & run.refsdata$SAMPLE_NAME == "HAMBRA" & run.refsdata$DOSE == 1.25], linetype = "dashed") +
  geom_hline(yintercept = run.refsdata$acceptrun.lowthres[run.refsdata$RUN_NAME=="IM230703-3-CONF" & run.refsdata$Inoculum=="BOTRCI" & run.refsdata$SAMPLE_NAME == "HAMBRA" & run.refsdata$DOSE == 1.25], linetype = "dashed") +
  geom_hline(yintercept = run.refsdata$mean.run[run.refsdata$RUN_NAME=="IM230703-3-CONF" & run.refsdata$Inoculum=="BOTRCI" & run.refsdata$SAMPLE_NAME == "HAMBRA" & run.refsdata$DOSE == 1.25]) +
  scale_y_continuous(limits = c(0, 0.45)) +
  theme_bw()


out.refsplates %>%
  filter(Inoculum == "FUSACU" & RUN_NAME == "IM230717-2-ENT") %>%
  ggplot(aes(x = as.factor(PLATE_NAME), y = platerefs.cv)) +
  geom_point() +
  geom_hline(yintercept = 0.2) +
  facet_wrap(~SAMPLE_NAME*DOSE) +
  theme_bw()
