library(tidyverse)
library(readxl)

# Negative controls (DMSO)

# get data

files <- list.files("C:/Users/GMCDP/OneDrive - Bayer/Desktop/Biology/Primary/Hit-Fisher CT/2021-23/data/DMSO", pattern = "\\.xlsx$", full.names = T)

dmso.data <- lapply(files, read_xlsx)

names(dmso.data) <- list.files("C:/Users/GMCDP/OneDrive - Bayer/Desktop/Biology/Primary/Hit-Fisher CT/2021-23/data/DMSO", pattern = "\\.xlsx$")

# select only dmso data and merge

dmso.data <- lapply(dmso.data, function(x) x[x$WELL_TYPE_CODE=="DMSO",])

dmso.data <- do.call(rbind, dmso.data)

# show summary statistics of dmso per plate

dmso.plates <- dmso.data %>%
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

###### data cleaning: #######

## outlier identification (wells) ##

outlier_detection <- function (rawdata_HFformat, plates_data) {
  
  clean.data <- rawdata_HFformat %>%
    left_join(plates_data) %>%
    mutate(Outlier = ifelse(abs(Surface-median.growth)>6*mad.growth, "Y", "N"))
  
  return(clean.data)
  
}

outlier.data <- outlier_detection(dmso.data, dmso.plates)

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

## outlier identification (plates) ##

run.data <- dmso.data %>%
  group_by(Inoculum, RUN_NAME) %>%
  summarise(mean.growth = mean(Surface), median.growth = median(Surface),
            sd.growth = sd(Surface), mad.growth = mad(Surface)) %>%
  mutate(cv.growth = sd.growth/mean.growth)

plate_outlier <- function (plate.data, run.data) {
  
  clean_data <- plate.data %>%
  
}
