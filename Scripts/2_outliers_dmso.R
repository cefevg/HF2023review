library(tidyverse)


###### data cleaning: #######

## outlier identification (wells) ##

outlier_detection <- function (rawdata_HFformat, plates_data, level) {
  
  clean.data <- rawdata_HFformat %>%
    left_join(plates_data) %>%
    mutate(Well_outlier = ifelse(abs(Surface-median.growth)>level*mad.growth, "Y", "N"))
  
  return(clean.data)
  
}

dmso.plates <- dmso.data %>%
  group_by(Inoculum, RUN_NAME, PLATE_NAME) %>%
  summarise(mean.growth = mean(Surface), median.growth = median(Surface),
            sd.growth = sd(Surface), mad.growth = mad(Surface)) %>%
  mutate(cv.growth = sd.growth/mean.growth)

outlier.data <- outlier_detection(dmso.data, dmso.plates, 5.2)

