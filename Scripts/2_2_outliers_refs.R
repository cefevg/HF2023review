library(tidyverse)


###### data cleaning: #######

## outlier identification (wells) ##

outlier_detection <- function (rawdata_HFformat, plates_data, level) {
  
  clean.data <- rawdata_HFformat %>%
    left_join(plates_data) %>%
    mutate(Well_refsoutlier = ifelse(abs(Surface-median.growth)>level*mad.growth, "Y", "N"))
  
  return(clean.data)
  
}

refs.plates <- refs.data %>%
  group_by(Inoculum, RUN_NAME, PLATE_NAME, SAMPLE_NAME, DOSE) %>%
  summarise(mean.growth = mean(Surface), median.growth = median(Surface),
            sd.growth = sd(Surface), mad.growth = mad(Surface)) %>%
  mutate(cv.growth = sd.growth/mean.growth)

outlier.refsdata <- outlier_detection(refs.data, refs.plates, 5.2)
