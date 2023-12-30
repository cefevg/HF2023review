library(tidyverse)


###### data cleaning: #######

## outlier identification (wells) ##

outlier_detection <- function (rawdata_HFformat, plates_data) {
  
  clean.data <- rawdata_HFformat %>%
    left_join(plates_data) %>%
    mutate(Outlier = ifelse(abs(Surface-median.growth)>6*mad.growth, "Y", "N"))
  
  return(clean.data)
  
}

dmso.plates <- dmso.data %>%
  group_by(Inoculum, RUN_NAME, PLATE_NAME) %>%
  summarise(mean.growth = mean(Surface), median.growth = median(Surface),
            sd.growth = sd(Surface), mad.growth = mad(Surface)) %>%
  mutate(cv.growth = sd.growth/mean.growth)

outlier.data <- outlier_detection(dmso.data, dmso.plates)


## outlier identification (plates) ##

run.data <- dmso.data %>%
  group_by(Inoculum, RUN_NAME) %>%
  summarise(mean.run = mean(Surface), median.run = median(Surface),
            sd.run = sd(Surface), mad.run = mad(Surface)) %>%
  mutate(acceptrun.lowthres = mean.run-sd.run,
         acceptrun.highthres = mean.run+sd.run)

plate_outlier <- function (outlier.data, run.data) {
  
  clean_data <- outlier.data %>%
    filter(Outlier == "N") %>%
    group_by(Inoculum, RUN_NAME, PLATE_NAME) %>%
    summarise(sd.clean = sd(Surface),
              mean.clean = mean(Surface),
              plate.upconf = ifelse(sd.clean == 0, mean(Surface), t.test(Surface)$conf.int[2]), 
              plate.lowconf = ifelse(sd.clean == 0, mean(Surface), t.test(Surface)$conf.int[1]),
              plate.cv = sd.clean/mean.clean) %>%
    select(!c(sd.clean, mean.clean)) %>%
    left_join(run.data) %>%
    mutate(plate_meanoutlier = ifelse(plate.upconf > acceptrun.highthres | plate.lowconf < acceptrun.lowthres, "Y", "N"),
           plate_cvoutlier = ifelse(plate.cv > 0.2, "Y", "N"))
  
  return(clean_data)
  
}

out.plates <- plate_outlier(outlier.data, run.data) 

happy.data <- out.plates %>%
  right_join(outlier.data) %>%
  mutate(clean.data = ifelse(plate_meanoutlier=="Y" | Outlier == "Y" | plate_cvoutlier =="Y", "N", "Y"))
