library(tidyverse)

# Experimental validation review


## outlier identification (plates) ##

run.data <- dmso.data %>%
  group_by(Inoculum, RUN_NAME) %>%
  summarise(mean.run = mean(Surface), median.run = median(Surface),
            sd.run = sd(Surface), mad.run = mad(Surface), cv.run = sd.run/mean.run) %>%
  mutate(acceptrun.lowthres = mean.run-sd.run,
         acceptrun.highthres = mean.run+sd.run,
         cv.highthres = mean.run+cv.run/5,
         cv.lowthres = mean.run-cv.run/5)

plate_outlier <- function (outlier.data, run.data) {
  
  clean_data <- outlier.data %>%
    filter(Well_outlier == "N") %>%
    group_by(Inoculum, RUN_NAME, PLATE_NAME) %>%
    summarise(sd.clean = sd(Surface),
              mean.clean = mean(Surface),
              plate.upconf = ifelse(sd.clean == 0, mean(Surface), t.test(Surface)$conf.int[2]), 
              plate.lowconf = ifelse(sd.clean == 0, mean(Surface), t.test(Surface)$conf.int[1]),
              plate.cv = sd.clean/mean.clean) %>%
    left_join(run.data) %>%
    mutate(plate_meanoutlier = ifelse(plate.upconf < acceptrun.lowthres | plate.lowconf > acceptrun.highthres, "Y", "N"),
           plate_cvoutlier = ifelse(plate.cv > 0.2, "Y", "N"))
  
  return(clean_data)
  
}

out.plates <- plate_outlier(outlier.data, run.data) 

happy.data <- out.plates %>%
  select(!c(sd.clean, mean.clean)) %>%
  right_join(outlier.data) %>%
  mutate(clean.data = ifelse(plate_meanoutlier=="Y" | Well_outlier == "Y" | plate_cvoutlier =="Y", "N", "Y"))

dmso.plates <- happy.data %>%
  filter(clean.data == "Y") %>%
  group_by(Inoculum, RUN_NAME, PLATE_NAME) %>%
  summarise(mean.growth = mean(Surface), median.growth = median(Surface),
            sd.growth = sd(Surface), mad.growth = mad(Surface)) %>%
  mutate(cv.growth = sd.growth/mean.growth)
