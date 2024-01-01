library(tidyverse)

# Experimental validation review


## outlier identification (plates) ##

run.refsdata <- refs.data %>%
  group_by(Inoculum, RUN_NAME, SAMPLE_NAME, DOSE) %>%
  summarise(mean.run = mean(Surface), median.run = median(Surface),
            sd.run = sd(Surface), mad.run = mad(Surface)) %>%
  mutate(acceptrun.lowthres = mean.run-sd.run,
         acceptrun.highthres = mean.run+sd.run)

plate_refsoutlier <- function (outlier.refsdata, run.refsdata) {
  
  clean_data <- outlier.refsdata %>%
    filter(RefsOutlier == "N") %>%
    group_by(Inoculum, RUN_NAME, PLATE_NAME, SAMPLE_NAME, DOSE) %>%
    summarise(sd.clean = sd(Surface),
              mean.clean = mean(Surface),
              platerefs.upconf = ifelse(sd.clean == 0, mean(Surface), t.test(Surface)$conf.int[2]), 
              platerefs.lowconf = ifelse(sd.clean == 0, mean(Surface), t.test(Surface)$conf.int[1]),
              platerefs.cv = sd.clean/mean.clean) %>%
    select(!c(sd.clean, mean.clean)) %>%
    left_join(run.refsdata) %>%
    mutate(platerefs_meanoutlier = ifelse(platerefs.upconf > acceptrun.highthres | platerefs.lowconf < acceptrun.lowthres, "Y", "N"),
           platerefs_cvoutlier = ifelse(platerefs.cv > 0.2, "Y", "N"))
  
  return(clean_data)
  
}

out.refsplates <- plate_refsoutlier(outlier.refsdata, run.refsdata) 

happy.refs <- out.refsplates %>%
  right_join(outlier.refsdata) %>%
  mutate(clean.refs = ifelse(platerefs_meanoutlier=="Y" | RefsOutlier == "Y" | platerefs_cvoutlier =="Y", "N", "Y"))

refs.plates <- happy.refs %>%
  filter(clean.refs == "Y") %>%
  group_by(Inoculum, RUN_NAME, PLATE_NAME, SAMPLE_NAME, DOSE) %>%
  summarise(mean.growth = mean(Surface), median.growth = median(Surface),
            sd.growth = sd(Surface), mad.growth = mad(Surface)) %>%
  mutate(cv.growth = sd.growth/mean.growth)
