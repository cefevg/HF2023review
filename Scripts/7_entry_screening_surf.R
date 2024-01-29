library(tidyverse)

# load entry DMSOs and detect outliers

# merge dmsos and samples

dmsos <- outlier.data %>%
  filter(Well_outlier == "N") %>%
  mutate(SAMPLE_NAME = "DMSO") %>%
  select(RUN_NAME, PLATE_NAME, Inoculum, Surface, SAMPLE_NAME)

plate.data <- cmpds.data %>%
  select(RUN_NAME, PLATE_NAME, Surface, SAMPLE_NAME, Inoculum, RESPONSE) %>%
  bind_rows(dmsos)

# pval estimation: DMSO - cmpd surface

cmpd.activity <- plate.data %>%
  group_by(SAMPLE_NAME, Inoculum) %>%
  summarise(n = n(), act.filt = any(RESPONSE > 15))

cmpd.activity <- cmpd.activity %>%
  filter(SAMPLE_NAME != "DMSO" & act.filt) %>%
  mutate(pval = NA)

for(i in 1:nrow(cmpd.activity)) {
  
  cmpd <- plate.data %>%
    filter(Inoculum == cmpd.activity$Inoculum[i] & SAMPLE_NAME == cmpd.activity$SAMPLE_NAME[i]) 
  
  if(nrow(cmpd) <2) {next}
  
  if(sd(cmpd$Surface)==0) {
    cmpd$Surface <- cmpd$Surface + runif(nrow(cmpd), 0, 0.001)
  }
  
  toh.data <- plate.data %>%
    filter(Inoculum == cmpd$Inoculum[1] & SAMPLE_NAME == "DMSO" & RUN_NAME %in% cmpd$RUN_NAME & PLATE_NAME %in% cmpd$PLATE_NAME) %>%
    bind_rows(cmpd)
  
  cmpd.activity$pval[i] <- t.test(Surface ~ SAMPLE_NAME, mu = median(toh.data$Surface[toh.data$SAMPLE_NAME=="DMSO"])/2, 
         data = toh.data, alternative = "greater")$p.value
  
}
