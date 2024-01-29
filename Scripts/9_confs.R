library(tidyverse)
library(drc)
library(writexl)

# Run drc

results.conf <- conf.data %>%
  group_by(Inoculum, SAMPLE_NAME) %>%
  summarise(Doses = n(), act.filt = any(RESPONSE > 50),
            fully.act = any(RESPONSE > 85)) %>%
  filter(act.filt) %>%
  mutate(ED50 = NA, L95_ED50 = NA, U95_ED50 = NA)

for (i in 1:nrow(results.conf)) {
  
  drc.data <- conf.data %>%
    filter(Inoculum == results.conf$Inoculum[i] & SAMPLE_NAME == results.conf$SAMPLE_NAME[i])
  
  dosecurve <- drm(RESPONSE ~ DOSE, data = drc.data, fct = LL.4())
  
  eds <- ED(dosecurve, 50, interval = "delta", type = "absolute", display = F)
  
  results.conf$ED50[i] <- eds[1]
  results.conf$L95_ED50[i] <- eds[3]
  results.conf$U95_ED50[i] <- eds[4]
  
}

write_xlsx(results.conf,"C:/Users/GMCDP/OneDrive - Bayer/Desktop/Biology/Primary/Hit-Fisher CT/2021-23/results/drcs.xlsx")
