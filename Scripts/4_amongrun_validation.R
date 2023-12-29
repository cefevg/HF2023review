happy.data %>%
  filter(clean.data == "Y") %>%
  group_by(Inoculum, RUN_NAME, PLATE_NAME) %>%
  summarise(Run_meand = mean(Surface)) %>%
  group_by(Inoculum, RUN_NAME) %>%
  summarise(Run_mean = mean(Run_meand), Run_sd = sd(Run_meand)) %>%
  ggplot(aes(x = as.factor(RUN_NAME), y = Run_mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=Run_mean-Run_sd, ymax=Run_mean+Run_sd)) +
  facet_wrap(~Inoculum, scales = "free") +
  theme_bw()
