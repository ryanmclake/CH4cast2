
# TRAP 1
# parameter uncertainty
ebu_rate_forecast_t1_parameter_unc <- ebu_rate_model_t1 %>%
  group_by(time, variable)%>%
  mutate(forecast_ebu_rate_t1 = (rnorm(1, ebu_beta2_t1_mean, ebu_beta2_t1_sd)*ebu_latent_t1_mean) +
           (rnorm(1, ebu_beta3_t1_mean, ebu_beta3_t1_sd)*mean(scaled_temp_t1)) +
           rnorm(1, ebu_beta1_t1_mean, ebu_beta1_t1_sd) + ebu_sigma_t1_mean)%>%
  filter(time >= "2019-07-15")

# Summarize to daily
ebu_rate_forecast_t1_summarize_param_unc <- ebu_rate_forecast_t1_parameter_unc %>%
  group_by(time) %>% 
  filter(forecast_ebu_rate_t1 < quantile(forecast_ebu_rate_t1, 0.75, na.rm = T)) %>% 
  filter(forecast_ebu_rate_t1 > quantile(forecast_ebu_rate_t1, 0.25, na.rm = T)) %>%
  summarise(mean = mean(forecast_ebu_rate_t1),
            upper = quantile(forecast_ebu_rate_t1, 0.75),
            lower = quantile(forecast_ebu_rate_t1, 0.25),
            vairiance = var(forecast_ebu_rate_t1),.groups = "drop")%>%
  mutate(trap_id = "Trap 1")

# driver data uncertainty 
ebu_rate_forecast_t1_drive_unc <- ebu_rate_model_t1 %>%
  group_by(time, variable)%>%
  mutate(forecast_ebu_rate_t1 = (ebu_beta2_t1_mean*ebu_latent_t1_mean) +
           (ebu_beta3_t1_mean*scaled_temp_t1) +
           ebu_beta1_t1_mean + ebu_sigma_t1_mean) %>%
  filter(time >= "2019-07-15")

# Summarize to daily
ebu_rate_forecast_t1_summarize_drive_unc <- ebu_rate_forecast_t1_drive_unc %>%
  group_by(time) %>% 
  filter(forecast_ebu_rate_t1 < quantile(forecast_ebu_rate_t1, 0.75, na.rm = T)) %>% 
  filter(forecast_ebu_rate_t1 > quantile(forecast_ebu_rate_t1, 0.25, na.rm = T)) %>%
  summarise(mean = mean(forecast_ebu_rate_t1),
            upper = quantile(forecast_ebu_rate_t1, 0.75),
            lower = quantile(forecast_ebu_rate_t1, 0.25),
            vairiance = var(forecast_ebu_rate_t1),.groups = "drop")%>%
  mutate(trap_id = "Trap 1")

# model process uncertainty
ebu_rate_forecast_t1_proc_unc <- ebu_rate_model_t1 %>%
  group_by(time, variable)%>%
  mutate(forecast_ebu_rate_t1 = (ebu_beta2_t1_mean*ebu_latent_t1_mean) +
           (ebu_beta3_t1_mean*mean(scaled_temp_t1)) +
           ebu_beta1_t1_mean + rnorm(1, ebu_sigma_t1_mean, ebu_sigma_t1_sd))%>%
  filter(time >= "2019-07-15")
# Summarize to daily
ebu_rate_forecast_t1_summarize_proc_unc <- ebu_rate_forecast_t1_proc_unc %>%
  group_by(time) %>% 
  filter(forecast_ebu_rate_t1 < quantile(forecast_ebu_rate_t1, 0.75, na.rm = T)) %>% 
  filter(forecast_ebu_rate_t1 > quantile(forecast_ebu_rate_t1, 0.25, na.rm = T)) %>%
  summarise(mean = mean(forecast_ebu_rate_t1),
            upper = quantile(forecast_ebu_rate_t1, 0.75),
            lower = quantile(forecast_ebu_rate_t1, 0.25),
            vairiance = var(forecast_ebu_rate_t1),.groups = "drop")%>%
  mutate(trap_id = "Trap 1")

# initial conditions uncertainty
ebu_rate_forecast_init_unc <- ebu_rate_model_t1 %>%
  group_by(time, variable)%>%
  mutate(forecast_ebu_rate_t1 = (ebu_beta2_t1_mean*rnorm(1, ebu_latent_t1_mean, ebu_latent_t1_sd)) +
           (ebu_beta3_t1_mean*mean(scaled_temp_t1)) +
           ebu_beta1_t1_mean + ebu_sigma_t1_mean)%>%
  filter(time >= "2019-07-15")
# Summarize to daily
ebu_rate_forecast_t1_summarize_init_unc <- ebu_rate_forecast_init_unc %>%
  group_by(time) %>% 
  filter(forecast_ebu_rate_t1 < quantile(forecast_ebu_rate_t1, 0.75, na.rm = T)) %>% 
  filter(forecast_ebu_rate_t1 > quantile(forecast_ebu_rate_t1, 0.25, na.rm = T)) %>%
  summarise(mean = mean(forecast_ebu_rate_t1),
            upper = quantile(forecast_ebu_rate_t1, 0.75),
            lower = quantile(forecast_ebu_rate_t1, 0.25),
            vairiance = var(forecast_ebu_rate_t1),.groups = "drop")%>%
  mutate(trap_id = "Trap 1")

#TRAP 2
# parameter uncertainty
ebu_rate_forecast_t2_parameter_unc <- ebu_rate_model_t2 %>%
  group_by(time, variable)%>%
  mutate(forecast_ebu_rate_t2 = (rnorm(1, ebu_beta2_t2_mean, ebu_beta2_t2_sd)*ebu_latent_t2_mean) +
           (rnorm(1, ebu_beta3_t2_mean, ebu_beta3_t2_sd)*mean(scaled_temp_t2)) +
           rnorm(1, ebu_beta1_t2_mean, ebu_beta1_t2_sd) + ebu_sigma_t2_mean)%>%
  filter(time >= "2019-07-15")
# Summarize to daily
ebu_rate_forecast_t2_summarize_param_unc <- ebu_rate_forecast_t2_parameter_unc %>%
  group_by(time) %>% 
  filter(forecast_ebu_rate_t2 < quantile(forecast_ebu_rate_t2, 0.75, na.rm = T)) %>% 
  filter(forecast_ebu_rate_t2 > quantile(forecast_ebu_rate_t2, 0.25, na.rm = T)) %>%
  summarise(mean = mean(forecast_ebu_rate_t2),
            upper = quantile(forecast_ebu_rate_t2, 0.75),
            lower = quantile(forecast_ebu_rate_t2, 0.25),
            vairiance = var(forecast_ebu_rate_t2),.groups = "drop")%>%
  mutate(trap_id = "Trap 2")

# driver data uncertainty 
ebu_rate_forecast_t2_drive_unc <- ebu_rate_model_t2 %>%
  group_by(time, variable)%>%
  mutate(forecast_ebu_rate_t2 = (ebu_beta2_t2_mean*ebu_latent_t2_mean) +
           (ebu_beta3_t2_mean*scaled_temp_t2) +
           ebu_beta1_t2_mean + ebu_sigma_t2_mean) %>%
  filter(time >= "2019-07-15")
# Summarize to daily
ebu_rate_forecast_t2_summarize_drive_unc <- ebu_rate_forecast_t2_drive_unc %>%
  group_by(time) %>% 
  filter(forecast_ebu_rate_t2 < quantile(forecast_ebu_rate_t2, 0.75, na.rm = T)) %>% 
  filter(forecast_ebu_rate_t2 > quantile(forecast_ebu_rate_t2, 0.25, na.rm = T)) %>%
  summarise(mean = mean(forecast_ebu_rate_t2),
            upper = quantile(forecast_ebu_rate_t2, 0.75),
            lower = quantile(forecast_ebu_rate_t2, 0.25),
            vairiance = var(forecast_ebu_rate_t2),.groups = "drop")%>%
  mutate(trap_id = "Trap 2")

# model process uncertainty
ebu_rate_forecast_t2_proc_unc <- ebu_rate_model_t2 %>%
  group_by(time, variable)%>%
  mutate(forecast_ebu_rate_t2 = (ebu_beta2_t2_mean*ebu_latent_t2_mean) +
           (ebu_beta3_t2_mean*mean(scaled_temp_t2)) +
           ebu_beta1_t2_mean + rnorm(1, ebu_sigma_t2_mean, ebu_sigma_t2_sd))%>%
  filter(time >= "2019-07-15")
# Summarize to daily
ebu_rate_forecast_t2_summarize_proc_unc <- ebu_rate_forecast_t2_proc_unc %>%
  group_by(time) %>% 
  filter(forecast_ebu_rate_t2 < quantile(forecast_ebu_rate_t2, 0.75, na.rm = T)) %>% 
  filter(forecast_ebu_rate_t2 > quantile(forecast_ebu_rate_t2, 0.25, na.rm = T)) %>%
  summarise(mean = mean(forecast_ebu_rate_t2),
            upper = quantile(forecast_ebu_rate_t2, 0.75),
            lower = quantile(forecast_ebu_rate_t2, 0.25),
            vairiance = var(forecast_ebu_rate_t2),.groups = "drop")%>%
  mutate(trap_id = "Trap 2")

# initial conditions uncertainty
ebu_rate_forecast_init_unc <- ebu_rate_model_t2 %>%
  group_by(time, variable)%>%
  mutate(forecast_ebu_rate_t2 = (ebu_beta2_t2_mean*rnorm(1, ebu_latent_t2_mean, ebu_latent_t2_sd)) +
           (ebu_beta3_t2_mean*mean(scaled_temp_t2)) +
           ebu_beta1_t2_mean + ebu_sigma_t2_mean)%>%
  filter(time >= "2019-07-15")
# Summarize to daily
ebu_rate_forecast_t2_summarize_init_unc <- ebu_rate_forecast_init_unc %>%
  group_by(time) %>% 
  filter(forecast_ebu_rate_t2 < quantile(forecast_ebu_rate_t2, 0.75, na.rm = T)) %>% 
  filter(forecast_ebu_rate_t2 > quantile(forecast_ebu_rate_t2, 0.25, na.rm = T)) %>%
  summarise(mean = mean(forecast_ebu_rate_t2),
            upper = quantile(forecast_ebu_rate_t2, 0.75),
            lower = quantile(forecast_ebu_rate_t2, 0.25),
            vairiance = var(forecast_ebu_rate_t2),.groups = "drop")%>%
  mutate(trap_id = "Trap 2")


#TRAP 3

# parameter uncertainty
ebu_rate_forecast_t3_parameter_unc <- ebu_rate_model_t3 %>%
  group_by(time, variable)%>%
  mutate(forecast_ebu_rate_t3 = (rnorm(1, ebu_beta2_t3_mean, ebu_beta2_t3_sd)*ebu_latent_t3_mean) +
           (rnorm(1, ebu_beta3_t3_mean, ebu_beta3_t3_sd)*mean(scaled_temp_t3)) +
           rnorm(1, ebu_beta1_t3_mean, ebu_beta1_t3_sd) + ebu_sigma_t3_mean)%>%
  filter(time >= "2019-07-15")

# Summarize to daily
ebu_rate_forecast_t3_summarize_param_unc <- ebu_rate_forecast_t3_parameter_unc %>%
  group_by(time) %>% 
  filter(forecast_ebu_rate_t3 < quantile(forecast_ebu_rate_t3, 0.75, na.rm = T)) %>% 
  filter(forecast_ebu_rate_t3 > quantile(forecast_ebu_rate_t3, 0.25, na.rm = T)) %>%
  summarise(mean = mean(forecast_ebu_rate_t3),
            upper = quantile(forecast_ebu_rate_t3, 0.75),
            lower = quantile(forecast_ebu_rate_t3, 0.25),
            vairiance = var(forecast_ebu_rate_t3),.groups = "drop")%>%
  mutate(trap_id = "Trap 3")

# driver data uncertainty 
ebu_rate_forecast_t3_drive_unc <- ebu_rate_model_t3 %>%
  group_by(time, variable)%>%
  mutate(forecast_ebu_rate_t3 = (ebu_beta2_t3_mean*ebu_latent_t3_mean) +
           (ebu_beta3_t3_mean*scaled_temp_t3) +
           ebu_beta1_t3_mean + ebu_sigma_t3_mean) %>%
  filter(time >= "2019-07-15")
# Summarize to daily
ebu_rate_forecast_t3_summarize_drive_unc <- ebu_rate_forecast_t3_drive_unc %>%
  group_by(time) %>% 
  filter(forecast_ebu_rate_t3 < quantile(forecast_ebu_rate_t3, 0.75, na.rm = T)) %>% 
  filter(forecast_ebu_rate_t3 > quantile(forecast_ebu_rate_t3, 0.25, na.rm = T)) %>%
  summarise(mean = mean(forecast_ebu_rate_t3),
            upper = quantile(forecast_ebu_rate_t3, 0.75),
            lower = quantile(forecast_ebu_rate_t3, 0.25),
            vairiance = var(forecast_ebu_rate_t3),.groups = "drop")%>%
  mutate(trap_id = "Trap 3")

# model process uncertainty
ebu_rate_forecast_t3_proc_unc <- ebu_rate_model_t3 %>%
  group_by(time, variable)%>%
  mutate(forecast_ebu_rate_t3 = (ebu_beta2_t3_mean*ebu_latent_t3_mean) +
           (ebu_beta3_t3_mean*mean(scaled_temp_t3)) +
           ebu_beta1_t3_mean + rnorm(1, ebu_sigma_t3_mean, ebu_sigma_t3_sd))%>%
  filter(time >= "2019-07-15")
# Summarize to daily
ebu_rate_forecast_t3_summarize_proc_unc <- ebu_rate_forecast_t3_proc_unc %>%
  group_by(time) %>% 
  filter(forecast_ebu_rate_t3 < quantile(forecast_ebu_rate_t3, 0.75, na.rm = T)) %>% 
  filter(forecast_ebu_rate_t3 > quantile(forecast_ebu_rate_t3, 0.25, na.rm = T)) %>%
  summarise(mean = mean(forecast_ebu_rate_t3),
            upper = quantile(forecast_ebu_rate_t3, 0.75),
            lower = quantile(forecast_ebu_rate_t3, 0.25),
            vairiance = var(forecast_ebu_rate_t3),.groups = "drop")%>%
  mutate(trap_id = "Trap 3")

# initial conditions uncertainty
ebu_rate_forecast_init_unc <- ebu_rate_model_t3 %>%
  group_by(time, variable)%>%
  mutate(forecast_ebu_rate_t3 = (ebu_beta2_t3_mean*rnorm(1, ebu_latent_t3_mean, ebu_latent_t3_sd)) +
           (ebu_beta3_t3_mean*mean(scaled_temp_t3)) +
           ebu_beta1_t3_mean + ebu_sigma_t3_mean)%>%
  filter(time >= "2019-07-15")
# Summarize to daily
ebu_rate_forecast_t3_summarize_init_unc <- ebu_rate_forecast_init_unc %>%
  group_by(time) %>% 
  filter(forecast_ebu_rate_t3 < quantile(forecast_ebu_rate_t3, 0.75, na.rm = T)) %>% 
  filter(forecast_ebu_rate_t3 > quantile(forecast_ebu_rate_t3, 0.25, na.rm = T)) %>%
  summarise(mean = mean(forecast_ebu_rate_t3),
            upper = quantile(forecast_ebu_rate_t3, 0.75),
            lower = quantile(forecast_ebu_rate_t3, 0.25),
            vairiance = var(forecast_ebu_rate_t3),.groups = "drop")%>%
  mutate(trap_id = "Trap 3")


#TRAP 4
# parameter uncertainty
ebu_rate_forecast_t4_parameter_unc <- ebu_rate_model_t4 %>%
  group_by(time, variable)%>%
  mutate(forecast_ebu_rate_t4 = (rnorm(1, ebu_beta2_t4_mean, ebu_beta2_t4_sd)*ebu_latent_t4_mean) +
           (rnorm(1, ebu_beta3_t4_mean, ebu_beta3_t4_sd)*mean(scaled_temp_t4)) +
           rnorm(1, ebu_beta1_t4_mean, ebu_beta1_t4_sd) + ebu_sigma_t4_mean)%>%
  filter(time >= "2019-07-15")

# Summarize to daily
ebu_rate_forecast_t4_summarize_param_unc <- ebu_rate_forecast_t4_parameter_unc %>%
  group_by(time) %>% 
  filter(forecast_ebu_rate_t4 < quantile(forecast_ebu_rate_t4, 0.75, na.rm = T)) %>% 
  filter(forecast_ebu_rate_t4 > quantile(forecast_ebu_rate_t4, 0.25, na.rm = T)) %>%
  summarise(mean = mean(forecast_ebu_rate_t4),
            upper = quantile(forecast_ebu_rate_t4, 0.75),
            lower = quantile(forecast_ebu_rate_t4, 0.25),
            vairiance = var(forecast_ebu_rate_t4),.groups = "drop")%>%
  mutate(trap_id = "Trap 4")

# driver data uncertainty 
ebu_rate_forecast_t4_drive_unc <- ebu_rate_model_t4 %>%
  group_by(time, variable)%>%
  mutate(forecast_ebu_rate_t4 = (ebu_beta2_t4_mean*ebu_latent_t4_mean) +
           (ebu_beta3_t4_mean*scaled_temp_t4) +
           ebu_beta1_t4_mean + ebu_sigma_t4_mean) %>%
  filter(time >= "2019-07-15")
# Summarize to daily
ebu_rate_forecast_t4_summarize_drive_unc <- ebu_rate_forecast_t4_drive_unc %>%
  group_by(time) %>% 
  filter(forecast_ebu_rate_t4 < quantile(forecast_ebu_rate_t4, 0.75, na.rm = T)) %>% 
  filter(forecast_ebu_rate_t4 > quantile(forecast_ebu_rate_t4, 0.25, na.rm = T)) %>%
  summarise(mean = mean(forecast_ebu_rate_t4),
            upper = quantile(forecast_ebu_rate_t4, 0.75),
            lower = quantile(forecast_ebu_rate_t4, 0.25),
            vairiance = var(forecast_ebu_rate_t4),.groups = "drop")%>%
  mutate(trap_id = "Trap 4")

# model process uncertainty
ebu_rate_forecast_t4_proc_unc <- ebu_rate_model_t4 %>%
  group_by(time, variable)%>%
  mutate(forecast_ebu_rate_t4 = (ebu_beta2_t4_mean*ebu_latent_t4_mean) +
           (ebu_beta3_t4_mean*mean(scaled_temp_t4)) +
           ebu_beta1_t4_mean + rnorm(1, ebu_sigma_t4_mean, ebu_sigma_t4_sd))%>%
  filter(time >= "2019-07-15")
# Summarize to daily
ebu_rate_forecast_t4_summarize_proc_unc <- ebu_rate_forecast_t4_proc_unc %>%
  group_by(time) %>% 
  filter(forecast_ebu_rate_t4 < quantile(forecast_ebu_rate_t4, 0.75, na.rm = T)) %>% 
  filter(forecast_ebu_rate_t4 > quantile(forecast_ebu_rate_t4, 0.25, na.rm = T)) %>%
  summarise(mean = mean(forecast_ebu_rate_t4),
            upper = quantile(forecast_ebu_rate_t4, 0.75),
            lower = quantile(forecast_ebu_rate_t4, 0.25),
            vairiance = var(forecast_ebu_rate_t4),.groups = "drop")%>%
  mutate(trap_id = "Trap 4")

# initial conditions uncertainty
ebu_rate_forecast_init_unc <- ebu_rate_model_t4 %>%
  group_by(time, variable)%>%
  mutate(forecast_ebu_rate_t4 = (ebu_beta2_t4_mean*rnorm(1, ebu_latent_t4_mean, ebu_latent_t4_sd)) +
           (ebu_beta3_t4_mean*mean(scaled_temp_t4)) +
           ebu_beta1_t4_mean + ebu_sigma_t4_mean)%>%
  filter(time >= "2019-07-15")
# Summarize to daily
ebu_rate_forecast_t4_summarize_init_unc <- ebu_rate_forecast_init_unc %>%
  group_by(time) %>% 
  filter(forecast_ebu_rate_t4 < quantile(forecast_ebu_rate_t4, 0.75, na.rm = T)) %>% 
  filter(forecast_ebu_rate_t4 > quantile(forecast_ebu_rate_t4, 0.25, na.rm = T)) %>%
  summarise(mean = mean(forecast_ebu_rate_t4),
            upper = quantile(forecast_ebu_rate_t4, 0.75),
            lower = quantile(forecast_ebu_rate_t4, 0.25),
            vairiance = var(forecast_ebu_rate_t4),.groups = "drop")%>%
  mutate(trap_id = "Trap 4")

#All TRAPS averaged
# parameter uncertainty

ebu_rate_forecast_alltraps_parameter_unc <- ebu_rate_model_alltraps %>%
  group_by(time, variable)%>%
  mutate(forecast_ebu_rate_alltrap = (rnorm(1, ebu_beta2_alltrap_mean, ebu_beta2_alltrap_sd)*ebu_latent_alltrap_mean) +
           (rnorm(1, ebu_beta3_alltrap_mean, ebu_beta3_alltrap_sd)*mean(scaled_temp_alltraps)) +
           rnorm(1, ebu_beta1_alltrap_mean, ebu_beta1_alltrap_sd) + ebu_sigma_alltrap_mean)%>%
  filter(time >= "2019-07-15")

# Summarize to daily
ebu_rate_forecast_alltraps_summarize_param_unc <- ebu_rate_forecast_alltraps_parameter_unc %>%
  group_by(time) %>% 
  filter(forecast_ebu_rate_alltrap < quantile(forecast_ebu_rate_alltrap, 0.75, na.rm = T)) %>% 
  filter(forecast_ebu_rate_alltrap > quantile(forecast_ebu_rate_alltrap, 0.25, na.rm = T)) %>%
  summarise(mean = mean(forecast_ebu_rate_alltrap),
            upper = quantile(forecast_ebu_rate_alltrap, 0.75),
            lower = quantile(forecast_ebu_rate_alltrap, 0.25),
            vairiance = var(forecast_ebu_rate_alltrap),.groups = "drop")%>%
  mutate(trap_id = "All Traps")

# driver data uncertainty 
ebu_rate_forecast_alltraps_drive_unc <- ebu_rate_model_alltraps %>%
  group_by(time, variable)%>%
  mutate(forecast_ebu_rate_alltrap = (ebu_beta2_alltrap_mean*ebu_latent_alltrap_mean) +
           (ebu_beta3_alltrap_mean*scaled_temp_alltraps) +
           ebu_beta1_alltrap_mean + ebu_sigma_alltrap_mean) %>%
  filter(time >= "2019-07-15")
# Summarize to daily
ebu_rate_forecast_alltraps_summarize_drive_unc <- ebu_rate_forecast_alltraps_drive_unc %>%
  group_by(time) %>% 
  filter(forecast_ebu_rate_alltrap < quantile(forecast_ebu_rate_alltrap, 0.75, na.rm = T)) %>% 
  filter(forecast_ebu_rate_alltrap > quantile(forecast_ebu_rate_alltrap, 0.25, na.rm = T)) %>%
  summarise(mean = mean(forecast_ebu_rate_alltrap),
            upper = quantile(forecast_ebu_rate_alltrap, 0.75),
            lower = quantile(forecast_ebu_rate_alltrap, 0.25),
            vairiance = var(forecast_ebu_rate_alltrap),.groups = "drop")%>%
  mutate(trap_id = "Trap 4")

# model process uncertainty
ebu_rate_forecast_alltraps_proc_unc <- ebu_rate_model_alltraps %>%
  group_by(time, variable)%>%
  mutate(forecast_ebu_rate_alltrap = (ebu_beta2_alltrap_mean*ebu_latent_alltrap_mean) +
           (ebu_beta3_alltrap_mean*mean(scaled_temp_alltraps)) +
           ebu_beta1_alltrap_mean + rnorm(1, ebu_sigma_alltrap_mean, ebu_sigma_alltrap_sd))%>%
  filter(time >= "2019-07-15")
# Summarize to daily
ebu_rate_forecast_alltraps_summarize_proc_unc <- ebu_rate_forecast_alltraps_proc_unc %>%
  group_by(time) %>% 
  filter(forecast_ebu_rate_alltrap < quantile(forecast_ebu_rate_alltrap, 0.75, na.rm = T)) %>% 
  filter(forecast_ebu_rate_alltrap > quantile(forecast_ebu_rate_alltrap, 0.25, na.rm = T)) %>%
  summarise(mean = mean(forecast_ebu_rate_alltrap),
            upper = quantile(forecast_ebu_rate_alltrap, 0.75),
            lower = quantile(forecast_ebu_rate_alltrap, 0.25),
            vairiance = var(forecast_ebu_rate_alltrap),.groups = "drop")%>%
  mutate(trap_id = "Trap 4")

# initial conditions uncertainty
ebu_rate_forecast_init_unc <- ebu_rate_model_alltraps %>%
  group_by(time, variable)%>%
  mutate(forecast_ebu_rate_alltrap = (ebu_beta2_alltrap_mean*rnorm(1, ebu_latent_alltrap_mean, ebu_latent_alltrap_sd)) +
           (ebu_beta3_alltrap_mean*mean(scaled_temp_alltraps)) +
           ebu_beta1_alltrap_mean + ebu_sigma_alltrap_mean)%>%
  filter(time >= "2019-07-15")
# Summarize to daily
ebu_rate_forecast_alltraps_summarize_init_unc <- ebu_rate_forecast_init_unc %>%
  group_by(time) %>% 
  filter(forecast_ebu_rate_alltrap < quantile(forecast_ebu_rate_alltrap, 0.75, na.rm = T)) %>% 
  filter(forecast_ebu_rate_alltrap > quantile(forecast_ebu_rate_alltrap, 0.25, na.rm = T)) %>%
  summarise(mean = mean(forecast_ebu_rate_alltrap),
            upper = quantile(forecast_ebu_rate_alltrap, 0.75),
            lower = quantile(forecast_ebu_rate_alltrap, 0.25),
            vairiance = var(forecast_ebu_rate_alltrap),.groups = "drop")%>%
  mutate(trap_id = "All Traps")

summarized_trap_forecasts_init_unc <- rbind(ebu_rate_forecast_t1_summarize_init_unc,
                                            ebu_rate_forecast_t2_summarize_init_unc,
                                            ebu_rate_forecast_t3_summarize_init_unc,
                                            ebu_rate_forecast_t4_summarize_init_unc,
                                            ebu_rate_forecast_alltraps_summarize_init_unc)

summarized_trap_forecasts_parameter_unc <- rbind(ebu_rate_forecast_t1_summarize_param_unc,
                                                 ebu_rate_forecast_t2_summarize_param_unc,
                                                 ebu_rate_forecast_t3_summarize_param_unc,
                                                 ebu_rate_forecast_t4_summarize_param_unc,
                                                 ebu_rate_forecast_alltraps_summarize_param_unc)

summarized_trap_forecasts_drive_unc <- rbind(ebu_rate_forecast_t1_summarize_drive_unc,
                                             ebu_rate_forecast_t2_summarize_drive_unc,
                                             ebu_rate_forecast_t3_summarize_drive_unc,
                                             ebu_rate_forecast_t4_summarize_drive_unc,
                                             ebu_rate_forecast_alltraps_summarize_drive_unc)

summarized_trap_forecasts_proc_unc <- rbind(ebu_rate_forecast_t1_summarize_proc_unc,
                                            ebu_rate_forecast_t2_summarize_proc_unc,
                                            ebu_rate_forecast_t3_summarize_proc_unc,
                                            ebu_rate_forecast_t4_summarize_proc_unc,
                                            ebu_rate_forecast_alltraps_summarize_proc_unc)


trap1_uncertatiny_partition <- left_join(ebu_rate_forecast_t1_summarize_init_unc[,c(1,5)],
                                         ebu_rate_forecast_t1_summarize_proc_unc[,c(1,5)], by = "time") %>%
  left_join(., ebu_rate_forecast_t1_summarize_drive_unc[,c(1,5)], by = "time")%>%
  left_join(., ebu_rate_forecast_t1_summarize_param_unc[,c(1,5)], by = "time")%>%
  mutate(trap_id = "Trap 1")%>%
  mutate(sum_var = vairiance.x+vairiance.y+vairiance.x.x+vairiance.y.y)%>%
  mutate(paramter_unc = vairiance.y.y/sum_var)%>%
  mutate(driver_unc = vairiance.x.x/sum_var)%>%
  mutate(process_unc = vairiance.y/sum_var)%>%
  mutate(init_unc = vairiance.x/sum_var) %>%
  mutate(sum = paramter_unc+driver_unc+process_unc+init_unc)%>%
  select(time, trap_id, paramter_unc,driver_unc,process_unc,init_unc, sum)

trap2_uncertatiny_partition <- left_join(ebu_rate_forecast_t2_summarize_init_unc[,c(1,5)],
                                         ebu_rate_forecast_t2_summarize_proc_unc[,c(1,5)], by = "time") %>%
  left_join(., ebu_rate_forecast_t2_summarize_drive_unc[,c(1,5)], by = "time")%>%
  left_join(., ebu_rate_forecast_t2_summarize_param_unc[,c(1,5)], by = "time")%>%
  mutate(trap_id = "Trap 2")%>%
  mutate(sum_var = vairiance.x+vairiance.y+vairiance.x.x+vairiance.y.y)%>%
  mutate(paramter_unc = vairiance.y.y/sum_var)%>%
  mutate(driver_unc = vairiance.x.x/sum_var)%>%
  mutate(process_unc = vairiance.y/sum_var)%>%
  mutate(init_unc = vairiance.x/sum_var) %>%
  mutate(sum = paramter_unc+driver_unc+process_unc+init_unc)%>%
  select(time, trap_id, paramter_unc,driver_unc,process_unc,init_unc, sum)

trap3_uncertatiny_partition <- left_join(ebu_rate_forecast_t3_summarize_init_unc[,c(1,5,6)],
                                         ebu_rate_forecast_t3_summarize_proc_unc[,c(1,5,6)], by = "time") %>%
  left_join(., ebu_rate_forecast_t3_summarize_drive_unc[,c(1,5,6)], by = "time")%>%
  left_join(., ebu_rate_forecast_t3_summarize_param_unc[,c(1,5,6)], by = "time") %>%
  mutate(trap_id = "Trap 3")%>%
  mutate(sum_var = vairiance.x+vairiance.y+vairiance.x.x+vairiance.y.y)%>%
  mutate(paramter_unc = vairiance.y.y/sum_var)%>%
  mutate(driver_unc = vairiance.x.x/sum_var)%>%
  mutate(process_unc = vairiance.y/sum_var)%>%
  mutate(init_unc = vairiance.x/sum_var) %>%
  mutate(sum = paramter_unc+driver_unc+process_unc+init_unc)%>%
  select(time, trap_id, paramter_unc,driver_unc,process_unc,init_unc, sum)

trap4_uncertatiny_partition <- left_join(ebu_rate_forecast_t4_summarize_init_unc[,c(1,5,6)],
                                         ebu_rate_forecast_t4_summarize_proc_unc[,c(1,5,6)], by = "time") %>%
  left_join(., ebu_rate_forecast_t4_summarize_drive_unc[,c(1,5,6)], by = "time")%>%
  left_join(., ebu_rate_forecast_t4_summarize_param_unc[,c(1,5,6)], by = "time")%>%
  mutate(trap_id = "Trap 4")%>%
  mutate(sum_var = vairiance.x+vairiance.y+vairiance.x.x+vairiance.y.y)%>%
  mutate(paramter_unc = vairiance.y.y/sum_var)%>%
  mutate(driver_unc = vairiance.x.x/sum_var)%>%
  mutate(process_unc = vairiance.y/sum_var)%>%
  mutate(init_unc = vairiance.x/sum_var) %>%
  mutate(sum = paramter_unc+driver_unc+process_unc+init_unc)%>%
  select(time, trap_id, paramter_unc,driver_unc,process_unc,init_unc, sum)

alltraps_uncertatiny_partition <- left_join(ebu_rate_forecast_alltraps_summarize_init_unc[,c(1,5,6)],
                                         ebu_rate_forecast_alltraps_summarize_proc_unc[,c(1,5,6)], by = "time") %>%
  left_join(., ebu_rate_forecast_alltraps_summarize_drive_unc[,c(1,5,6)], by = "time")%>%
  left_join(., ebu_rate_forecast_alltraps_summarize_param_unc[,c(1,5,6)], by = "time")%>%
  mutate(trap_id = "All Traps")%>%
  mutate(sum_var = vairiance.x+vairiance.y+vairiance.x.x+vairiance.y.y)%>%
  mutate(paramter_unc = vairiance.y.y/sum_var)%>%
  mutate(driver_unc = vairiance.x.x/sum_var)%>%
  mutate(process_unc = vairiance.y/sum_var)%>%
  mutate(init_unc = vairiance.x/sum_var) %>%
  mutate(sum = paramter_unc+driver_unc+process_unc+init_unc)%>%
  select(time, trap_id, paramter_unc,driver_unc,process_unc,init_unc, sum)


uncertatiny_partition_all <- rbind(trap1_uncertatiny_partition,
                                   trap2_uncertatiny_partition,
                                   trap3_uncertatiny_partition,
                                   trap4_uncertatiny_partition,
                                   alltraps_uncertatiny_partition)

uncertatiny_partition_all <- uncertatiny_partition_all[-7]
uncertatiny_partition_all <- melt(uncertatiny_partition_all, id = c("time", "trap_id"))
library(viridis)

# plot the partitioned uncertainty among all four traps
ggplot(uncertatiny_partition_all, aes(x = time, y = value, fill = variable)) + 
  geom_area(position = 'stack', color = "black")+
  xlab("")+
  ylab(c("Proportion of total variance"))+
  scale_fill_viridis(discrete = TRUE, option = "C")+
  theme_bw()+
  theme(legend.position="top")+
  scale_y_continuous(limits = c(0,1.001), expand = c(0,0))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key = element_blank(),legend.background = element_blank(),
        legend.key.size = unit(1,"cm"),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(color="black", size=15),
        axis.title.y = element_text(color="black", size=15),
        axis.text = element_text(color="black", size = 15), 
        axis.text.x = element_text(angle = 45, hjust = 1.2, vjust = 1.2),
        legend.title = element_blank())+
  facet_wrap(~trap_id)
