
# Compile data together to execute the forecast! 
###########################################################################################################
ebu_lag_t1 <- full_ebullition_model %>% filter(trap_id == "t1eb1")%>%
  select(time, log_ebu_rate, log_ebu_rate_sd)%>%
  fill(log_ebu_rate)%>%
  fill(log_ebu_rate_sd)

ebu_rate_model_t1 <- left_join(temp_scale_model_t1[,c(1,2,10)], ebu_beta1_all[1:3], by = "time")%>%
  left_join(., ebu_beta2_all[1:3], by = "time")%>%
  left_join(., ebu_beta3_all[1:3], by = "time")%>%
  left_join(., ebu_sigma_all[1:3], by = "time")%>%
  left_join(., ebu_latent_all[1:3], by = "time")%>%
  left_join(., ebu_lag_t1, by = "time")

ebu_rate_model_t1 <- ebu_rate_model_t1 %>% mutate(ebu_latent_t1_mean = ifelse(is.na(ebu_latent_t1_mean), log_ebu_rate, ebu_latent_t1_mean))%>%
  mutate(ebu_latent_t1_sd = ifelse(is.na(ebu_latent_t1_sd), log_ebu_rate_sd, ebu_latent_t1_sd))


ebu_lag_t2 <- full_ebullition_model %>% filter(trap_id == "t1eb2")%>%
  select(time, log_ebu_rate, log_ebu_rate_sd)%>%
  fill(log_ebu_rate)%>%
  fill(log_ebu_rate_sd)

ebu_rate_model_t2 <- left_join(temp_scale_model_t2[,c(1,2,10)], ebu_beta1_all[,c(1,4,5)], by = "time")%>%
  left_join(., ebu_beta2_all[,c(1,4,5)], by = "time")%>%
  left_join(., ebu_beta3_all[,c(1,4,5)], by = "time")%>%
  left_join(., ebu_sigma_all[,c(1,4,5)], by = "time")%>%
  left_join(., ebu_latent_all[,c(1,4,5)], by = "time")%>%
  left_join(., ebu_lag_t2, by = "time")

ebu_rate_model_t2 <- ebu_rate_model_t2 %>% mutate(ebu_latent_t2_mean = ifelse(is.na(ebu_latent_t2_mean), log_ebu_rate, ebu_latent_t2_mean))%>%
  mutate(ebu_latent_t2_sd = ifelse(is.na(ebu_latent_t2_sd), log_ebu_rate_sd, ebu_latent_t2_sd))



ebu_lag_t3 <- full_ebullition_model %>% filter(trap_id == "t1eb3")%>%
  select(time, log_ebu_rate, log_ebu_rate_sd)%>%
  fill(log_ebu_rate)%>%
  fill(log_ebu_rate_sd)

ebu_rate_model_t3 <- left_join(temp_scale_model_t3[,c(1,2,10)], ebu_beta1_all[,c(1,6,7)], by = "time")%>%
  left_join(., ebu_beta2_all[,c(1,6,7)], by = "time")%>%
  left_join(., ebu_beta3_all[,c(1,6,7)], by = "time")%>%
  left_join(., ebu_sigma_all[,c(1,6,7)], by = "time")%>%
  left_join(., ebu_latent_all[,c(1,6,7)], by = "time")%>%
    left_join(., ebu_lag_t3, by = "time")

ebu_rate_model_t3 <- ebu_rate_model_t3 %>% mutate(ebu_latent_t3_mean = ifelse(is.na(ebu_latent_t3_mean), log_ebu_rate, ebu_latent_t3_mean))%>%
  mutate(ebu_latent_t3_sd = ifelse(is.na(ebu_latent_t3_sd), log_ebu_rate_sd, ebu_latent_t3_sd))


ebu_lag_t4 <- full_ebullition_model %>% filter(trap_id == "t1eb4")%>%
  select(time, log_ebu_rate, log_ebu_rate_sd)%>%
  fill(log_ebu_rate)%>%
  fill(log_ebu_rate_sd)

ebu_rate_model_t4 <- left_join(temp_scale_model_t4[,c(1,2,10)], ebu_beta1_all[,c(1,8,9)], by = "time")%>%
  left_join(., ebu_beta2_all[,c(1,8,9)], by = "time")%>%
  left_join(., ebu_beta3_all[,c(1,8,9)], by = "time")%>%
  left_join(., ebu_sigma_all[,c(1,8,9)], by = "time")%>%
  left_join(., ebu_latent_all[,c(1,8,9)], by = "time")%>%
  left_join(., ebu_lag_t4, by = "time")

ebu_rate_model_t4 <- ebu_rate_model_t4 %>% mutate(ebu_latent_t4_mean = ifelse(is.na(ebu_latent_t4_mean), log_ebu_rate, ebu_latent_t4_mean))%>%
  mutate(ebu_latent_t4_sd = ifelse(is.na(ebu_latent_t4_sd), log_ebu_rate_sd, ebu_latent_t4_sd))


ebu_lag_alltraps <- full_ebullition_model_short_all %>% 
  select(time, log_ebu_rate, log_ebu_rate_sd)%>%
  fill(log_ebu_rate)%>%
  fill(log_ebu_rate_sd)

ebu_rate_model_alltraps <- left_join(temp_scale_model_all[,c(1,2,10)], ebu_beta1_all[,c(1,10,11)], by = "time")%>%
  left_join(., ebu_beta2_all[,c(1,10,11)], by = "time")%>%
  left_join(., ebu_beta3_all[,c(1,10,11)], by = "time")%>%
  left_join(., ebu_sigma_all[,c(1,10,11)], by = "time")%>%
  left_join(., ebu_latent_all[,c(1,10,11)], by = "time")%>%
  left_join(., ebu_lag_alltraps, by = "time")

ebu_rate_model_alltraps <- ebu_rate_model_alltraps %>% mutate(ebu_latent_alltrap_mean = ifelse(is.na(ebu_latent_alltrap_mean), log_ebu_rate, ebu_latent_alltrap_mean))%>%
  mutate(ebu_latent_alltrap_sd = ifelse(is.na(ebu_latent_alltrap_sd), log_ebu_rate_sd, ebu_latent_alltrap_sd))

###########################################################################################################

# Execute the actual ebullition forecast
###########################################################################################################
# TRAP #1 
# all sources of uncertainty accounted for
ebu_rate_forecast_t1 <- ebu_rate_model_t1 %>%
  group_by(time, variable)%>%
  mutate(forecast_ebu_rate_t1 = (rnorm(1, ebu_beta2_t1_mean, ebu_beta2_t1_sd)*rnorm(1, ebu_latent_t1_mean, ebu_latent_t1_sd)) +
                                (rnorm(1, ebu_beta3_t1_mean, ebu_beta3_t1_sd)*scaled_temp_t1) +
                                rnorm(1, ebu_beta1_t1_mean, ebu_beta1_t1_sd) + rnorm(1, ebu_sigma_t1_mean, ebu_sigma_t1_sd))%>%
  filter(time >= "2019-07-15")

# Summarize to daily
ebu_rate_forecast_t1_summarize <- ebu_rate_forecast_t1 %>%
  group_by(time) %>% 
  filter(forecast_ebu_rate_t1 < quantile(forecast_ebu_rate_t1, 0.75, na.rm = T)) %>% 
  filter(forecast_ebu_rate_t1 > quantile(forecast_ebu_rate_t1, 0.25, na.rm = T)) %>%
  summarise(mean = mean(forecast_ebu_rate_t1),
            upper = quantile(forecast_ebu_rate_t1, 0.75),
            lower = quantile(forecast_ebu_rate_t1, 0.25),
            vairiance = var(forecast_ebu_rate_t1),.groups = "drop")%>%
  mutate(trap_id = "Trap 1")


# Trap #2
ebu_rate_forecast_t2 <- ebu_rate_model_t2 %>%
  group_by(time, variable)%>%
  mutate(forecast_ebu_rate_t2 = (rnorm(1, ebu_beta2_t2_mean, ebu_beta2_t2_sd)*rnorm(1, ebu_latent_t2_mean, ebu_latent_t2_sd)) +
           (rnorm(1, ebu_beta3_t2_mean, ebu_beta3_t2_sd)*scaled_temp_t2) +
           rnorm(1, ebu_beta1_t2_mean, ebu_beta1_t2_sd) + rnorm(1, ebu_sigma_t2_mean, ebu_sigma_t2_sd))%>%
  filter(time >= "2019-07-15")

ebu_rate_forecast_t2_summarize <- ebu_rate_forecast_t2 %>%
  group_by(time) %>% 
  filter(forecast_ebu_rate_t2 < quantile(forecast_ebu_rate_t2, 0.75, na.rm = T)) %>% 
  filter(forecast_ebu_rate_t2 > quantile(forecast_ebu_rate_t2, 0.25, na.rm = T)) %>%
  summarise(mean = mean(forecast_ebu_rate_t2),
            upper = quantile(forecast_ebu_rate_t2, 0.75),
            lower = quantile(forecast_ebu_rate_t2, 0.25),
            vairiance = var(forecast_ebu_rate_t2),.groups = "drop")%>%
  mutate(trap_id = "Trap 2")


# Trap #3
ebu_rate_forecast_t3 <- ebu_rate_model_t3 %>%
  group_by(time, variable)%>%
  mutate(forecast_ebu_rate_t3 = (rnorm(1, ebu_beta2_t3_mean, ebu_beta2_t3_sd)*rnorm(1, ebu_latent_t3_mean, ebu_latent_t3_sd)) +
           (rnorm(1, ebu_beta3_t3_mean, ebu_beta3_t3_sd)*scaled_temp_t3) +
           rnorm(1, ebu_beta1_t3_mean, ebu_beta1_t3_sd) + rnorm(1, ebu_sigma_t3_mean, ebu_sigma_t3_sd))%>%
  filter(time >= "2019-07-15")

ebu_rate_forecast_t3_summarize <- ebu_rate_forecast_t3 %>%
  group_by(time) %>% 
  filter(forecast_ebu_rate_t3 < quantile(forecast_ebu_rate_t3, 0.75, na.rm = T)) %>% 
  filter(forecast_ebu_rate_t3 > quantile(forecast_ebu_rate_t3, 0.25, na.rm = T)) %>%
  summarise(mean = mean(forecast_ebu_rate_t3),
            upper = quantile(forecast_ebu_rate_t3, 0.75),
            lower = quantile(forecast_ebu_rate_t3, 0.25),
            vairiance = var(forecast_ebu_rate_t3),.groups = "drop")%>%
  mutate(trap_id = "Trap 3")



# Trap #4
ebu_rate_forecast_t4 <- ebu_rate_model_t4 %>%
  group_by(time, variable)%>%
  mutate(forecast_ebu_rate_t4 = (rnorm(1, ebu_beta2_t4_mean, ebu_beta2_t4_sd)*rnorm(1, ebu_latent_t4_mean, ebu_latent_t4_sd)) +
           (rnorm(1, ebu_beta3_t4_mean, ebu_beta3_t4_sd)*scaled_temp_t4) +
           rnorm(1, ebu_beta1_t4_mean, ebu_beta1_t4_sd) + rnorm(1, ebu_sigma_t4_mean, ebu_sigma_t4_sd))%>%
  filter(time >= "2019-07-15")

ebu_rate_forecast_t4_summarize <- ebu_rate_forecast_t4 %>%
  group_by(time) %>% 
  filter(forecast_ebu_rate_t4 < quantile(forecast_ebu_rate_t4, 0.75, na.rm = T)) %>% 
  filter(forecast_ebu_rate_t4 > quantile(forecast_ebu_rate_t4, 0.25, na.rm = T)) %>%
  summarise(mean = mean(forecast_ebu_rate_t4),
            upper = quantile(forecast_ebu_rate_t4, 0.75),
            lower = quantile(forecast_ebu_rate_t4, 0.25),
            vairiance = var(forecast_ebu_rate_t4),.groups = "drop")%>%
  mutate(trap_id = "Trap 4")


ebu_rate_forecast_alltraps <- ebu_rate_model_alltraps %>%
  group_by(time, variable)%>%
  mutate(forecast_ebu_rate_alltraps = (rnorm(1, ebu_beta2_alltrap_mean, ebu_beta2_alltrap_sd)*rnorm(1, ebu_latent_alltrap_mean, ebu_latent_alltrap_mean)) +
           (rnorm(1, ebu_beta3_alltrap_mean, ebu_beta3_alltrap_sd)*scaled_temp_alltraps) +
           rnorm(1, ebu_beta1_alltrap_mean, ebu_beta1_alltrap_sd) + rnorm(1, ebu_sigma_alltrap_mean, ebu_sigma_alltrap_sd))%>%
  filter(time >= "2019-07-15")

ebu_rate_forecast_alltraps_summarize <- ebu_rate_forecast_alltraps %>%
  group_by(time) %>% 
  filter(forecast_ebu_rate_alltraps < quantile(forecast_ebu_rate_alltraps, 0.75, na.rm = T)) %>% 
  filter(forecast_ebu_rate_alltraps > quantile(forecast_ebu_rate_alltraps, 0.25, na.rm = T)) %>%
  summarise(mean = mean(forecast_ebu_rate_alltraps),
            upper = quantile(forecast_ebu_rate_alltraps, 0.75),
            lower = quantile(forecast_ebu_rate_alltraps, 0.25),
            vairiance = var(forecast_ebu_rate_alltraps),.groups = "drop")%>%
  mutate(trap_id = "All Traps")
###########################################################################################################

# Plot the summarized total forecast variance
###########################################################################################################
summarized_trap_forecasts <- rbind(ebu_rate_forecast_t1_summarize,
                                   ebu_rate_forecast_t2_summarize,
                                   ebu_rate_forecast_t3_summarize,
                                   ebu_rate_forecast_t4_summarize,
                                   ebu_rate_forecast_alltraps_summarize)

# plot the total variance of the forecasts for each trap
ggplot(summarized_trap_forecasts, aes(time, vairiance, color = trap_id))+
  geom_point(size = 3, pch = 19)+
  theme_bw()+
  ylab("Forecast Variance")+
  scale_color_manual(breaks = c("Trap 1", "Trap 2", "Trap 3", "Trap 4", "All Traps"),
                     values = c("Trap 1" = "red", "Trap 2" = "orange", "Trap 3" = "darkgreen", "Trap 4" = "darkblue", "All Traps" = "grey30"))+
    theme(axis.text=element_text(size=15, color = "black"),
          axis.title=element_text(size=15, color = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.title = element_blank(), 
          title = element_text(size = 15),legend.position = "none",
          legend.text = element_text(size = 16, color = "black"))+
  facet_wrap(~trap_id, scales = "free_y")
###########################################################################################################

