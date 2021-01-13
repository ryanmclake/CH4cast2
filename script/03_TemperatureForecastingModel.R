# Develop the temperature scaling model forecasts

FLARE_long <- FLARE_long %>% arrange(full_time_day) %>%
  rename(time = full_time_day)

temp_scale_model_t1 <- left_join(FLARE_long, temp_beta1_all[1:3], by = "time")%>%
  left_join(., temp_beta2_all[1:3], by = "time")%>%
  left_join(., temp_sigma_all[1:3], by = "time")

temp_scale_model_t2 <- left_join(FLARE_long, temp_beta1_all[,c(1,4,5)], by = "time")%>%
  left_join(., temp_beta2_all[,c(1,4,5)], by = "time")%>%
  left_join(., temp_sigma_all[,c(1,4,5)], by = "time")

temp_scale_model_t3 <- left_join(FLARE_long, temp_beta1_all[,c(1,6,7)], by = "time")%>%
  left_join(., temp_beta2_all[,c(1,6,7)], by = "time")%>%
  left_join(., temp_sigma_all[,c(1,6,7)], by = "time")

temp_scale_model_t4 <- left_join(FLARE_long, temp_beta1_all[,c(1,8,9)], by = "time")%>%
  left_join(., temp_beta2_all[,c(1,8,9)], by = "time")%>%
  left_join(., temp_sigma_all[,c(1,8,9)], by = "time")

temp_scale_model_alltraps <- left_join(FLARE_long, temp_beta1_all[,c(1,10,11)], by = "time")%>%
  left_join(., temp_beta2_all[,c(1,10,11)], by = "time")%>%
  left_join(., temp_sigma_all[,c(1,10,11)], by = "time")


temp_scale_model_t1 <- temp_scale_model_t1 %>%
  group_by(time, variable)%>%
  mutate(scaled_temp_t1 = (rnorm(1, beta2_t1_mean, beta2_t1_sd)*value) + rnorm(1, beta1_t1_mean, beta1_t1_sd) + rnorm(1, sigma_t1_mean, sigma_t1_sd))%>%
  filter(time >= "2019-07-15")

temp_forecast_t1_summarize <- temp_scale_model_t1 %>%
  group_by(time) %>% 
  summarise(mean = mean(scaled_temp_t1),
            upper = quantile(scaled_temp_t1, 0.75),
            lower = quantile(scaled_temp_t1, 0.25),
            vairiance = var(scaled_temp_t1),.groups = "drop")%>%
  mutate(trap_id = "Trap 1")

temp_scale_model_t2 <- temp_scale_model_t2 %>%
  group_by(time, variable)%>%
  mutate(scaled_temp_t2 = (rnorm(1, beta2_t2_mean, beta2_t2_sd)*value) + rnorm(1, beta1_t2_mean, beta1_t2_sd) + rnorm(1, sigma_t2_mean, sigma_t2_sd))%>%
  filter(time >= "2019-07-15")

temp_forecast_t2_summarize <- temp_scale_model_t2 %>%
  group_by(time) %>% 
  summarise(mean = mean(scaled_temp_t2),
            upper = quantile(scaled_temp_t2, 0.75),
            lower = quantile(scaled_temp_t2, 0.25),
            vairiance = var(scaled_temp_t2),.groups = "drop")%>%
  mutate(trap_id = "Trap 2")

temp_scale_model_t3 <- temp_scale_model_t3 %>%
  group_by(time, variable)%>%
  mutate(scaled_temp_t3 = (rnorm(1, beta2_t3_mean, beta2_t3_sd)*value) + rnorm(1, beta1_t3_mean, beta1_t3_sd) + rnorm(1, sigma_t3_mean, sigma_t3_sd))%>%
  filter(time >= "2019-07-15")

temp_forecast_t3_summarize <- temp_scale_model_t3 %>%
  group_by(time) %>% 
  summarise(mean = mean(scaled_temp_t3),
            upper = quantile(scaled_temp_t3, 0.75),
            lower = quantile(scaled_temp_t3, 0.25),
            vairiance = var(scaled_temp_t3),.groups = "drop")%>%
  mutate(trap_id = "Trap 3")

temp_scale_model_t4 <- temp_scale_model_t4 %>%
  group_by(time, variable)%>%
  mutate(scaled_temp_t4 = (rnorm(1, beta2_t4_mean, beta2_t4_sd)*value) + rnorm(1, beta1_t4_mean, beta1_t4_sd) + rnorm(1, sigma_t4_mean, sigma_t4_sd))%>%
  filter(time >= "2019-07-15")

temp_forecast_t4_summarize <- temp_scale_model_t4 %>%
  group_by(time) %>% 
  summarise(mean = mean(scaled_temp_t4),
            upper = quantile(scaled_temp_t4, 0.75),
            lower = quantile(scaled_temp_t4, 0.25),
            vairiance = var(scaled_temp_t4),.groups = "drop")%>%
  mutate(trap_id = "Trap 4")

temp_scale_model_all <- temp_scale_model_alltraps %>%
  group_by(time, variable)%>%
  mutate(scaled_temp_alltraps = (rnorm(1, beta2_alltrap_mean, beta2_alltrap_sd)*value) + rnorm(1, beta1_alltrap_mean, beta1_alltrap_sd) + rnorm(1, sigma_alltrap_mean, sigma_alltrap_sd))%>%
  filter(time >= "2019-07-15")

temp_forecast_alltraps_summarize <- temp_scale_model_all %>%
  group_by(time) %>% 
  summarise(mean = mean(scaled_temp_alltraps),
            upper = quantile(scaled_temp_alltraps, 0.75),
            lower = quantile(scaled_temp_alltraps, 0.25),
            vairiance = var(scaled_temp_alltraps),.groups = "drop")%>%
  mutate(trap_id = "All Traps")


summarized_trap_forecasts <- rbind(temp_forecast_t1_summarize,
                                   temp_forecast_t2_summarize,
                                   temp_forecast_t3_summarize,
                                   temp_forecast_t4_summarize,
                                   temp_forecast_alltraps_summarize)

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
