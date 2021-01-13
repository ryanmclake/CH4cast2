
#Temperature forecast plots

hobo_t1 <- hobo_t1 %>% filter(time >= "2019-06-10")
hobo_t2 <- hobo_t2 %>% filter(time >= "2019-06-10")
hobo_t3 <- hobo_t3 %>% filter(time >= "2019-06-10")
hobo_t4 <- hobo_t4 %>% filter(time >= "2019-06-10")
alltrap <- full_ebullition_model_short_all %>% filter(time >= "2019-06-10")

temp_trap_1 <- ggplot()+
  geom_line(data = temp_scale_model_t1, aes(time, scaled_temp_t1, group = variable), color = "red", alpha = 0.2)+
  geom_ribbon(data = temp_forecast_t1_summarize, aes(x = time, ymin = lower, ymax = upper),color = "grey20", fill = "darkred", alpha = 0.4)+
  geom_line(data = temp_forecast_t1_summarize, aes(time, mean), size = 1, color = "darkred")+
  geom_errorbar(data = hobo_t1, aes(x = as.POSIXct(time), ymin = temperature - temperature_sd, ymax = temperature + temperature_sd, position = "dodge"), color = "black")+
  geom_point(data = hobo_t1, aes(x = as.POSIXct(time), temperature),pch = 21, fill = "white", size = 3, color = "black")+
  labs(title = "Trap 1")+
  xlab("")+
  ylab(expression('Temperature ('*~degree*C*')'))+
  theme_bw()+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(), 
        title = element_text(size = 15),legend.position = "none",
        legend.text = element_text(size = 16, color = "black"))

temp_trap_2 <- ggplot()+
  geom_line(data = temp_scale_model_t2, aes(time, scaled_temp_t2, group = variable), color = "orange", alpha = 0.2)+
  geom_ribbon(data = temp_forecast_t2_summarize, aes(x = time, ymin = lower, ymax = upper),color = "grey20", fill = "darkorange", alpha = 0.4)+
  geom_line(data = temp_forecast_t2_summarize, aes(time, mean), size = 1, color = "darkorange")+
  geom_errorbar(data = hobo_t2, aes(x = as.POSIXct(time), ymin = temperature - temperature_sd, ymax = temperature + temperature_sd, position = "dodge"), color = "black")+
  geom_point(data = hobo_t2, aes(x = as.POSIXct(time), temperature),pch = 21, fill = "white", size = 3, color = "black")+
  labs(title = "Trap 2")+
  xlab("")+
  ylab(expression('Temperature ('*~degree*C*')'))+
  theme_bw()+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(), 
        title = element_text(size = 15),legend.position = "none",
        legend.text = element_text(size = 16, color = "black"))

temp_trap_3 <- ggplot()+
  geom_line(data = temp_scale_model_t3, aes(time, scaled_temp_t3, group = variable), color = "darkgreen", alpha = 0.2)+
  geom_ribbon(data = temp_forecast_t3_summarize, aes(x = time, ymin = lower, ymax = upper),color = "grey20", fill = "darkgreen", alpha = 0.4)+
  geom_line(data = temp_forecast_t3_summarize, aes(time, mean), size = 1, color = "darkgreen")+
  geom_errorbar(data = hobo_t3, aes(x = as.POSIXct(time), ymin = temperature - temperature_sd, ymax = temperature + temperature_sd, position = "dodge"), color = "black")+
  geom_point(data = hobo_t3, aes(x = as.POSIXct(time), temperature),pch = 21, fill = "white", size = 3, color = "black")+
  labs(title = "Trap 3")+
  xlab("")+
  ylab(expression('Temperature ('*~degree*C*')'))+
  theme_bw()+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(), 
        title = element_text(size = 15),legend.position = "none",
        legend.text = element_text(size = 16, color = "black"))

temp_trap_4 <- ggplot()+
  geom_line(data = temp_scale_model_t4, aes(time, scaled_temp_t4, group = variable), color = "blue1", alpha = 0.2)+
  geom_ribbon(data = temp_forecast_t4_summarize, aes(x = time, ymin = lower, ymax = upper),color = "grey20", fill = "darkblue", alpha = 0.4)+
  geom_line(data = temp_forecast_t4_summarize, aes(time, mean), size = 1, color = "darkblue")+
  geom_errorbar(data = hobo_t4, aes(x = as.POSIXct(time), ymin = temperature - temperature_sd, ymax = temperature + temperature_sd, position = "dodge"), color = "black")+
  geom_point(data = hobo_t4, aes(x = as.POSIXct(time), temperature),pch = 21, fill = "white", size = 3, color = "black")+
  labs(title = "Trap 4")+
  xlab("")+
  ylab(expression('Temperature ('*~degree*C*')'))+
  theme_bw()+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(), 
        title = element_text(size = 15),legend.position = "none",
        legend.text = element_text(size = 16, color = "black"))

temp_trap_all <- ggplot()+
  geom_line(data = temp_scale_model_all, aes(time, scaled_temp_alltraps, group = variable), color = "grey50", alpha = 0.2)+
  geom_ribbon(data = temp_forecast_alltraps_summarize, aes(x = time, ymin = lower, ymax = upper),color = "grey20", fill = "grey30", alpha = 0.4)+
  geom_line(data = temp_forecast_alltraps_summarize, aes(time, mean), size = 1, color = "grey30")+
  geom_errorbar(data = alltrap, aes(x = as.POSIXct(time), ymin = hobo_temp - hobo_temp_sd, ymax = hobo_temp + hobo_temp_sd, position = "dodge"), color = "black")+
  geom_point(data = alltrap, aes(x = as.POSIXct(time), hobo_temp),pch = 21, fill = "white", size = 3, color = "black")+
  labs(title = "All Traps")+
  xlab("")+
  ylab(expression('Temperature ('*~degree*C*')'))+
  theme_bw()+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(), 
        title = element_text(size = 15),legend.position = "none",
        legend.text = element_text(size = 16, color = "black"))

library(patchwork)
png("./figures/TemperatureScalingModelTimeSeries.png", width = 13, height = 15, units = 'in', res = 800)
temp_scale_model_plot = (temp_trap_1 | temp_trap_2)/(temp_trap_3 | temp_trap_4)/(temp_trap_all | plot_spacer())
temp_scale_model_plot
dev.off()