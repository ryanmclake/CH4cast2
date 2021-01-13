# Plot the ebullition forecast models

ebu_t1 <- full_ebullition_model %>% filter(trap_id == "t1eb1") %>%
  filter(time >= "2019-06-10")

ebu_t2 <- full_ebullition_model %>% filter(trap_id == "t1eb2") %>%
  filter(time >= "2019-06-10")

ebu_t3 <- full_ebullition_model %>% filter(trap_id == "t1eb3") %>%
  filter(time >= "2019-06-10")

ebu_t4 <- full_ebullition_model %>% filter(trap_id == "t1eb4") %>%
  filter(time >= "2019-06-10")


ebu_trap_1 <- ggplot()+
  geom_line(data = ebu_rate_forecast_t1, aes(time, forecast_ebu_rate_t1, group = variable), color = "red", alpha = 0.05)+
  geom_ribbon(data = ebu_rate_forecast_t1_summarize, aes(x = time, ymin = lower, ymax = upper),color = "grey20", fill = "darkred", alpha = 0.4)+
  geom_line(data = ebu_rate_forecast_t1_summarize, aes(time, mean), size = 1, color = "darkred")+
  geom_point(data = ebu_t1, aes(x = as.POSIXct(time), log_ebu_rate), pch = 21, color = "black", fill = "white", size = 2.5)+
  labs(title = "Trap 1")+
  xlab("")+
  theme_bw()+
  coord_cartesian(ylim=c(-10, 20))+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(), 
        title = element_text(size = 15),legend.position = "none",
        legend.text = element_text(size = 16, color = "black"))


ebu_trap_2 <- ggplot()+
  geom_line(data = ebu_rate_forecast_t2, aes(time, forecast_ebu_rate_t2, group = variable), color = "orange", alpha = 0.05)+
  geom_ribbon(data = ebu_rate_forecast_t2_summarize, aes(x = time, ymin = lower, ymax = upper),color = "grey20", fill = "darkorange", alpha = 0.4)+
  geom_line(data = ebu_rate_forecast_t2_summarize, aes(time, mean), size = 1, color = "darkorange")+
  geom_point(data = ebu_t2, aes(x = as.POSIXct(time), log_ebu_rate), pch = 21, color = "black", fill = "white", size = 2.5)+
  labs(title = "Trap 2")+
  xlab("")+
  theme_bw()+
  coord_cartesian(ylim=c(-10, 20))+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(), 
        title = element_text(size = 15),legend.position = "none",
        legend.text = element_text(size = 16, color = "black"))


ebu_trap_3 <- ggplot()+
  geom_line(data = ebu_rate_forecast_t3, aes(time, forecast_ebu_rate_t3, group = variable), color = "darkgreen", alpha = 0.05)+
  geom_ribbon(data = ebu_rate_forecast_t3_summarize, aes(x = time, ymin = lower, ymax = upper),color = "grey20", fill = "darkgreen", alpha = 0.4)+
  geom_line(data = ebu_rate_forecast_t3_summarize, aes(time, mean), size = 1, color = "darkgreen")+
  geom_point(data = ebu_t3, aes(x = as.POSIXct(time), log_ebu_rate), pch = 21, color = "black", fill = "white", size = 2.5)+
  labs(title = "Trap 3")+
  xlab("")+
  theme_bw()+
  coord_cartesian(ylim=c(-10, 20))+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(), 
        title = element_text(size = 15),legend.position = "none",
        legend.text = element_text(size = 16, color = "black"))


ebu_trap_4 <- ggplot()+
  geom_line(data = ebu_rate_forecast_t4, aes(time, forecast_ebu_rate_t4, group = variable), color = "darkblue", alpha = 0.05)+
  geom_ribbon(data = ebu_rate_forecast_t4_summarize, aes(x = time, ymin = lower, ymax = upper),color = "grey20", fill = "darkblue", alpha = 0.4)+
  geom_line(data = ebu_rate_forecast_t4_summarize, aes(time, mean), size = 1, color = "darkblue")+
  geom_point(data = ebu_t4, aes(x = as.POSIXct(time), log_ebu_rate), pch = 21, color = "black", fill = "white", size = 2.5)+
  labs(title = "Trap 4")+
  xlab("")+
  theme_bw()+
  coord_cartesian(ylim=c(-10, 20))+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(), 
        title = element_text(size = 15),legend.position = "none",
        legend.text = element_text(size = 16, color = "black"))



ebu_trap_all <- ggplot()+
  geom_line(data = ebu_rate_forecast_alltraps, aes(time, forecast_ebu_rate_alltraps, group = variable), color = "grey30", alpha = 0.05)+
  geom_ribbon(data = ebu_rate_forecast_alltraps_summarize, aes(x = time, ymin = lower, ymax = upper),color = "grey20", fill = "grey30", alpha = 0.4)+
  geom_line(data = ebu_rate_forecast_alltraps_summarize, aes(time, mean), size = 1, color = "grey30")+
  geom_point(data = alltrap, aes(x = as.POSIXct(time), log_ebu_rate), pch = 21, color = "black", fill = "white", size = 2.5)+
  labs(title = "All Traps")+
  xlab("")+
  theme_bw()+
  coord_cartesian(ylim=c(-10, 20))+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(), 
        title = element_text(size = 15),legend.position = "none",
        legend.text = element_text(size = 16, color = "black"))


png("./figures/EbullitionForecastModelTimeSeries.png", width = 13, height = 15, units = 'in', res = 800)
ebu_forecast_model_plot = (ebu_trap_1 | ebu_trap_2)/(ebu_trap_3 | ebu_trap_4)/(ebu_trap_all | plot_spacer())
ebu_forecast_model_plot
dev.off()

