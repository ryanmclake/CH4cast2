# Parameter estimates for each trap all season long
temp_beta1_all_short <- temp_beta1_all_short %>% filter(time >= "2019-07-15")

beta1_temp_model <- ggplot()+
  geom_ribbon(data = temp_beta1_all_short, aes(x = time, ymin = beta1_t1_mean - beta1_t1_sd, ymax = beta1_t1_mean + beta1_t1_sd), fill = "red", alpha = 0.3)+
  geom_ribbon(data = temp_beta1_all_short, aes(x = time, ymin = beta1_t2_mean - beta1_t2_sd, ymax = beta1_t2_mean + beta1_t2_sd), fill = "orange", alpha = 0.3)+
  geom_ribbon(data = temp_beta1_all_short, aes(x = time, ymin = beta1_t3_mean - beta1_t3_sd, ymax = beta1_t3_mean + beta1_t3_sd), fill = "darkgreen", alpha = 0.3)+
  geom_ribbon(data = temp_beta1_all_short, aes(x = time, ymin = beta1_t4_mean - beta1_t4_sd, ymax = beta1_t4_mean + beta1_t4_sd), fill = "darkblue", alpha = 0.3)+
  geom_ribbon(data = temp_beta1_all_short, aes(x = time, ymin = beta1_alltrap_mean - beta1_alltrap_sd, ymax = beta1_alltrap_mean + beta1_alltrap_sd), fill = "grey30", alpha = 0.3)+
  geom_line(data = temp_beta1_all_short, aes(time, beta1_t1_mean, color = "Trap 1"), size = 1.5)+
  geom_line(data = temp_beta1_all_short, aes(time, beta1_t2_mean, color = "Trap 2"), size = 1.5)+
  geom_line(data = temp_beta1_all_short, aes(time, beta1_t3_mean, color = "Trap 3"), size = 1.5)+
  geom_line(data = temp_beta1_all_short, aes(time, beta1_t4_mean, color = "Trap 4"), size = 1.5)+
  geom_line(data = temp_beta1_all_short, aes(time, beta1_alltrap_mean, color = "All Traps"), size = 1.5)+
  scale_color_manual(breaks = c("Trap 1", "Trap 2", "Trap 3", "Trap 4", "All Traps"),
                     values = c("Trap 1" = "red", "Trap 2" = "orange", "Trap 3" = "darkgreen", "Trap 4" = "darkblue", "All Traps" = "grey30"))+
  ylab(expression(paste(beta[1], " (Intercept)")))+
  xlab("")+
  labs(title = "A")+
  theme_classic()+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(), 
        title = element_text(size = 15),legend.position = c(0.5,0.95),
        legend.text = element_text(size = 16, color = "black"))

temp_beta2_all_short <- temp_beta2_all_short %>% filter(time >= "2019-07-15")

beta2_temp_model <- ggplot()+
  geom_ribbon(data = temp_beta2_all_short, aes(x = time, ymin = beta2_t1_mean - beta2_t1_sd, ymax = beta2_t1_mean + beta2_t1_sd), fill = "red", alpha = 0.3)+
  geom_ribbon(data = temp_beta2_all_short, aes(x = time, ymin = beta2_t2_mean - beta2_t2_sd, ymax = beta2_t2_mean + beta2_t2_sd), fill = "orange", alpha = 0.3)+
  geom_ribbon(data = temp_beta2_all_short, aes(x = time, ymin = beta2_t3_mean - beta2_t3_sd, ymax = beta2_t3_mean + beta2_t3_sd), fill = "darkgreen", alpha = 0.3)+
  geom_ribbon(data = temp_beta2_all_short, aes(x = time, ymin = beta2_t4_mean - beta2_t4_sd, ymax = beta2_t4_mean + beta2_t4_sd), fill = "darkblue", alpha = 0.3)+
  geom_ribbon(data = temp_beta2_all_short, aes(x = time, ymin = beta2_alltrap_mean - beta2_alltrap_sd, ymax = beta2_alltrap_mean + beta2_alltrap_sd), fill = "grey30", alpha = 0.3)+
  geom_line(data = temp_beta2_all_short, aes(time, beta2_t1_mean, color = "Trap 1"), size = 1.5)+
  geom_line(data = temp_beta2_all_short, aes(time, beta2_t2_mean, color = "Trap 2"), size = 1.5)+
  geom_line(data = temp_beta2_all_short, aes(time, beta2_t3_mean, color = "Trap 3"), size = 1.5)+
  geom_line(data = temp_beta2_all_short, aes(time, beta2_t4_mean, color = "Trap 4"), size = 1.5)+
  geom_line(data = temp_beta2_all_short, aes(time, beta2_alltrap_mean, color = "All Traps"), size = 1.5)+
  scale_color_manual(breaks = c("Trap 1", "Trap 2", "Trap 3", "Trap 4", "All Traps"),
                     values = c("Trap 1" = "red", "Trap 2" = "orange", "Trap 3" = "darkgreen", "Trap 4" = "darkblue", "All Traps" = "grey30"))+
  ylab(expression(paste(beta[2], " (Temperature)")))+
  xlab("")+
  labs(title = "B")+
  theme_classic()+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(), 
        title = element_text(size = 15),legend.position = "none",
        legend.text = element_text(size = 16, color = "black"))

temp_sigma_all_short <- temp_sigma_all_short %>% filter(time >= "2019-07-15")

sigma_temp_model <- ggplot()+
  geom_ribbon(data = temp_sigma_all_short, aes(x = time, ymin = sigma_t1_mean - sigma_t1_sd, ymax = sigma_t1_mean + sigma_t1_sd), fill = "red", alpha = 0.3)+
  geom_ribbon(data = temp_sigma_all_short, aes(x = time, ymin = sigma_t2_mean - sigma_t2_sd, ymax = sigma_t2_mean + sigma_t2_sd), fill = "orange", alpha = 0.3)+
  geom_ribbon(data = temp_sigma_all_short, aes(x = time, ymin = sigma_t3_mean - sigma_t3_sd, ymax = sigma_t3_mean + sigma_t3_sd), fill = "darkgreen", alpha = 0.3)+
  geom_ribbon(data = temp_sigma_all_short, aes(x = time, ymin = sigma_t4_mean - sigma_t4_sd, ymax = sigma_t4_mean + sigma_t4_sd), fill = "darkblue", alpha = 0.3)+
  geom_ribbon(data = temp_sigma_all_short, aes(x = time, ymin = sigma_alltrap_mean - sigma_alltrap_sd, ymax = sigma_alltrap_mean + sigma_alltrap_sd), fill = "grey30", alpha = 0.3)+
  geom_line(data = temp_sigma_all_short, aes(time, sigma_t1_mean, color = "Trap 1"), size = 1.5)+
  geom_line(data = temp_sigma_all_short, aes(time, sigma_t2_mean, color = "Trap 2"), size = 1.5)+
  geom_line(data = temp_sigma_all_short, aes(time, sigma_t3_mean, color = "Trap 3"), size = 1.5)+
  geom_line(data = temp_sigma_all_short, aes(time, sigma_t4_mean, color = "Trap 4"), size = 1.5)+
  geom_line(data = temp_sigma_all_short, aes(time, sigma_alltrap_mean, color = "All Traps"), size = 1.5)+
  scale_color_manual(breaks = c("Trap 1", "Trap 2", "Trap 3", "Trap 4", "All Traps"),
                     values = c("Trap 1" = "red", "Trap 2" = "orange", "Trap 3" = "darkgreen", "Trap 4" = "darkblue", "All Traps" = "grey30"))+
  ylab(expression(paste(epsilon, " (sigma)")))+
  xlab("")+
  labs(title = "C")+
  theme_classic()+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(), 
        title = element_text(size = 15),legend.position = "none",
        legend.text = element_text(size = 16, color = "black"))

require(patchwork)

png("./figures/TemperatureScalingModelParamterEstimates.png", width = 13, height = 10, units = 'in', res = 800)
temp_model_parameters = (beta1_temp_model | beta2_temp_model)/(sigma_temp_model | plot_spacer())
temp_model_parameters
dev.off()



require(ggforce)
# Parameter estimates for each trap all season long
ebu_beta1_all_short <- ebu_beta1_all_short %>% filter(time >= "2019-07-15")

beta1_ebu_model <- ggplot()+
  geom_ribbon(data = ebu_beta1_all_short, aes(x = time, ymin = ebu_beta1_t1_mean - ebu_beta1_t1_sd, ymax = ebu_beta1_t1_mean + ebu_beta1_t1_sd), fill = "red", alpha = 0.3)+
  geom_ribbon(data = ebu_beta1_all_short, aes(x = time, ymin = ebu_beta1_t2_mean - ebu_beta1_t2_sd, ymax = ebu_beta1_t2_mean + ebu_beta1_t2_sd), fill = "orange", alpha = 0.3)+
  geom_ribbon(data = ebu_beta1_all_short, aes(x = time, ymin = ebu_beta1_t3_mean - ebu_beta1_t3_sd, ymax = ebu_beta1_t3_mean + ebu_beta1_t3_sd), fill = "darkgreen", alpha = 0.3)+
  geom_ribbon(data = ebu_beta1_all_short, aes(x = time, ymin = ebu_beta1_t4_mean - ebu_beta1_t4_sd, ymax = ebu_beta1_t4_mean + ebu_beta1_t4_sd), fill = "darkblue", alpha = 0.3)+
  geom_ribbon(data = ebu_beta1_all_short, aes(x = time, ymin = ebu_beta1_alltrap_mean - ebu_beta1_alltrap_sd, ymax = ebu_beta1_alltrap_mean + ebu_beta1_alltrap_sd), fill = "grey30", alpha = 0.3)+
  geom_line(data = ebu_beta1_all_short, aes(time, ebu_beta1_t1_mean, color = "Trap 1"), size = 1.5)+
  geom_line(data = ebu_beta1_all_short, aes(time, ebu_beta1_t2_mean, color = "Trap 2"), size = 1.5)+
  geom_line(data = ebu_beta1_all_short, aes(time, ebu_beta1_t3_mean, color = "Trap 3"), size = 1.5)+
  geom_line(data = ebu_beta1_all_short, aes(time, ebu_beta1_t4_mean, color = "Trap 4"), size = 1.5)+
  geom_line(data = ebu_beta1_all_short, aes(time, ebu_beta1_alltrap_mean, color = "All Traps"), size = 1.5)+
  scale_color_manual(breaks = c("Trap 1", "Trap 2", "Trap 3", "Trap 4", "All Traps"),
                     values = c("Trap 1" = "red", "Trap 2" = "orange", "Trap 3" = "darkgreen", "Trap 4" = "darkblue", "All Traps" = "grey30"))+
  ylab(expression(paste(beta[1], " (Intercept)")))+
  xlab("")+
  labs(title = "A")+
  theme_bw()+
  coord_cartesian(ylim=c(-50, 20))+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(), 
        title = element_text(size = 15),legend.position = c(0.8,0.2),
        legend.text = element_text(size = 16, color = "black"))

ebu_beta2_all_short <- ebu_beta2_all_short %>% filter(time >= "2019-07-15")

beta2_ebu_model <- ggplot()+
  geom_ribbon(data = ebu_beta2_all_short, aes(x = time, ymin = ebu_beta2_t1_mean - ebu_beta2_t1_sd, ymax = ebu_beta2_t1_mean + ebu_beta2_t1_sd), fill = "red", alpha = 0.3)+
  geom_ribbon(data = ebu_beta2_all_short, aes(x = time, ymin = ebu_beta2_t2_mean - ebu_beta2_t2_sd, ymax = ebu_beta2_t2_mean + ebu_beta2_t2_sd), fill = "orange", alpha = 0.3)+
  geom_ribbon(data = ebu_beta2_all_short, aes(x = time, ymin = ebu_beta2_t3_mean - ebu_beta2_t3_sd, ymax = ebu_beta2_t3_mean + ebu_beta2_t3_sd), fill = "darkgreen", alpha = 0.3)+
  geom_ribbon(data = ebu_beta2_all_short, aes(x = time, ymin = ebu_beta2_t4_mean - ebu_beta2_t4_sd, ymax = ebu_beta2_t4_mean + ebu_beta2_t4_sd), fill = "darkblue", alpha = 0.3)+
  geom_ribbon(data = ebu_beta2_all_short, aes(x = time, ymin = ebu_beta2_alltrap_mean - ebu_beta2_alltrap_sd, ymax = ebu_beta2_alltrap_mean + ebu_beta2_alltrap_sd), fill = "grey30", alpha = 0.3)+
  geom_line(data = ebu_beta2_all_short, aes(time, ebu_beta2_t1_mean, color = "Trap 1"), size = 1.5)+
  geom_line(data = ebu_beta2_all_short, aes(time, ebu_beta2_t2_mean, color = "Trap 2"), size = 1.5)+
  geom_line(data = ebu_beta2_all_short, aes(time, ebu_beta2_t3_mean, color = "Trap 3"), size = 1.5)+
  geom_line(data = ebu_beta2_all_short, aes(time, ebu_beta2_t4_mean, color = "Trap 4"), size = 1.5)+
  geom_line(data = ebu_beta2_all_short, aes(time, ebu_beta2_alltrap_mean, color = "All Traps"), size = 1.5)+
  scale_color_manual(breaks = c("Trap 1", "Trap 2", "Trap 3", "Trap 4", "All Traps"),
                     values = c("Trap 1" = "red", "Trap 2" = "orange", "Trap 3" = "darkgreen", "Trap 4" = "darkblue", "All Traps" = "grey30"))+
  ylab(expression(paste(beta[2], " (CH"[4]," Ebullition Rate"["(t)"],")")))+
  xlab("")+
  labs(title = "B")+
  theme_bw()+
  coord_cartesian(ylim=c(-1,1.5))+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(), 
        title = element_text(size = 15),legend.position = "none",
        legend.text = element_text(size = 16, color = "black"))

ebu_beta3_all_short <- ebu_beta3_all_short %>% filter(time >= "2019-07-15")

beta3_ebu_model <- ggplot()+
  geom_ribbon(data = ebu_beta3_all_short, aes(x = time, ymin = ebu_beta3_t1_mean - ebu_beta3_t1_sd, ymax = ebu_beta3_t1_mean + ebu_beta3_t1_sd), fill = "red", alpha = 0.3)+
  geom_ribbon(data = ebu_beta3_all_short, aes(x = time, ymin = ebu_beta3_t2_mean - ebu_beta3_t2_sd, ymax = ebu_beta3_t2_mean + ebu_beta3_t2_sd), fill = "orange", alpha = 0.3)+
  geom_ribbon(data = ebu_beta3_all_short, aes(x = time, ymin = ebu_beta3_t3_mean - ebu_beta3_t3_sd, ymax = ebu_beta3_t3_mean + ebu_beta3_t3_sd), fill = "darkgreen", alpha = 0.3)+
  geom_ribbon(data = ebu_beta3_all_short, aes(x = time, ymin = ebu_beta3_t4_mean - ebu_beta3_t4_sd, ymax = ebu_beta3_t4_mean + ebu_beta3_t4_sd), fill = "darkblue", alpha = 0.3)+
  geom_ribbon(data = ebu_beta3_all_short, aes(x = time, ymin = ebu_beta3_alltrap_mean - ebu_beta3_alltrap_sd, ymax = ebu_beta3_alltrap_mean + ebu_beta3_alltrap_sd), fill = "grey30", alpha = 0.3)+
  geom_line(data = ebu_beta3_all_short, aes(time, ebu_beta3_t1_mean, color = "Trap 1"), size = 1.5)+
  geom_line(data = ebu_beta3_all_short, aes(time, ebu_beta3_t2_mean, color = "Trap 2"), size = 1.5)+
  geom_line(data = ebu_beta3_all_short, aes(time, ebu_beta3_t3_mean, color = "Trap 3"), size = 1.5)+
  geom_line(data = ebu_beta3_all_short, aes(time, ebu_beta3_t4_mean, color = "Trap 4"), size = 1.5)+
  geom_line(data = ebu_beta3_all_short, aes(time, ebu_beta3_alltrap_mean, color = "All Traps"), size = 1.5)+
  scale_color_manual(breaks = c("Trap 1", "Trap 2", "Trap 3", "Trap 4", "All Traps"),
                     values = c("Trap 1" = "red", "Trap 2" = "orange", "Trap 3" = "darkgreen", "Trap 4" = "darkblue", "All Traps" = "grey30"))+
  ylab(expression(paste(beta[3], " (SWI Temperature"["(t+1)"],")")))+
  xlab("")+
  labs(title = "C")+
  theme_bw()+
  coord_cartesian(ylim=c(-1,3))+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(), 
        title = element_text(size = 15),legend.position = "none",
        legend.text = element_text(size = 16, color = "black"))

ebu_sigma_all_short <- ebu_sigma_all_short %>% filter(time >= "2019-07-15")

sigma_ebu_model <- ggplot()+
  geom_ribbon(data = ebu_sigma_all_short, aes(x = time, ymin = ebu_sigma_t1_mean - ebu_sigma_t1_sd, ymax = ebu_sigma_t1_mean + ebu_sigma_t1_sd), fill = "red", alpha = 0.3)+
  geom_ribbon(data = ebu_sigma_all_short, aes(x = time, ymin = ebu_sigma_t2_mean - ebu_sigma_t2_sd, ymax = ebu_sigma_t2_mean + ebu_sigma_t2_sd), fill = "orange", alpha = 0.3)+
  geom_ribbon(data = ebu_sigma_all_short, aes(x = time, ymin = ebu_sigma_t3_mean - ebu_sigma_t3_sd, ymax = ebu_sigma_t3_mean + ebu_sigma_t3_sd), fill = "darkgreen", alpha = 0.3)+
  geom_ribbon(data = ebu_sigma_all_short, aes(x = time, ymin = ebu_sigma_t4_mean - ebu_sigma_t4_sd, ymax = ebu_sigma_t4_mean + ebu_sigma_t4_sd), fill = "darkblue", alpha = 0.3)+
  geom_ribbon(data = ebu_sigma_all_short, aes(x = time, ymin = ebu_sigma_alltrap_mean - ebu_sigma_alltrap_sd, ymax = ebu_sigma_alltrap_mean + ebu_sigma_alltrap_sd), fill = "grey30", alpha = 0.3)+
  geom_line(data = ebu_sigma_all_short, aes(time, ebu_sigma_t1_mean, color = "Trap 1"), size = 1.5)+
  geom_line(data = ebu_sigma_all_short, aes(time, ebu_sigma_t2_mean, color = "Trap 2"), size = 1.5)+
  geom_line(data = ebu_sigma_all_short, aes(time, ebu_sigma_t3_mean, color = "Trap 3"), size = 1.5)+
  geom_line(data = ebu_sigma_all_short, aes(time, ebu_sigma_t4_mean, color = "Trap 4"), size = 1.5)+
  geom_line(data = ebu_sigma_all_short, aes(time, ebu_sigma_alltrap_mean, color = "All Traps"), size = 1.5)+
  scale_color_manual(breaks = c("Trap 1", "Trap 2", "Trap 3", "Trap 4", "All Traps"),
                     values = c("Trap 1" = "red", "Trap 2" = "orange", "Trap 3" = "darkgreen", "Trap 4" = "darkblue", "All Traps" = "grey30"))+
  ylab(expression(paste(epsilon, " (sigma)")))+
  xlab("")+
  labs(title = "D")+
  theme_bw()+
  coord_cartesian(ylim=c(0,5))+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(), 
        title = element_text(size = 15),legend.position = "none",
        legend.text = element_text(size = 16, color = "black"))

png("./figures/EbullitionARModelParamterEstimates.png", width = 13, height = 10, units = 'in', res = 800)
ebu_model_parameters = (beta1_ebu_model | beta2_ebu_model)/(beta3_ebu_model | sigma_ebu_model)
ebu_model_parameters
dev.off()

