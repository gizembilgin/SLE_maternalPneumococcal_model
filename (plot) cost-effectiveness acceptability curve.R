
require(beepr); require(ggpubr)

this_outcome = "cost_DALY_averted"



### WTP plot for varying discounting rate ######################################
queue = list(
  list(discounting_rate = 0),
  list(discounting_rate = 0.03),
  list(discounting_rate = 0.05)
)

workshop = data.frame()
ICEP_dataframe = data.frame()

for (ticket in 1:length(queue)){
  MASTER_CONTROLS = queue[[ticket]]
  
  source(paste(getwd(),"/(7)_cost_effectiveness_analysis.R",sep=""))
  
  row = CEA_log_long %>%
    filter(outcome == this_outcome) %>%
    mutate(discounting_rate = this_disqaly)
  workshop = rbind(workshop,row)
  
  row = incremental_log  %>%
    mutate(discounting_rate = this_disqaly)
  ICEP_dataframe = rbind(ICEP_dataframe,row)
  
}
 

CEAC_dataframe = data.frame()

for (this_WTP in c(round(min(workshop$cost)):
                     round(max(workshop$cost)))){
  for (this_discounting_rate in unique(workshop$discounting_rate)){
  
    this_workshop = workshop %>% filter(discounting_rate == this_discounting_rate)
    
    row = data.frame(WTP = this_WTP,
                     probability = nrow(this_workshop[this_workshop$cost <= this_WTP,])/nrow(this_workshop),
                     discounting_rate = this_discounting_rate)
    
    CEAC_dataframe = rbind(CEAC_dataframe,row)
  }
}

plot_1 = ggplot() + geom_point(data = CEAC_dataframe, aes(x=WTP,y=probability, color = as.factor(paste(discounting_rate*100,"%",sep = ""))) ) +
  xlab("Willingness to pay ($/DALY)") +
  ylab("Probability cost-effective") +
  theme_bw() +
  labs(color = "Discounting rate")
plot_2 = ggplot(ICEP_dataframe) +
  geom_point(aes(x=incremental_DALYs, y=incremental_cost, color = as.factor(paste(discounting_rate*100,"%",sep = ""))))+
  theme_bw() + 
  xlab("DALYs averted") +
  ylab("Incremental costs (USD)") +
  xlim(0,max(ICEP_dataframe$incremental_DALYs)) +
  ylim(0,max(ICEP_dataframe$incremental_cost))+
  labs(color = "Discounting rate")
beep()
################################################################################




### WTP plot for varying vaccine price #########################################
queue = list(
  list(vaccine_price = 8.63),
  list(vaccine_price = 2.90),
  list(vaccine_price = 2.90*8.63/14.5)
)

workshop = data.frame()
ICEP_dataframe = data.frame()
for (ticket in 1:length(queue)){
  MASTER_CONTROLS = queue[[ticket]]
  
  source(paste(getwd(),"/(7)_cost_effectiveness_analysis.R",sep=""))
  
  row = CEA_log_long %>%
    filter(outcome == this_outcome) %>%
    mutate(vaccine_price = price_per_dose)
  workshop = rbind(workshop,row)
  
  row = incremental_log  %>%
    mutate(vaccine_price = price_per_dose)
  ICEP_dataframe = rbind(ICEP_dataframe,row)
  
}


CEAC_dataframe = data.frame()

for (this_WTP in c(round(min(workshop$cost)):
                     round(max(workshop$cost)))){
  for (this_vaccine_price in unique(workshop$vaccine_price)){
    
    this_workshop = workshop %>% filter(vaccine_price == this_vaccine_price)
    
    row = data.frame(WTP = this_WTP,
                     probability = nrow(this_workshop[this_workshop$cost <= this_WTP,])/nrow(this_workshop),
                     vaccine_price = this_vaccine_price)
    
    CEAC_dataframe = rbind(CEAC_dataframe,row)
  }
}

CEAC_dataframe = CEAC_dataframe %>%
  mutate(scenario_label = case_when(
    vaccine_price == 8.63 ~ "$8.63 (PAHO PPV price)",
    vaccine_price == 2.90 ~ "$2.90 (PCV price)",
    vaccine_price == 2.90*8.63/14.5 ~ paste("$",round(2.90*8.63/14.5,digits =2)," (hypothetical PPV price)",sep="")
  ))
ICEP_dataframe= ICEP_dataframe%>%
  mutate(scenario_label = case_when(
    vaccine_price == 8.63 ~ "$8.63 (PAHO PPV price)",
    vaccine_price == 2.90 ~ "$2.90 (PCV price)",
    vaccine_price == 2.90*8.63/14.5 ~ paste("$",round(2.90*8.63/14.5,digits =2)," (hypothetical PPV price)",sep="")
  ))

plot_3 = ggplot() + geom_point(data = CEAC_dataframe, aes(x=WTP,y=probability, color = as.factor(scenario_label)) ) +
  xlab("Willingness to pay ($/DALY)") +
  ylab("Probability cost-effective") +
  theme_bw() +
  labs(color = "Vaccine price scenario")
plot_4 = ggplot(ICEP_dataframe) +
  geom_point(aes(x=incremental_DALYs, y=incremental_cost, color = as.factor(scenario_label)))+
  theme_bw() + 
  xlab("DALYs averted") +
  ylab("Incremental costs (USD)") +
  xlim(0,max(ICEP_dataframe$incremental_DALYs)) +
  ylim(0,max(ICEP_dataframe$incremental_cost))+
  labs(color = "Vaccine price scenario")
beep()
################################################################################





### WTP plot for varying vaccine effectiveness #################################
queue = list(
  list(meff = 0.5),
  list(meff =0.625),
  list(meff = 0.90),
  list(meff = 0.75) #baseline estimate
)

workshop = data.frame()
ICEP_dataframe = data.frame()
for (ticket in 1:length(queue)){
  MASTER_CONTROLS = queue[[ticket]]
  
  source(paste(getwd(),"/(0)_command_desk.R",sep=""))
  source(paste(getwd(),"/(7)_cost_effectiveness_analysis.R",sep=""))
  
  row = CEA_log_long %>%
    filter(outcome == this_outcome) %>%
    mutate(meff = meff)
  workshop = rbind(workshop,row)
  
  row = incremental_log  %>%
    mutate(meff = meff)
  ICEP_dataframe = rbind(ICEP_dataframe,row)
  
}


CEAC_dataframe = data.frame()

for (this_WTP in c(round(min(workshop$cost)):
                   round(max(workshop$cost)))){
  for (this_meff in unique(workshop$meff)){
    
    this_workshop = workshop %>% filter(meff == this_meff)
    
    row = data.frame(WTP = this_WTP,
                     probability = nrow(this_workshop[this_workshop$cost <= this_WTP,])/nrow(this_workshop),
                     meff = this_meff)
    
    CEAC_dataframe = rbind(CEAC_dataframe,row)
  }
}

plot_5 = ggplot() + geom_point(data = CEAC_dataframe, aes(x=WTP,y=probability, color = as.factor(paste(meff*100,"%",sep = ""))) ) +
  xlab("Willingness to pay ($/DALY)") +
  ylab("Probability cost-effective") +
  theme_bw() +
  labs(color = "Maternal vaccine effectiveness")
plot_6 = ggplot(ICEP_dataframe) +
  geom_point(aes(x=incremental_DALYs, y=incremental_cost, color = as.factor(paste(meff*100,"%",sep = ""))) )+
  theme_bw() + 
  xlab("DALYs averted") +
  ylab("Incremental costs (USD)") +
  xlim(0,max(ICEP_dataframe$incremental_DALYs)) +
  ylim(0,max(ICEP_dataframe$incremental_cost))+
  labs(color = "Maternal vaccine effectiveness")
beep()
################################################################################



ggarrange(
  plot_1,
  plot_2,
  common.legend = TRUE,
  legend = "bottom",
  ncol = 2,
  nrow = 1
)
ggarrange(
  plot_3,
  plot_4,
  common.legend = TRUE,
  legend = "bottom",
  ncol = 2,
  nrow = 1
)
ggarrange(
  plot_5,
  plot_6,
  common.legend = TRUE,
  legend = "bottom",
  ncol = 2,
  nrow = 1
) 

