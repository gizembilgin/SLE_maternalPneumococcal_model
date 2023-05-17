
require(beepr)

this_outcome = "cost_DALY_averted"



### WTP plot for varying discounting rate ######################################
queue = list(
  list(discounting_rate = 0),
  list(discounting_rate = 0.03),
  list(discounting_rate = 0.05)
)

workshop = data.frame()
for (ticket in 1:length(queue)){
  MASTER_CONTROLS = queue[[ticket]]
  
  source(paste(getwd(),"/(7)_cost_effectiveness_analysis.R",sep=""))
  
  row = CEA_log_long %>%
    filter(outcome == this_outcome) %>%
    mutate(discounting_rate = this_disqaly)
  
  workshop = rbind(workshop,row)
}
 

to_plot = data.frame()

for (this_WTP in c(round(min(workshop$cost)):
                     round(max(workshop$cost)))){
  for (this_discounting_rate in unique(workshop$discounting_rate)){
  
    this_workshop = workshop %>% filter(discounting_rate == this_discounting_rate)
    
    row = data.frame(WTP = this_WTP,
                     probability = nrow(this_workshop[this_workshop$cost <= this_WTP,])/nrow(this_workshop),
                     discounting_rate = this_discounting_rate)
    
    to_plot = rbind(to_plot,row)
  }
}

ggplot() + geom_line(data = to_plot, aes(x=WTP,y=probability, color = as.factor(paste(discounting_rate*100,"%",sep = ""))) ) +
  xlab("Willingness to pay ($/DALY)") +
  ylab("Probability cost-effective") +
  theme_bw() +
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
for (ticket in 1:length(queue)){
  MASTER_CONTROLS = queue[[ticket]]
  
  source(paste(getwd(),"/(7)_cost_effectiveness_analysis.R",sep=""))
  
  row = CEA_log_long %>%
    filter(outcome == this_outcome) %>%
    mutate(vaccine_price = price_per_dose)
  
  workshop = rbind(workshop,row)
}


to_plot = data.frame()

for (this_WTP in c(round(min(workshop$cost)):
                     round(max(workshop$cost)))){
  for (this_vaccine_price in unique(workshop$vaccine_price)){
    
    this_workshop = workshop %>% filter(vaccine_price == this_vaccine_price)
    
    row = data.frame(WTP = this_WTP,
                     probability = nrow(this_workshop[this_workshop$cost <= this_WTP,])/nrow(this_workshop),
                     vaccine_price = this_vaccine_price)
    
    to_plot = rbind(to_plot,row)
  }
}

to_plot = to_plot %>%
  mutate(scenario_label = case_when(
    vaccine_price == 8.63 ~ "$8.63 (PAHO PPV price)",
    vaccine_price == 2.90 ~ "$2.90 (PCV price)",
    vaccine_price == 2.90*8.63/14.5 ~ paste("$",round(2.90*8.63/14.5,digits =2)," (hypothetical PPV price)",sep="")
  ))

ggplot() + geom_line(data = to_plot, aes(x=WTP,y=probability, color = as.factor(scenario_label)) ) +
  xlab("Willingness to pay ($/DALY)") +
  ylab("Probability cost-effective") +
  theme_bw() +
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
for (ticket in 1:length(queue)){
  MASTER_CONTROLS = queue[[ticket]]
  
  source(paste(getwd(),"/(0)_command_desk.R",sep=""))
  source(paste(getwd(),"/(7)_cost_effectiveness_analysis.R",sep=""))
  
  row = CEA_log_long %>%
    filter(outcome == this_outcome) %>%
    mutate(meff = meff)
  
  workshop = rbind(workshop,row)
}


to_plot = data.frame()

for (this_WTP in c(round(min(workshop$cost)):
                   round(max(workshop$cost)))){
  for (this_meff in unique(workshop$meff)){
    
    this_workshop = workshop %>% filter(meff == this_meff)
    
    row = data.frame(WTP = this_WTP,
                     probability = nrow(this_workshop[this_workshop$cost <= this_WTP,])/nrow(this_workshop),
                     meff = this_meff)
    
    to_plot = rbind(to_plot,row)
  }
}

ggplot() + geom_line(data = to_plot, aes(x=WTP,y=probability, color = as.factor(paste(meff*100,"%",sep = ""))) ) +
  xlab("Willingness to pay ($/DALY)") +
  ylab("Probability cost-effective") +
  theme_bw() +
  labs(color = "Maternal vaccine effectiveness")
beep()
################################################################################

