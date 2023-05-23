
require(beepr); require(ggpubr)

this_outcome = "cost_DALY_averted"

CEAC_plot_list = list()
MASTER_CONTROLS = list()
source(paste(getwd(),"/(7)_cost_effectiveness_analysis.R",sep=""))

### WTP plot for varying discounting rate ######################################
queue = list(
  list(discounting_rate = 0),
  list(discounting_rate = 0.03),
  list(discounting_rate = 0.05)
)

for (this_perspective in c("societal","healthcare")){
  workshop = data.frame()
  ICEP_dataframe = data.frame()
  
  for (ticket in 1:length(queue)){
      
      MASTER_CONTROLS = queue[[ticket]]
      MASTER_CONTROLS = append(MASTER_CONTROLS,list(perspective = this_perspective))
      
      source(paste(getwd(),"/(7)_cost_effectiveness_analysis.R",sep=""))
      
      row = CEA_log_long %>%
        filter(outcome == this_outcome) %>%
        mutate(discounting_rate = this_disqaly,
               perspective = this_perspective)
      workshop = rbind(workshop,row)
      
      row = incremental_log  %>%
        mutate(discounting_rate = this_disqaly,
               perspective = this_perspective)
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
  
  CEAC_plot_list[[length(CEAC_plot_list)+1]] = ggplot() + geom_point(data = CEAC_dataframe, aes(x=WTP,y=probability, color = as.factor(paste(discounting_rate*100,"%",sep = ""))) ) +
    xlab("Willingness to pay ($/DALY)") +
    ylab("Probability cost-effective") +
    theme_bw() +
    labs(color = "Discounting rate") 
  CEAC_plot_list[[length(CEAC_plot_list)+1]] = ggplot(ICEP_dataframe) +
    geom_point(aes(x=incremental_DALYs, y=incremental_cost, color = as.factor(paste(discounting_rate*100,"%",sep = ""))))+
    theme_bw() + 
    xlab("DALYs averted") +
    ylab("Incremental costs (USD)") +
    xlim(min(ICEP_dataframe$incremental_DALYs),max(ICEP_dataframe$incremental_DALYs)) +
    ylim(min(ICEP_dataframe$incremental_cost),max(ICEP_dataframe$incremental_cost))+
    labs(color = "Discounting rate")
}

#beep()
################################################################################




### WTP plot for varying vaccine price #########################################
queue = list(
  list(vaccine_price = 8.63),
  list(vaccine_price = 2.90),
  list(vaccine_price = 2.90*8.63/14.5)
)

for (this_perspective in c("societal","healthcare")){
  workshop = data.frame()
  ICEP_dataframe = data.frame()
  for (ticket in 1:length(queue)){
    MASTER_CONTROLS = queue[[ticket]]
    MASTER_CONTROLS = append(MASTER_CONTROLS,list(perspective = this_perspective))
    
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
  
  CEAC_plot_list[[length(CEAC_plot_list)+1]] = ggplot() + geom_point(data = CEAC_dataframe, aes(x=WTP,y=probability, color = as.factor(scenario_label)) ) +
    xlab("Willingness to pay ($/DALY)") +
    ylab("Probability cost-effective") +
    theme_bw() +
    labs(color = "Vaccine price scenario")
  CEAC_plot_list[[length(CEAC_plot_list)+1]] = ggplot(ICEP_dataframe) +
    geom_point(aes(x=incremental_DALYs, y=incremental_cost, color = as.factor(scenario_label)))+
    theme_bw() + 
    xlab("DALYs averted") +
    ylab("Incremental costs (USD)") +
    xlim(min(ICEP_dataframe$incremental_DALYs),max(ICEP_dataframe$incremental_DALYs)) +
    ylim(min(ICEP_dataframe$incremental_cost),max(ICEP_dataframe$incremental_cost))+
    labs(color = "Vaccine price scenario")
  #beep()
}
################################################################################





### WTP plot for varying vaccine effectiveness #################################
queue = list(
  list(meff = 0.5),
  list(meff =0.625),
  list(meff = 0.90),
  list(meff = 0.75) #baseline estimate
)

for (this_perspective in c("societal","healthcare")){
  

  workshop = data.frame()
  ICEP_dataframe = data.frame()
  for (ticket in 1:length(queue)){
    MASTER_CONTROLS = queue[[ticket]]
    MASTER_CONTROLS = append(MASTER_CONTROLS,list(perspective = this_perspective))
    
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
  
  CEAC_plot_list[[length(CEAC_plot_list)+1]] = ggplot() + geom_point(data = CEAC_dataframe, aes(x=WTP,y=probability, color = as.factor(paste(meff*100,"%",sep = ""))) ) +
    xlab("Willingness to pay ($/DALY)") +
    ylab("Probability cost-effective") +
    theme_bw() +
    labs(color = "Maternal vaccine effectiveness")
  CEAC_plot_list[[length(CEAC_plot_list)+1]] = ggplot(ICEP_dataframe) +
    geom_point(aes(x=incremental_DALYs, y=incremental_cost, color = as.factor(paste(meff*100,"%",sep = ""))) )+
    theme_bw() + 
    xlab("DALYs averted") +
    ylab("Incremental costs (USD)") +
    xlim(min(ICEP_dataframe$incremental_DALYs),max(ICEP_dataframe$incremental_DALYs)) +
    ylim(min(ICEP_dataframe$incremental_cost),max(ICEP_dataframe$incremental_cost))+
    labs(color = "Maternal vaccine effectiveness")
}
#beep()
################################################################################

for (i in 1:3){
  CEAC_plot_list[[1 +(i-1)*4]] = CEAC_plot_list[[1+(i-1)*4]] + ggtitle(("Societal perspective"))+ 
    theme(plot.title = element_text(size = 15, face = "bold"))
  CEAC_plot_list[[2+(i-1)*4]] = CEAC_plot_list[[2+(i-1)*4]] + ggtitle((""))
  CEAC_plot_list[[3+(i-1)*4]] = CEAC_plot_list[[3+(i-1)*4]] + ggtitle(("Healthcare perspective"))+ 
    theme(plot.title = element_text(size = 15, face = "bold"))
  CEAC_plot_list[[4+(i-1)*4]] = CEAC_plot_list[[4+(i-1)*4]] + ggtitle((""))
}


ggarrange(
  CEAC_plot_list[[1]],
  CEAC_plot_list[[2]],
  CEAC_plot_list[[3]],
  CEAC_plot_list[[4]],
  common.legend = TRUE,
  legend = "bottom",
  ncol = 2,
  nrow = 2
) 

ggarrange(
  CEAC_plot_list[[5]],
  CEAC_plot_list[[6]],
  CEAC_plot_list[[7]],
  CEAC_plot_list[[8]],
  common.legend = TRUE,
  legend = "bottom",
  ncol = 2,
  nrow = 2
) 

ggarrange(
  CEAC_plot_list[[9]],
  CEAC_plot_list[[10]],
  CEAC_plot_list[[11]],
  CEAC_plot_list[[12]],
  common.legend = TRUE,
  legend = "bottom",
  ncol = 2,
  nrow = 2
) 

MASTER_CONTROLS = list()
