main_plot_list = list()

for (this_perspective in c("societal","healthcare")){
  MASTER_CONTROLS = list(perspective = this_perspective)
  workshop = data.frame()
  
  if(mcov != 0.9743 | meff != 0.75){
    source(paste(getwd(),"/(0)_command_desk.R",sep=""))
  }
  source(paste(getwd(),"/(7)_cost_effectiveness_analysis.R",sep=""))
  
  workshop = CEA_log_long %>%
    filter(outcome == this_outcome) 


  CEAC_dataframe = data.frame()
  this_workshop = workshop %>% filter(outcome == this_outcome)
  
  for (this_WTP in c(round(min(this_workshop$cost)):
                     round(max(this_workshop$cost)))){
      
      row = data.frame(WTP = this_WTP,
                       probability = nrow(this_workshop[this_workshop$cost <= this_WTP,])/nrow(this_workshop))
      
      CEAC_dataframe = rbind(CEAC_dataframe,row)
  }
  
  main_plot_list[[length(main_plot_list) + 1]] = ggplot() + geom_point(data = CEAC_dataframe, aes(x=WTP,y=probability)) +
    xlab("Willingness to pay ($/DALY)") +
    ylab("Probability cost-effective") +
    theme_bw() 
  main_plot_list[[length(main_plot_list) + 1]] = ggplot(incremental_log) +
    geom_point(aes(x=incremental_DALYs, y=incremental_cost))+
    theme_bw() + 
    xlab("DALYs averted") +
    ylab("Incremental costs (USD)") +
    xlim(0,max(incremental_log$incremental_DALYs)) +
    ylim(0,max(incremental_log$incremental_cost))

}

main_plot_list[[1]] = main_plot_list[[1]] + ggtitle(("Societal perspective"))+ 
  theme(plot.title = element_text(size = 15, face = "bold"))
main_plot_list[[2]] = main_plot_list[[2]] + ggtitle((""))
main_plot_list[[3]] = main_plot_list[[3]] + ggtitle(("Healthcare perspective"))+ 
  theme(plot.title = element_text(size = 15, face = "bold"))
main_plot_list[[4]] = main_plot_list[[4]] + ggtitle((""))

ggarrange(
  main_plot_list[[1]],
  main_plot_list[[2]],
  main_plot_list[[3]],
  main_plot_list[[4]],
  common.legend = TRUE,
  legend = "bottom",
  ncol = 2,
  nrow = 2
) 
#700 x 350 for paper

