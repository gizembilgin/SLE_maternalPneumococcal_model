CEAC_dataframe = data.frame()

this_workshop = CEA_log_long %>% filter(outcome == this_outcome)

for (this_WTP in c(round(min(this_workshop$cost)):
                   round(max(this_workshop$cost)))){
    
    row = data.frame(WTP = this_WTP,
                     probability = nrow(this_workshop[this_workshop$cost <= this_WTP,])/nrow(this_workshop))
    
    CEAC_dataframe = rbind(CEAC_dataframe,row)
}

main_plot_1 = ggplot() + geom_point(data = CEAC_dataframe, aes(x=WTP,y=probability)) +
  xlab("Willingness to pay ($/DALY)") +
  ylab("Probability cost-effective") +
  theme_bw() 
main_plot_2 = ggplot(incremental_log) +
  geom_point(aes(x=incremental_DALYs, y=incremental_cost))+
  theme_bw() + 
  xlab("DALYs averted") +
  ylab("Incremental costs (USD)") +
  xlim(0,max(incremental_log$incremental_DALYs)) +
  ylim(0,max(incremental_log$incremental_cost))

ggarrange(
  main_plot_1,
  main_plot_2,
  common.legend = TRUE,
  legend = "bottom",
  ncol = 2,
  nrow = 1
) 
#700 x 350 for paper