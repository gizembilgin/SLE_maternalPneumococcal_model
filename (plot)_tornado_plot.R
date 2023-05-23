
tornado_plot = list()

### RUN SCENARIOS
queue = list(
  list(vaccine_price = 1.73, label = "Vaccine price ($1.73-$8.63)",direction = "lower"),
  list(vaccine_price = 8.63, label = "Vaccine price ($1.73-$8.63)",direction = "upper"),
  
  list(vaccine_wastage = 0, label = "Vaccine wastage (0%-10%)",direction = "lower"),
  list(vaccine_wastage = 0.1, label = "Vaccine wastage (0%-10%)",direction = "upper"),
  
  list(operational_cost_multiplier = 0.5, label = "Operational costs (±50%)",direction = "lower"),
  list(operational_cost_multiplier = 1.5, label = "Operational costs (±50%)",direction = "upper"),
  
  list(healthcare_cost_multiplier = 0.5, label = "Direct and in-direct medical costs (±50%)",direction = "lower"),
  list(healthcare_cost_multiplier = 1.5, label = "Direct and in-direct medical costs (±50%)",direction = "upper"),  
  
  list(discounting_rate = 0.0, label = "Discounting (0%-5%)",direction = "lower" ),
  list(discounting_rate = 0.05, label = "Discounting (0%-5%)",direction = "upper"),
  
  list(meff = 0.5, label = "Vaccine effectiveness (50%-90%)",direction = "lower" ),
  list(meff = 0.90, label = "Vaccine effectiveness (50%-90%)",direction = "upper"),
  #list(meff = 0.75, label = "Vaccine effectiveness (50%-90%)"), #baseline estimate
  
  list(mcov = 0.7, label = "Vaccine coverage (70%-97%)",direction = "lower"),
  list(mcov = 0.9743, label = "Vaccine coverage (70%-97%)",direction = "upper") #baseline estimate
  
)

workshop = data.frame()

for (ticket in 1:length(queue)){
  MASTER_CONTROLS = queue[[ticket]]
  
  if ("meff" %in% names(MASTER_CONTROLS) | "mcov" %in% names(MASTER_CONTROLS)){
    source(paste(getwd(),"/(0)_command_desk.R",sep=""))
  }
  
  source(paste(getwd(),"/(7)_cost_effectiveness_analysis.R",sep=""))
  
  row = incremental_ICER %>%
    mutate(label = MASTER_CONTROLS$label,
           direction = MASTER_CONTROLS$direction)
  workshop = rbind(workshop,row)
  
}

MASTER_CONTROLS = list()
#_______________________________________________________________________________



### PLOT
base.value <- workshop$ICER[workshop$direction == "upper" & workshop$label == "Vaccine price ($1.73-$8.63)" & workshop$ICER_variable == "incremental_DALYs"] # final value was baseline estimates

# width of columns in plot (value between 0 and 1)
width <- 0.95
order_parameters <- workshop %>%
  filter(ICER_variable == "incremental_DALYs") %>%
  select(label,ICER,direction) %>%
  group_by(label) %>%
  summarise(LB = min(ICER),
            UB = max(ICER)) %>%
  mutate(UL_Difference = UB - LB) %>% 
  arrange(UL_Difference) %>%
  mutate(label=factor(x=label, levels=label)) %>%
  select(label) %>% 
  unlist() %>% 
  levels()

# get data frame in shape for ggplot and geom_rect
df_2 <- workshop %>%
  filter(ICER_variable == "incremental_DALYs") %>%
  select(label,ICER,direction) %>% 
  rename(value = ICER) %>%
  ungroup() %>%
  # create the columns for geom_rect
  mutate(label=factor(label, levels=order_parameters),
         ymin=pmin(value, base.value),
         ymax=pmax(value, base.value),
         xmin=as.numeric(label)-width/2,
         xmax=as.numeric(label)+width/2)

# force consistency between stochastic runs
df_2$ymin[df_2$label == "Vaccine price ($1.73-$8.63)" & df_2$direction == "upper"] = df_2$ymax[df_2$label == "Vaccine price ($1.73-$8.63)" & df_2$direction == "upper"]
df_2$ymin[df_2$label == "Vaccine coverage (70%-97%)" & df_2$direction == "upper"] = df_2$ymax[df_2$label == "Vaccine coverage (70%-97%)"  & df_2$direction == "upper"]


# create plot
require(ggtext)
options(scipen=999) #turn off scientific notation

tornado_plot[[length(tornado_plot)+1]] = ggplot() + 
  geom_rect(data = df_2, 
            aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=paste(direction,"estimate"))) + 
  geom_hline(yintercept = base.value) +
  theme_bw() + 
  #theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.title.y=element_blank(), legend.position = 'bottom',
        legend.title = element_blank())  +
  ylab('Cost per DALY averted (2020 USD)') +
  scale_x_continuous(breaks = c(1:length(order_parameters)), 
                     labels = order_parameters) +
  coord_flip() + 
  geom_hline(mapping = NULL, yintercept = 526.51, linetype='dashed') +
  annotate("text", x = 8, y = 415, label = "GDP per capita")

tornado_plot[[1]] = tornado_plot[[1]] + ggtitle(("Healthcare perspective"))+ 
  theme(plot.title = element_text(size = 15)) 
tornado_plot[[2]] = tornado_plot[[2]] + ggtitle(("Societal perspective"))+ 
  theme(plot.title = element_text(size = 15)) 

ggarrange(
  tornado_plot[[1]],
  tornado_plot[[2]],
  common.legend = TRUE,
  legend = "bottom",
  ncol = 1,
  nrow = 2
) 
