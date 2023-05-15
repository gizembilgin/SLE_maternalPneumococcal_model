#library(plyr)

df <- read.csv("x_results/tornado.csv", header = TRUE)

# original value of output
base.value <- 570       

# get order of parameters according to size of intervals
order.parameters <- df %>% arrange(UL_Difference) %>%
  mutate(Parameter=factor(x=Parameter, levels=Parameter)) %>%
  select(Parameter) %>% unlist() %>% levels()

# width of columns in plot (value between 0 and 1)
width <- 0.95

# get data frame in shape for ggplot and geom_rect
df.2 <- df %>% 
  # gather columns Lower_Bound and Upper_Bound into a single column using gather
  gather(key='type', value='output.value', Lower_Bound:Upper_Bound) %>%
  # just reordering columns
  select(Parameter, type, output.value, UL_Difference) %>%
  # create the columns for geom_rect
  mutate(Parameter=factor(Parameter, levels=order.parameters),
         ymin=pmin(output.value, base.value),
         ymax=pmax(output.value, base.value),
         xmin=as.numeric(Parameter)-width/2,
         xmax=as.numeric(Parameter)+width/2)

# create plot

png(width = 800, height = 400)
options(scipen=999) #turn off scientific notation
plot = ggplot() + 
  geom_rect(data = df.2, 
            aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.title.y=element_blank(), legend.position = 'bottom',
        legend.title = element_blank()) + 
  geom_hline(yintercept = base.value) +
  ylab('Cost per DALY averted (2020 USD)') +
  scale_x_continuous(breaks = c(1:length(order.parameters)), 
                     labels = order.parameters) +
  coord_flip() + 
  geom_hline(mapping = NULL, yintercept = 526.51, linetype='dashed')
dev.off()
plot

#ggsave(width = 80, height = 40, filename = 'tornado_plot.png', units='mm')
