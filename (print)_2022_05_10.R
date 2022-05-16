
### MAIN PAPER ####################################################################################
## TABLE 2 (6 runs)
round(results_warehouse[[1]][[1]])
workshop=results_warehouse[[2]]
workshop[,c(1:6)] = round(workshop[,c(1:6)])
workshop = na.omit(workshop)
workshop

## TABLE 3 (output of original)
results_warehouse[[1]][[4]]

## TABLE 4 (output of original)
results_warehouse[[1]][[5]]

## TABLE 5 (blunting)
round(results_warehouse[[1]][[2]])
results_warehouse[[1]][[5]] %>% filter(category == 'death')
workshop=results_warehouse[[3]]
workshop[,c(1:4)] = round(workshop[,c(1:4)])
workshop$death_effect = round(workshop$death_effect,digits=1)
workshop = na.omit(workshop)
workshop



### SUPPLEMENTARY MATERIAL ###########################################################################
#S6.1 Infant PCV cov and effectiveness
workshop = results_warehouse[[4]]
workshop[,c(1:4)] = round(workshop[,c(1:4)])
workshop = na.omit(workshop)
workshop

#S6.2 COVID_19 sensitivity analysis
results_warehouse[[5]] 

#S6.3 population structure  sensitivity analysis
#S6.3.1 age structure
results_warehouse[[6]]
#S6.3.2 age-specific mortality
results_warehouse[[7]]
#S6.3.1 population growth
results_warehouse[[8]]

#S5.2 Length of Maternally derived Immunity
workshop = results_warehouse[[9]]
workshop[,c(1:4)] = round(workshop[,c(1:4)])
workshop = na.omit(workshop)
workshop
