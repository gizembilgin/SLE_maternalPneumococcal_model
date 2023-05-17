# The script creates the distribution of age-specific health outcomes of pneumococcal infection
# NB: distributions are for those under five ONLY

###### SETUP
################################################################################
#list of age groups to calculate for
#age_group_list = c(2,4,6,8,10,12,24,60) 
age_group_list = c(1:24,60) 
#creating time sequence to plot over
num_weeks=52*3
X=seq(1,num_weeks)

### TOGGLES
subset_LIC = "off"      #decision not to subset, as report suggestions no significant difference within regions, + need to smooth curve
inform_up_to_6 ="fitted" #options:"reported","fitted"   #decision to use fitted as report notes cases <2 severely under reported

################################################################################


###### PART ONE: DEATHS
################################################################################
shape_value = 1.7
scale_value = 23
shift_value = 0.0005

#(1) Have a look at the pdf
Y = shift_value+dgamma(X,shape_value,scale = scale_value) #pdf
#plot(X,Y*100, 
#     main = "distribution of deaths under five",
#     ylab = "percentage of all deaths",
#     xlab = "age in weeks")


#(2) Calculate the cdf
cdf_deaths_0 = 100*(0.0005+pgamma(52/12*age_group_list,shape_value,scale =scale_value)) #months converted to weeks by 52/12
cdf_deaths = data.frame(outcome = paste("death"),age_months = age_group_list, percent_cum = cdf_deaths_0)
################################################################################


###### PART TWO: Other health outcomes from SAGE review
################################################################################
#NB: LIC and LDC all the agree for this list of countries

outcomes_dataset <- read.csv("1_inputs/health_outcomes.csv",header=TRUE)
outcomes_list <- unique(outcomes_dataset$outcome)
plot_list = list()
plot_list2 = list()
cdf_outcome_final = data.frame()
CHECK_cdf_outcome_months = data.frame()


for (i in 1:length(outcomes_list)){ #do for every outcome
  outcome = outcomes_list[i]
  pdf_outcome = data.frame(c(),numeric(),numeric()) #pdf for plotting
  cdf_outcome = data.frame(c(),numeric(),numeric()) #cdf for plotting
  cdf_outcome_months = data.frame(c(),c(),numeric(),numeric()) #cdf for results
  
  this_outcome_data = outcomes_dataset[outcomes_dataset$outcome == outcome,]
  if (subset_LIC == "on"){
    this_outcome_data = this_outcome_data[this_outcome_data$LIC == 1,]
  }
  
  country_list <- this_outcome_data$country
  
  for (j in 1:nrow(this_outcome_data)){ # loop over included countries
    ### load gamma distribution parameters
    country_name = this_outcome_data$country[j]
    mean_value = this_outcome_data$mean[j]
    sd_value = this_outcome_data$std[j]
    shift_value = this_outcome_data$shift[j]
    
    store <- gammaParamsConvert(mean=mean_value,sd=sd_value)
    shape_value <- store$shape
    scale_value <- store$scale
    
    Y = dgamma(X+shift_value,shape_value,scale = scale_value) #pdf
    Y_cum = 100*(pgamma(X+shift_value,shape_value,scale = scale_value))
    Y_cum_months = 100*(pgamma(52/12*age_group_list+shift_value,shape_value,scale =scale_value))
    
    #creating long form
    pdf_country = data.frame(paste(country_name),X,Y)
    cdf_country = data.frame(paste(country_name),X,Y_cum)
    cdf_country_months = data.frame(paste(outcome),paste(country_name),age_group_list,Y_cum_months)
    
    colnames(pdf_country) <-c("country","X","Y")
    colnames(cdf_country) <-c("country","X","Y")
    colnames(cdf_country_months) <-c("outcome","country","age_months","Y")
    
    pdf_outcome=rbind(pdf_outcome,pdf_country) 
    cdf_outcome=rbind(cdf_outcome,cdf_country)
    cdf_outcome_months = rbind(cdf_outcome_months,cdf_country_months)
  }
  
  colnames(pdf_outcome) <-c("country","X","Y")
  colnames(cdf_outcome) <-c("country","X","Y")
  colnames(cdf_outcome_months) <-c("outcome","country","age_months","Y")
  
  
  #### PLOTTING
  plot_list[[i]] <- ggplot(data=pdf_outcome, aes(x=X,y=Y, colour=country)) + 
    geom_point() +
    labs(title=paste("distribution of all ",outcome," cases under five",sep=""),
         x="age in weeks",
         y="percentage of all cases")
  
  plot_list2[[i]] <- ggplot(data=cdf_outcome, aes(x=X,y=Y, colour=country)) + 
    geom_point() +
    labs(title=paste("cumulative distribution of all ",outcome," cases under five",sep=""),
         x="age in weeks",
         y="percentage of all cases")
  
  #### INCLUDING REPORTED VALUES
  if (inform_up_to_6 == "reported" ){
    cdf_outcome_reported <- this_outcome_data[c(1,2,8,9,10)]
    cdf_outcome_reported <- rbind(cbind(cdf_outcome_reported,age_months=2),cbind(cdf_outcome_reported,age_months=4),cbind(cdf_outcome_reported,age_months=6))
    cdf_outcome_reported <- cdf_outcome_reported %>%
        mutate(Y = case_when(
        age_months == 2 ~ cum2,
        age_months == 4 ~ cum4,
        age_months == 6 ~ cum6))
    cdf_outcome_reported <- cdf_outcome_reported[c(1,2,6,7)]
    
    cdf_outcome_months <- cdf_outcome_months[cdf_outcome_months$age_months >6,]
    
    cdf_outcome_months <- rbind(cdf_outcome_reported,cdf_outcome_months)
  }
  #CHECK_cdf_outcome_months = rbind(CHECK_cdf_outcome_months,cdf_outcome_months[cdf_outcome_months$age_months < 7,])
  CHECK_cdf_outcome_months = rbind(CHECK_cdf_outcome_months,cdf_outcome_months)
  
  ### FINAL RESULTS
  cdf_final_0 = data.frame(outcome = character(),
                           age_months = numeric(),
                           Y = numeric())
 
  workshop_months = unique(cdf_outcome_months$age_months)
  workshop_average=numeric(length=length(workshop_months))
  
  for (i in 1:length(workshop_months)){
      month = workshop_months[i]
      workshop_average[i] <- unique(ave(cdf_outcome_months$Y[cdf_outcome_months$age_months == month]))
  }
  
  cdf_final_0 = data.frame(outcome = paste(outcome),age_months = workshop_months, percent_cum = workshop_average)
  cdf_outcome_final = rbind(cdf_outcome_final,cdf_final_0)
  
}

# Print plots!
 plot_list[1] ; plot_list2[1]
 plot_list[2] ; plot_list2[2]
 plot_list[3] ; plot_list2[3]
 plot_list[4] ; plot_list2[4]
#CONFIRMED - all IPD presentations are distributions under three

cdf_outcome_final
cdf_outcome_final[,3]=cdf_outcome_final[,3]*0.9 #modification factor to get from U3 to U5

#Add deaths in with other outcomes
cdf_outcome_final = rbind(cdf_outcome_final,cdf_deaths)

### CONVERTING CUM TO ABS
#test <- cdf_outcome_final %>%
#  group_by(outcome,age_months) %>%
#  arrange(outcome,age_months) %>%
#  mutate(percent_interval = case_when(
#    age_months == 2 ~ percent_cum,
#    age_months != 2 ~ percent_cum - lag(percent_cum)))

cdf_outcome_final <- cdf_outcome_final %>%
  group_by(outcome) %>%
  arrange(outcome,age_months)%>%
  mutate(percent_interval = percent_cum - lag(percent_cum,default=0))

#write.csv(cdf_outcome_final, file = 'x_results/cdf final.csv')
################################################################################




