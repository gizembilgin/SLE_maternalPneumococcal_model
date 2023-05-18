
### This file takes estimate from Kobayashi et al. (2021) and converts them to 2020 USD estimates

### Step One: read in raw data
raw_estimates <- read.csv("1_inputs/Kobayashi_estimates.csv", header = TRUE)

### Step Two: convert back to Ghanaian cedis
#"“all costs collected in Ghanaian cedis and converted to 2019 USD using 1 Ghanaian cedis = 0.19 US dollars”
workshop = raw_estimates %>%
  mutate(estimate = estimate /0.19,
         lower.quantile = lower.quantile / 0.19,
         upper.quantile = upper.quantile /0.19)

### Step Three: inflate to 2020 ghanian
# World Economic Outlook database: April 2023 (UPDATED)
inflation = 235.886/215.685
workshop = workshop %>%
  mutate(estimate = estimate * inflation,
         lower.quantile = lower.quantile * inflation,
         upper.quantile = upper.quantile  * inflation)

### Step Four: convert back to USD
#https://data.worldbank.org/indicator/PA.NUS.FCRF?locations=GH
exchange_rate = 1/5.6
workshop = workshop %>%
  mutate(estimate = estimate * exchange_rate,
         lower.quantile = lower.quantile * exchange_rate,
         upper.quantile = upper.quantile  * exchange_rate)

healthcare_cost_estimates = workshop
save(healthcare_cost_estimates, file = "1_inputs/healthcare_cost_estimates.Rdata")
