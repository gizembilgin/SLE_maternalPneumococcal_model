#NOTE: THESE ARE ALL IN 2020 USD
#COMBEBACK COUNT = 4

###user toggles
costing = "fixed" #options: fixed, rand
num_runs = 1
# mcov = 0.99


#       (1/3) Costs of vaccine program                 
################################################################################
###(A/C) Vaccine costs
# Note, these are all fixed costs
price_per_dose = 8.63  # 8.63 compared to 2.90 GAVI price for PCV (sensitivity analysis)
wastage = 0.05                # % of doses
freight_costs = 0.045         # % of vaccine value
injection_equipment = 0.044  
injection_equipment_wastage = 0.1

vaccine_cost_per_dose = price_per_dose * (1 + wastage) * (1 + freight_costs) + injection_equipment * (1+injection_equipment_wastage)
vaccine_cost_100000 = mcov * 100000 * vaccine_cost_per_dose


###(B/C) Operational costs
OC_mean = 0.95; OC_sd = 0.38
OC_a = (OC_mean/OC_sd)^2 ; OC_b = (OC_sd^2)/OC_mean #6.25 and 0.152

if (costing == "fixed"){operational_costs_100000 = 100000*OC_mean
} else if (costing == "rand"){operational_costs_100000 = sum(rgamma(100000, OC_a, scale=OC_b))}


###(C/C) Total Intervention costs
total_intervention_costs_100000 = vaccine_cost_100000 + operational_costs_100000
################################################################################



#       (2/3) Health system costs averted               
################################################################################
# COMEBACK, code rand in health outcome AND presentation distribution

###(A/D) Incidence of health outcomes/syndromes averted
#structured as per Figure 1 (pneum severe, pneum non-severe, meningitis, other IPD severe, other IPD non-severe, non-IPD non-severe)
incidence_averted = rep(0,6) 
incidence_averted[1] = -burden_dataset_applied_U1[5,3]
incidence_averted[2] = -(burden_dataset_applied_U1[4,3]- burden_dataset_applied_U1[5,3])
incidence_averted[3] = -burden_dataset_applied_U1[6,3]
incidence_averted[4] = -burden_dataset_applied_U1[9,3]
incidence_averted[5] = -(burden_dataset_applied_U1[8,3] - burden_dataset_applied_U1[9,3])
incidence_averted[6] = reduction_incidence_under_one + burden_dataset_applied_U1[1,3]
incidence_averted; sum(incidence_averted); reduction_incidence_under_one #CHECK



###(B/D) Branch pneumoccocal presentations into types of care 
if (costing == "fixed"){
  branch_1 = 0.55   # Access to hospital care
  branch_2 = 0.857  # Care-seeking
  branch_3 = 0.70   # Access to hospital care
  branch_4 = 0.857  # Care-seeking
} else if (costing == "rand"){
  branch_1 = 0.55   # Access to hospital care
  branch_2 = 0.857  # Care-seeking
  branch_3 = 0.70   # Access to hospital care
  branch_4 = 0.857  # Care-seeking
}


presentations_averted = rep(0,12)
#structured as per Figure 1 (I,O,O,H,I,O,I,O,O,H,O,H)
presentations_averted[1] = incidence_averted[1] * branch_1
presentations_averted[2] = incidence_averted[1] * (1-branch_1)
presentations_averted[3] = incidence_averted[2] * branch_2
presentations_averted[4] = incidence_averted[2] * (1-branch_2)
presentations_averted[5] = incidence_averted[3] * branch_3
presentations_averted[6] = incidence_averted[3] * (1-branch_3)
presentations_averted[7] = incidence_averted[4] * branch_1
presentations_averted[8] = incidence_averted[4] * (1-branch_1)
presentations_averted[9] = incidence_averted[5] * branch_2
presentations_averted[10] = incidence_averted[5] * (1-branch_2)
presentations_averted[11] = incidence_averted[6] * branch_4
presentations_averted[12] = incidence_averted[6] * (1-branch_4)

presentations_averted <- round(presentations_averted, digits=0)
# incidence_averted;presentations_averted;sum(incidence_averted);sum(presentations_averted)# CHECK
num_outpatient = 0 #COMEBACK


###(C/D) 
## COMEBACK need to random
#initalise costs
inpatient_pneum = 128.5; inpatient_men = 141.1; inpatient_IPD = inpatient_pneum
outpatient_pneum = 1; outpatient_men = outpatient_pneum; outpatient_IPD = outpatient_pneum;outpatient_ARI = outpatient_pneum
homecare = 1.3

#outpatient costs
OP_mean = 0.95; OP_sd = 0.75
OP_a = (OP_mean/OP_sd)^2 ; OP_b = (OP_sd^2)/OP_mean #1.6, 0.59

mean = 0.95
sd = 0.75

a = (mean/sd)^2 #1.6
b = (sd^2)/mean #0.59

if (costing == "fixed"){operational_costs_100000 = num_outpatient*OP_mean
} else if (costing == "rand"){operational_costs_100000 = sum(rgamma(num_outpatient, OP_a, scale=OP_b))}

cost_care = c(inpatient_pneum,outpatient_pneum,outpatient_pneum,homecare,
              inpatient_men,outpatient_men,
              inpatient_IPD,outpatient_IPD,outpatient_IPD,homecare,
              outpatient_ARI,homecare)


###(D/D) 
total_cost_care_averted=presentations_averted*cost_care
total_cost_care_averted=sum(total_cost_care_averted)
################################################################################



#       (3/3) Cost-effectiveness Analysis               
################################################################################
#Note: this will be the cost per 100,000
cost_final = total_intervention_costs_100000 + total_cost_care_averted

cost_case_averted = cost_final/reduction_incidence_under_one
cost_death_averted = cost_final/-burden_dataset_applied[3,3]
cost_hospo_averted = cost_final/(presentations_averted[1]+presentations_averted[5]+presentations_averted[7])

cost_case_averted;cost_death_averted;cost_hospo_averted


##do we even need discounting if only over a year?
