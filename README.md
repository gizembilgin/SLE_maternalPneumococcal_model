# SLE_maternalPneumococcal_model
**Associated paper(s):**
* Modelling the impact of maternal pneumococcal vaccination on infant pneumococcal disease in low-income settings - published in Vaccine, see https://doi.org/10.1016/j.vaccine.2022.05.066
* Cost-effectiveness analysis of a maternal pneumococcal vaccine in low-income, high-burden settings such as Sierra Leone - working paper, see preprint https://doi.org/10.1101/2022.07.25.22278016

## Project overview
Maternal pneumococcal vaccines have been proposed as a strategy for protection in early childhood before direct immunity through infant pneumococcal conjugate vaccines develop. 
To our knowledge, this project offers the most comprehensive estimates of the impacts of a maternal pneumococcal vaccine. We developed a mathematical model informed by previous 
small-scale studies of maternal pneumococcal vaccination, and the characteristics of existing maternal tetanus, pertussis and influenza vaccines. We then used these expected 
characteristics of a maternal pneumoccocal vaccine to undertake cost-effectiveness analysis informed by previous studies of pneumococcal conjugate vaccines.

## Code history
The underlying transmission model was initially developed in 2019 in MATLAB as part of GMB's Honour Thesis. The scripts were later translated into Python and then R as part of the publication process of the 
mathematical modelling paper in 2021; version control history is available from this point. The cost-effectiveness analysis components were developed in 2021.

## Scripts included in this project
* *1_inputs/* contains all parameter files
* *x_results/* contains results
* *(0)_command_desk.R* runs all sub-scripts of the transmission model to complete one standard 'run'
* *(1-6)** scripts are the sub-scripts of the transmission model
* *(7)_cost_effectiveness_analysis** runs all sub-scripts of the cost-effectiveness analysis to complete one standard 'run'
* *(command)** scripts initiate complete runs of the model
* *(function)** scripts contain functions
* *(mech shop)** scripts create a subset of parameters used by the model
* *(plot)** & *(print)** scripts create results
* *(workshop)** scripts contain explorations of the model

