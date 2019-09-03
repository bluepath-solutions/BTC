############################################################################
# Osteoporosis Projection Model                                            #
# Date: Mar 21, 2019                                                       #
# Author: Max Feinberg                                                     # 
#         maxwell.a.feinberg@gmail.com                                     #
#                                                                          #
# This program simulates the effects of osteoporosis on a simulated        #
# population generated based upon user provided information and data       #
# from various studies and US population information.                      #  
#                                                                          #
# Based on the work presented in Healthcare Policy Changes in Osteoporosis #
# Can Improve Outcomes and Reduce Costs in the US by Lewiecki et. al.      #
#                                                                          #
############################################################################
# microsim <- function(POP, CAUC, HISP, ASIAN, BLACK, BMD_MEAN, BMD_SD, RA_INP,
#                      FXR_INP, PARFXR_INP, SMOKER, ALCO, GLUCO_TX, BASECASEID,
#                      BASECASETX, S1ID, S1TX, COSTINPT1, COSTINPT2, COSTOUTPT1, COSTOUTPT2,
#                      COSTLTC1, COSTLTC2, COSTED1, COSTED2, COSTOTHER1, COSTOTHER2,
#                      COSTPHARM1, COSTPHARM2, COSTPROD1, COSTPROD2, COSTCARE1,
#                      COSTCARE2, YEAR, CASE) {
microsim <- function(POP, ASIAN, BMD_MEAN, BMD_SD, RA_INP,
                     FXR_INP, PARFXR_INP, SMOKER, ALCO, GLUCO_TX, BASECASEID,
                     BASECASETX, S1ID, S1TX, COSTINPT1, COSTINPT2, COSTOUTPT1, COSTOUTPT2,
                     COSTLTC1, COSTLTC2, COSTED1, COSTED2, COSTOTHER1, COSTOTHER2,
                     COSTPHARM1, COSTPHARM2, COSTPROD1, COSTPROD2, COSTCARE1,
                     COSTCARE2, YEAR, CASE) {
# Setup Hashmap for lookups    
id_to_frax_hash <- hashmap(ID_lookup$ID, ID_lookup$`FRAX- HIP`)
MAX_HIP_FRACTURE_RATE <- max(ID_lookup$`FRAX- HIP`)
id_to_frax_major_hash <- hashmap(ID_lookup$ID,ID_lookup$`FRAX- MAJOR`)
MAX_MAJOR_FRACTURE_RATE <- max(ID_lookup$`FRAX- MAJOR`)

# Gather and Assign inputs
year <- YEAR
index <- which(age_probabilities$Year == year)
age_prob <- c(age_probabilities[index,2:52])

# Population Size - THIS VALUE IS CURRENTLY FIXED, IT WILL NOT CHANGE WITH UI INPUTS
population_size <- POP
if(POP < 100000){
  population_size <- POP
  EXTRAPOLATION_FACTOR <- 1.0
} else if(POP >= 100000) {
  population_size <- 100000
  EXTRAPOLATION_FACTOR <- POP/population_size
}
#EXTRAPOLATION_FACTOR <- POP/population_size

# Demographic Percentages
# race_prob <- c(CAUC, 
#                HISP, 
#                ASIAN, 
#                BLACK)

# Mean Bone Mineral Density (g/cm2)
bmdTScoremean <- BMD_MEAN

# STDEV Bone Mineral Density 
bmdTScoreSD <- BMD_SD

# Rheumatoid Arthritis
rheu_arth_prob <- RA_INP

# Previous Fracture
prev_fracture_prob <- FXR_INP

# Parent History of Hip Fracture
hist_fracture_prob <- PARFXR_INP

# Smoker Prob
smoker_prob <- SMOKER

# Alcoholics
alcohol_prob <- ALCO

# Glucocorticoid
gluco_prob <- GLUCO_TX

# 0.113 in Copy version
dxa_prob <- BASECASEID 
med_base_prob <-  BASECASETX  

dxa_prob_s1 <- S1ID
med_base_prob_s1 <- S1TX

# https://fred.stlouisfed.org/series/KORCPIALLMINMEI
# Korean CPI Jan 2012-Jan 2019, Jan 2006-Jan 2019
price_inflation_2012_2019 <- 104.24/96.184 
price_inflation_2012_2018 <- 103.42/93.07
price_inflation_2006_2019 <- 104.24/79.306


# Treatment Mix - THIS IS NOT DYNAMIC
treatment_mix <-c(0.167, # Alendronate  PRIMARY (BISPHOSPHONATES?) 0.161
                  0.098,  # Ibandronate 150 MG 0.092
                  0.209,  # Risedronate 0.285
                  0.249,  # Ibandronate IV 0.243
                  0.060,  # Zoledronic   PRIMARY 0.055
                  0.045, # Denosumab    PRIMARY 0.040
                  0.033,  # Conjugated Estrogens/Bazedoxifene 0.028
                  0.098,  # Raloxifene 0.092
                  0.009,  # Forteo 0.004
                  0.0) # Tymlos


# Monthly Costs
treatment_monthly_cost <- (price_inflation_2012_2019 * 
                            c(0.67,  # Alendronate PRIMARY 10.00
                            17.03,  # Ibandronate 150 MG 28.33
                            0.64, # Risedronate 212.54
                            47.94,  # Ibandronate IV 80.00
                            273.56,  # Zoledronic  PRIMARY 18.75
                            192.07, # Denosumab   PRIMARY 203.18
                            0.94, # Conjugated Estrogens/Bazedoxifene 176.79
                            0.63,  # Raloxifene 69.30
                            290.78,# Forteo  3426.50
                            1822.41))# Tymlos 1822.41

treatment_efficacy_hip <- c(0.65, # Alendronate Primary
                            0.73, # Ibandronate 150 MG
                            0.74, # Risedronate
                            0.59, # Ibandronate IV
                            0.59, # Zoledronic
                            0.61, # Denosumab
                            0.59, # Conjugated Estrogens/Bazedoxifene
                            0.59, # Raloxifene
                            0.25, # Forteo
                            0.25) # Tymlos

treatment_efficacy_other <- c(0.65, # Alendronate Primary
                              0.73, # Ibandronate 150 MG
                              0.74, # Risedronate
                              0.59, # Ibandronate IV
                              0.59, # Zoledronic
                              0.61, # Denosumab
                              0.59, # Conjugated Estrogens/Bazedoxifene
                              0.59, # Raloxifene
                              0.25, # Forteo
                              0.25) # Tymlos

# CONSTANTS

risk_factor_prob <- c(rheu_arth_prob, prev_fracture_prob, hist_fracture_prob,
                      smoker_prob, alcohol_prob, gluco_prob)
risk_names <- c('arthritis', 'prevFrac', 'parentFrac', 'smoker', 'alcohol', 'gluco')

MEDICATION_COST <- treatment_mix %*% treatment_monthly_cost
HIP_FRACTURE_AVERAGE <- treatment_mix %*% treatment_efficacy_hip * MEDICATION_ADHERENCE +
                        treatment_mix %*% treatment_efficacy_hip * (1 - MEDICATION_ADHERENCE) * NON_ADHERENT_INCREASED_FRACTURE_RISK
ANY_FRACTURE_AVERAGE <- treatment_mix %*% treatment_efficacy_other * MEDICATION_ADHERENCE +
                        treatment_mix %*% treatment_efficacy_other * (1 - MEDICATION_ADHERENCE) * NON_ADHERENT_INCREASED_FRACTURE_RISK   
## this is equal to almost .9 (~.8932)

# Weird Coefficent - This extrapolates the simulated population to the projected 
#                    Korean population (of women).

# THIS IS NOT DYNAMIC
dxa_cost <- 33.36 

# Population projection using data from korea government stats website.
# data and model located in "C:\Users\mjackson\Documents\BTC SK\SK_popn_projections.xlsx"
# uses a quadratic projection of census to account for predicted decline
# in Korean population.
weird_coefficient <- c(5072.137, 5097.536, # 2014, 2015
                       5121.37, 5143.64, 5164.346, 5183.488, 5201.066,
                       5217.079, 5231.528, 5244.413, 5255.734, 5265.491, # 2021-2025
                       5273.683, 5280.311, 5285.375, 5288.875, 5290.811,
                       5291.182, 5289.989, 5287.232, 5282.911, 5277.026, # 2031-2035
                       5269.576, 5260.562, 5249.984, 5237.842, 5224.136)

# Coefficient here extrapolates to census data
weird_coefficient <- weird_coefficient/weird_coefficient[1]

inpatient_wo_subsequent_fracture <- COSTINPT1 #9576
inpatient_w_subsequent_fracture <-  COSTINPT2 #16477

outpatient_wo_subsequent_fracture <- COSTOUTPT1#2843
outpatient_w_subsequent_fracture <-  COSTOUTPT2#4353

ltc_wo_subsequent_fracture <- COSTLTC1#4065
ltc_w_subsequent_fracture <-  COSTLTC2#8721

ed_wo_subsequent_fracture <- COSTED1#870
ed_w_subsequent_fracture <-  COSTED1#1217

other_wo_subsequent_fracture <- COSTOTHER1#2502
other_w_subsequent_fracture <-  COSTOTHER2#4295

pharmacy_wo_subsequent_fracture <- COSTPHARM1#2174
pharmacy_w_subsequent_fracture <-  COSTPHARM2#2488

productivity_wo_subsequent_fracture <- COSTPROD1#2445
productivity_w_subsequent_fracture <- COSTPROD2#2445

caregiver_wo_subsequent_fracture <- COSTCARE1#2445
caregiver_w_subsequent_fracture <- COSTCARE2#2445


# Simulate Population and Assign Index Scores
age_index <- getAgeIndex(minimum_age,
                         maximum_age,
                         population_size,
                         age_prob,
                         age_cutoffs,
                         age_index_scores)

# race_index <- getRaceIndex(population_size, 
#                            race_prob,
#                            race_categories,
#                            race_index_scores)

bmd_index <- getBMDIndex(population_size,
                         bmdTScoremean,
                         bmdTScoreSD,
                         centering_mean,
                         bmd_cutoffs,
                         bmd_index_scores
)

risk_factor_index <- getRiskFactorIndex(population_size,
                                       risk_factor_prob)


index <- as.integer(age_index + bmd_index + risk_factor_index$risk_factor_index) # + race_index

# This is where index scores are assigned
# THIS IS THE MOST TIME CONSUMING STEP
# Hashmaps were purposefully used here to minimize lookup time
# Direct FRAX calculations are not available as the model is proprietary.


frax       <- id_to_frax_hash[[ index ]] # frax hip
frax[is.na(frax)] <- MAX_HIP_FRACTURE_RATE

frax_major <- id_to_frax_major_hash[[ index ]] # frax major
frax_major[is.na(frax_major)] <- MAX_MAJOR_FRACTURE_RATE


# Determine Identification and Treatment Populations

dxa_scans <- getDXAScans(population_size,
                         frax_major,
                         dxa_prob) 

dxa_scans_s1 <- getDXAScans(population_size,
                            frax_major,
                            dxa_prob_s1) 


med_patients <- getMedPatients(frax_major,
                               med_base_prob,
                               year)

med_patients_s1 <- getMedPatients(frax_major,
                               med_base_prob_s1,
                               year)
# Determine Fractures

samples <- runif(population_size)

any_fracture <- getFracture(med_patients,
                            ANY_FRACTURE_AVERAGE,
                            frax_major,
                            samples)

any_fracture_s1 <- getFracture(med_patients_s1,
                            ANY_FRACTURE_AVERAGE, 
                            frax_major,
                            samples)

samples <- runif(population_size)

hip_fracture <- getFracture(med_patients,
                            HIP_FRACTURE_AVERAGE,
                            frax,
                            samples)

hip_fracture_s1 <- getFracture(med_patients_s1,
                            HIP_FRACTURE_AVERAGE,
                            frax,
                            samples)

other_fracture <- ifelse(!any_fracture,
                         F,
                         !hip_fracture)


other_fracture_s1 <- ifelse(!any_fracture_s1,
                            F,
                            !hip_fracture_s1)


fracture_given_previous_fractures <- risk_factor_index$prev_fracture_incidence & (any_fracture | hip_fracture)

fracture_given_previous_fractures_s1 <- risk_factor_index$prev_fracture_incidence & (any_fracture_s1 | hip_fracture_s1)

fracture_given_no_previous_fractures <- (!risk_factor_index$prev_fracture_incidence) & (any_fracture | hip_fracture)

fracture_given_no_previous_fractures_s1 <- (!risk_factor_index$prev_fracture_incidence) & (any_fracture_s1 | hip_fracture_s1)

# getting at total people with fractures

prob_fracture_given_previous_fractures <- sum(fracture_given_previous_fractures)/sum(risk_factor_index$prev_fracture_incidence)
prob_fracture_given_previous_fractures_s1 <- sum(fracture_given_previous_fractures_s1)/sum(risk_factor_index$prev_fracture_incidence)

prob_fracture_given_no_previous_fractures <- sum(fracture_given_no_previous_fractures)/sum(!risk_factor_index$prev_fracture_incidence)
prob_fracture_given_no_previous_fractures_s1 <- sum(fracture_given_no_previous_fractures_s1)/sum(!risk_factor_index$prev_fracture_incidence)

prev_fracs_per_yr <- sum(risk_factor_index$prev_fracture_incidence)*weird_coefficient[year - 2013]
prev_no_fracs_per_yr <- sum(!risk_factor_index$prev_fracture_incidence)*weird_coefficient[year - 2013]


# use bayes to get counts of populations 
prob_history_given_fracture <- (prob_fracture_given_previous_fractures*prev_fracture_prob)/(prob_fracture_given_no_previous_fractures*(1-prev_fracture_prob) + prob_fracture_given_previous_fractures*prev_fracture_prob)
prob_no_history_given_fracture <- (prob_fracture_given_no_previous_fractures*(1-prev_fracture_prob))/(prob_fracture_given_no_previous_fractures*(1-prev_fracture_prob) + prob_fracture_given_previous_fractures*prev_fracture_prob)
prob_history_given_fracture_s1 <- (prob_fracture_given_previous_fractures_s1*prev_fracture_prob)/(prob_fracture_given_no_previous_fractures_s1*(1-prev_fracture_prob) + prob_fracture_given_previous_fractures_s1*prev_fracture_prob)
prob_no_history_given_fracture_s1 <- (prob_fracture_given_no_previous_fractures_s1*(1-prev_fracture_prob))/(prob_fracture_given_no_previous_fractures_s1*(1-prev_fracture_prob) + prob_fracture_given_previous_fractures_s1*prev_fracture_prob)


# Use hip and other fracture data to extrapolate to other types of fractures
# There is excessive extrapolation here, but it follows the model.

total_other_fracture <- sum(other_fracture)
total_other_fracture_s1 <- sum(other_fracture_s1)


total_hip <- sum(hip_fracture) * MULTI_FRACTURE_FACTOR * weird_coefficient[year-2013]
total_shoulder <- fracture_breakdown[1] * total_other_fracture* MULTI_FRACTURE_FACTOR* weird_coefficient[year-2013]
total_vertebral <- fracture_breakdown[2] * total_other_fracture * MULTI_FRACTURE_FACTOR * weird_coefficient[year-2013]
total_forearm <- fracture_breakdown[3] * total_other_fracture* MULTI_FRACTURE_FACTOR* weird_coefficient[year-2013]

total_hip_s1 <- sum(hip_fracture_s1) * MULTI_FRACTURE_FACTOR * weird_coefficient[year-2013]
total_shoulder_s1 <- fracture_breakdown[1] * total_other_fracture_s1* MULTI_FRACTURE_FACTOR* weird_coefficient[year-2013]
total_vertebral_s1 <- fracture_breakdown[2] * total_other_fracture_s1 * MULTI_FRACTURE_FACTOR * weird_coefficient[year-2013]
total_forearm_s1 <- fracture_breakdown[3] * total_other_fracture_s1* MULTI_FRACTURE_FACTOR* weird_coefficient[year-2013]


total_other <- HIP_FRACTURE_RATIO * total_hip - total_shoulder - total_vertebral - total_forearm
total_fractures <- total_hip + total_shoulder + total_vertebral + total_forearm + total_other
# total_fractures is equivalent to (1 + HIP_FRACTURE_RATIO)*total_hip

total_other_s1 <- HIP_FRACTURE_RATIO * total_hip_s1 - total_shoulder_s1 - total_vertebral_s1 - total_forearm_s1
total_fractures_s1 <- total_hip_s1 + total_shoulder_s1 + total_vertebral_s1 + total_forearm_s1 + total_other_s1


total_fractures_with_previous_fracture <- total_fractures*prob_history_given_fracture
total_fractures_with_previous_fracture_s1 <- total_fractures_s1*prob_history_given_fracture_s1
total_fractures_wo_previous_fracture <- total_fractures*prob_no_history_given_fracture
total_fractures_wo_previous_fracture_s1 <- total_fractures_s1*prob_no_history_given_fracture_s1

# to account for the extra .226 fracs being attributed to primary popn instead of secondary
## YO YO YO! I'M GOING TO RENAME THESE TO {total_fractures...} so I don't have to create a lot of new
## objects and logic. YO YO YO!!
adj_fractures_wo_previous_fracture <- total_fractures_wo_previous_fracture/MULTI_FRACTURE_FACTOR
adj_fractures_wo_previous_fracture_s1 <- total_fractures_wo_previous_fracture_s1/MULTI_FRACTURE_FACTOR
adj_fractures_with_previous_fracture <- total_fractures_with_previous_fracture + (total_fractures_wo_previous_fracture - adj_fractures_wo_previous_fracture)
adj_fractures_with_previous_fracture_s1 <- total_fractures_with_previous_fracture_s1 + (total_fractures_wo_previous_fracture_s1 - adj_fractures_wo_previous_fracture_s1)

total_fractures_with_previous_fracture <- adj_fractures_with_previous_fracture
total_fractures_with_previous_fracture_s1 <- adj_fractures_with_previous_fracture_s1
total_fractures_wo_previous_fracture <- adj_fractures_wo_previous_fracture
total_fractures_wo_previous_fracture_s1 <- adj_fractures_wo_previous_fracture_s1

## YO YO YO, END THE YO YO YO SECTION. YO.


# End of Clinical Data, Beginning of Financial Data
# Calculate Costs


# the 6 + 6*med_adhere is from the excel model

total_dxa_cost <- sum(dxa_scans) * dxa_cost* weird_coefficient[year-2013]
total_med_cost <- sum(med_patients) * MEDICATION_COST * (6 + 6*MEDICATION_ADHERENCE) * weird_coefficient[year-2013]

total_dxa_cost_s1 <- sum(dxa_scans_s1) * dxa_cost* weird_coefficient[year-2013]
total_med_cost_s1 <- sum(med_patients_s1) * MEDICATION_COST * (6 + 6*MEDICATION_ADHERENCE) * weird_coefficient[year-2013]

total_inpatient_cost <- getMultiFraxCost(total_fractures,
                                         MULTI_FRACTURE_FACTOR,
                                         inpatient_wo_subsequent_fracture,
                                         inpatient_w_subsequent_fracture)
total_inpatient_cost_s1 <- getMultiFraxCost(total_fractures_s1,
                                         MULTI_FRACTURE_FACTOR,
                                         inpatient_wo_subsequent_fracture,
                                         inpatient_w_subsequent_fracture)

total_inpatient_with_prev_frac_cost <- getMultiFraxCost(total_fractures_with_previous_fracture,
                                                        MULTI_FRACTURE_FACTOR,
                                                        inpatient_wo_subsequent_fracture,
                                                        inpatient_w_subsequent_fracture)
total_inpatient_with_prev_frac_cost_s1 <- getMultiFraxCost(total_fractures_with_previous_fracture_s1,
                                                        MULTI_FRACTURE_FACTOR,
                                                        inpatient_wo_subsequent_fracture,
                                                        inpatient_w_subsequent_fracture)

total_inpatient_wo_prev_frac_cost <- getMultiFraxCost(total_fractures_wo_previous_fracture,
                                                        MULTI_FRACTURE_FACTOR,
                                                        inpatient_wo_subsequent_fracture,
                                                        inpatient_w_subsequent_fracture)
total_inpatient_wo_prev_frac_cost_s1 <- getMultiFraxCost(total_fractures_wo_previous_fracture_s1,
                                                           MULTI_FRACTURE_FACTOR,
                                                           inpatient_wo_subsequent_fracture,
                                                           inpatient_w_subsequent_fracture)

total_outpatient_cost <- getMultiFraxCost(total_fractures,
                                         MULTI_FRACTURE_FACTOR,
                                         outpatient_wo_subsequent_fracture,
                                         outpatient_w_subsequent_fracture)
total_outpatient_cost_s1 <- getMultiFraxCost(total_fractures_s1,
                                          MULTI_FRACTURE_FACTOR,
                                          outpatient_wo_subsequent_fracture,
                                          outpatient_w_subsequent_fracture)

total_outpatient_with_prev_frac_cost <- getMultiFraxCost(total_fractures_with_previous_fracture,
                                          MULTI_FRACTURE_FACTOR,
                                          outpatient_wo_subsequent_fracture,
                                          outpatient_w_subsequent_fracture)
total_outpatient_with_prev_frac_cost_s1 <- getMultiFraxCost(total_fractures_with_previous_fracture_s1,
                                                         MULTI_FRACTURE_FACTOR,
                                                         outpatient_wo_subsequent_fracture,
                                                         outpatient_w_subsequent_fracture)

total_outpatient_wo_prev_frac_cost <- getMultiFraxCost(total_fractures_wo_previous_fracture,
                                                         MULTI_FRACTURE_FACTOR,
                                                         outpatient_wo_subsequent_fracture,
                                                         outpatient_w_subsequent_fracture)
total_outpatient_wo_prev_frac_cost_s1 <- getMultiFraxCost(total_fractures_wo_previous_fracture_s1,
                                                            MULTI_FRACTURE_FACTOR,
                                                            outpatient_wo_subsequent_fracture,
                                                            outpatient_w_subsequent_fracture)

total_ltc_cost <- getMultiFraxCost(total_fractures,
                                   MULTI_FRACTURE_FACTOR,
                                   ltc_wo_subsequent_fracture,
                                   ltc_w_subsequent_fracture)
total_ltc_cost_s1 <- getMultiFraxCost(total_fractures_s1,
                                      MULTI_FRACTURE_FACTOR,
                                      ltc_wo_subsequent_fracture,
                                      ltc_w_subsequent_fracture)

total_ltc_with_prev_frac_cost <- getMultiFraxCost(total_fractures_with_previous_fracture,
                                   MULTI_FRACTURE_FACTOR,
                                   ltc_wo_subsequent_fracture,
                                   ltc_w_subsequent_fracture)
total_ltc_with_prev_frac_cost_s1 <- getMultiFraxCost(total_fractures_with_previous_fracture_s1,
                                                  MULTI_FRACTURE_FACTOR,
                                                  ltc_wo_subsequent_fracture,
                                                  ltc_w_subsequent_fracture)

total_ltc_wo_prev_frac_cost <- getMultiFraxCost(total_fractures_wo_previous_fracture,
                                                  MULTI_FRACTURE_FACTOR,
                                                  ltc_wo_subsequent_fracture,
                                                  ltc_w_subsequent_fracture)
total_ltc_wo_prev_frac_cost_s1 <- getMultiFraxCost(total_fractures_wo_previous_fracture_s1,
                                                     MULTI_FRACTURE_FACTOR,
                                                     ltc_wo_subsequent_fracture,
                                                     ltc_w_subsequent_fracture)

total_ed_cost <- getMultiFraxCost(total_fractures,
                                  MULTI_FRACTURE_FACTOR,
                                  ed_wo_subsequent_fracture,
                                  ed_w_subsequent_fracture)
total_ed_cost_s1 <- getMultiFraxCost(total_fractures_s1,
                                     MULTI_FRACTURE_FACTOR,
                                     ed_wo_subsequent_fracture,
                                     ed_w_subsequent_fracture)

total_ed_with_prev_frac_cost <- getMultiFraxCost(total_fractures_with_previous_fracture,
                                  MULTI_FRACTURE_FACTOR,
                                  ed_wo_subsequent_fracture,
                                  ed_w_subsequent_fracture)

total_ed_with_prev_frac_cost_s1 <- getMultiFraxCost(total_fractures_with_previous_fracture_s1,
                                                 MULTI_FRACTURE_FACTOR,
                                                 ed_wo_subsequent_fracture,
                                                 ed_w_subsequent_fracture)

total_ed_wo_prev_frac_cost <- getMultiFraxCost(total_fractures_wo_previous_fracture,
                                                 MULTI_FRACTURE_FACTOR,
                                                 ed_wo_subsequent_fracture,
                                                 ed_w_subsequent_fracture)

total_ed_wo_prev_frac_cost_s1 <- getMultiFraxCost(total_fractures_wo_previous_fracture_s1,
                                                    MULTI_FRACTURE_FACTOR,
                                                    ed_wo_subsequent_fracture,
                                                    ed_w_subsequent_fracture)

total_other_cost <- getMultiFraxCost(total_fractures,
                                 MULTI_FRACTURE_FACTOR,
                                 other_wo_subsequent_fracture,
                                 other_w_subsequent_fracture)
total_other_cost_s1 <- getMultiFraxCost(total_fractures_s1,
                                     MULTI_FRACTURE_FACTOR,
                                     other_wo_subsequent_fracture,
                                     other_w_subsequent_fracture)

total_other_with_prev_frac_cost <- getMultiFraxCost(total_fractures_with_previous_fracture,
                                     MULTI_FRACTURE_FACTOR,
                                     other_wo_subsequent_fracture,
                                     other_w_subsequent_fracture)

total_other_with_prev_frac_cost_s1 <- getMultiFraxCost(total_fractures_with_previous_fracture_s1,
                                                    MULTI_FRACTURE_FACTOR,
                                                    other_wo_subsequent_fracture,
                                                    other_w_subsequent_fracture)

total_other_wo_prev_frac_cost <- getMultiFraxCost(total_fractures_wo_previous_fracture,
                                                    MULTI_FRACTURE_FACTOR,
                                                    other_wo_subsequent_fracture,
                                                    other_w_subsequent_fracture)

total_other_wo_prev_frac_cost_s1 <- getMultiFraxCost(total_fractures_wo_previous_fracture_s1,
                                                       MULTI_FRACTURE_FACTOR,
                                                       other_wo_subsequent_fracture,
                                                       other_w_subsequent_fracture)

total_pharmacy_cost <- getMultiFraxCost(total_fractures,
                                     MULTI_FRACTURE_FACTOR,
                                     pharmacy_wo_subsequent_fracture,
                                     pharmacy_w_subsequent_fracture)
total_pharmacy_cost_s1 <- getMultiFraxCost(total_fractures_s1,
                                        MULTI_FRACTURE_FACTOR,
                                        pharmacy_wo_subsequent_fracture,
                                        pharmacy_w_subsequent_fracture)

total_pharmacy_with_prev_frac_cost <- getMultiFraxCost(total_fractures_with_previous_fracture,
                                        MULTI_FRACTURE_FACTOR,
                                        pharmacy_wo_subsequent_fracture,
                                        pharmacy_w_subsequent_fracture)
total_pharmacy_with_prev_frac_cost_s1 <- getMultiFraxCost(total_fractures_with_previous_fracture_s1,
                                                       MULTI_FRACTURE_FACTOR,
                                                       pharmacy_wo_subsequent_fracture,
                                                       pharmacy_w_subsequent_fracture)

total_pharmacy_wo_prev_frac_cost <- getMultiFraxCost(total_fractures_wo_previous_fracture,
                                                       MULTI_FRACTURE_FACTOR,
                                                       pharmacy_wo_subsequent_fracture,
                                                       pharmacy_w_subsequent_fracture)
total_pharmacy_wo_prev_frac_cost_s1 <- getMultiFraxCost(total_fractures_wo_previous_fracture_s1,
                                                          MULTI_FRACTURE_FACTOR,
                                                          pharmacy_wo_subsequent_fracture,
                                                          pharmacy_w_subsequent_fracture)

# Indirect Costs can be turned off
if(CASE) {
  total_productivity_losses <- getMultiFraxCost(total_fractures,
                                                MULTI_FRACTURE_FACTOR,
                                                productivity_wo_subsequent_fracture,
                                                productivity_w_subsequent_fracture)
  
  total_productivity_losses_s1 <- getMultiFraxCost(total_fractures_s1,
                                                   MULTI_FRACTURE_FACTOR,
                                                   productivity_wo_subsequent_fracture,
                                                   productivity_w_subsequent_fracture)
  
  total_caregiver_losses <- getMultiFraxCost(total_fractures,
                                             MULTI_FRACTURE_FACTOR,
                                             caregiver_wo_subsequent_fracture,
                                             caregiver_w_subsequent_fracture)
  
  total_caregiver_losses_s1 <- getMultiFraxCost(total_fractures_s1,
                                                MULTI_FRACTURE_FACTOR,
                                                caregiver_wo_subsequent_fracture,
                                                caregiver_w_subsequent_fracture) 
  ########################
  total_productivity_with_prev_frac_losses <- getMultiFraxCost(total_fractures_with_previous_fracture,
                                                MULTI_FRACTURE_FACTOR,
                                                productivity_wo_subsequent_fracture,
                                                productivity_w_subsequent_fracture)

  total_productivity_with_prev_frac_losses_s1 <- getMultiFraxCost(total_fractures_with_previous_fracture_s1,
                                                   MULTI_FRACTURE_FACTOR,
                                                   productivity_wo_subsequent_fracture,
                                                   productivity_w_subsequent_fracture)

  total_caregiver_with_prev_frac_losses <- getMultiFraxCost(total_fractures_with_previous_fracture,
                                             MULTI_FRACTURE_FACTOR,
                                             caregiver_wo_subsequent_fracture,
                                             caregiver_w_subsequent_fracture)

  total_caregiver_with_prev_frac_losses_s1 <- getMultiFraxCost(total_fractures_with_previous_fracture_s1,
                                                MULTI_FRACTURE_FACTOR,
                                                caregiver_wo_subsequent_fracture,
                                                caregiver_w_subsequent_fracture)
  #######################
  total_productivity_wo_prev_frac_losses <- getMultiFraxCost(total_fractures_wo_previous_fracture,
                                                               MULTI_FRACTURE_FACTOR,
                                                               productivity_wo_subsequent_fracture,
                                                               productivity_w_subsequent_fracture)

  total_productivity_wo_prev_frac_losses_s1 <- getMultiFraxCost(total_fractures_wo_previous_fracture_s1,
                                                                  MULTI_FRACTURE_FACTOR,
                                                                  productivity_wo_subsequent_fracture,
                                                                  productivity_w_subsequent_fracture)

  total_caregiver_wo_prev_frac_losses <- getMultiFraxCost(total_fractures_wo_previous_fracture,
                                                            MULTI_FRACTURE_FACTOR,
                                                            caregiver_wo_subsequent_fracture,
                                                            caregiver_w_subsequent_fracture)

  total_caregiver_wo_prev_frac_losses_s1 <- getMultiFraxCost(total_fractures_wo_previous_fracture_s1,
                                                               MULTI_FRACTURE_FACTOR,
                                                               caregiver_wo_subsequent_fracture,
                                                               caregiver_w_subsequent_fracture)
} else {
  total_productivity_losses <- 0
  
  total_productivity_losses_s1 <- 0
  
  total_caregiver_losses <- 0
  
  total_caregiver_losses_s1 <- 0 
  
  ################################
  total_productivity_with_prev_frac_losses <- 0

  total_productivity_with_prev_frac_losses_s1 <- 0

  total_caregiver_with_prev_frac_losses <- 0

  total_caregiver_with_prev_frac_losses_s1 <- 0

  ###############################
  total_productivity_wo_prev_frac_losses <- 0

  total_productivity_wo_prev_frac_losses_s1 <- 0

  total_caregiver_wo_prev_frac_losses <- 0

  total_caregiver_wo_prev_frac_losses_s1 <- 0
}

total_direct_cost <- total_dxa_cost + total_med_cost + total_inpatient_cost +
                     total_outpatient_cost + total_ltc_cost + total_ed_cost +
                     total_other_cost + total_pharmacy_cost

total_direct_cost_s1 <- total_dxa_cost_s1 + total_med_cost_s1 + total_inpatient_cost_s1 +
                     total_outpatient_cost_s1 + total_ltc_cost_s1 + total_ed_cost_s1 +
                     total_other_cost_s1 + total_pharmacy_cost_s1

total_direct_with_prev_frac_cost <- (total_dxa_cost + total_med_cost)*(total_fractures_with_previous_fracture/(total_fractures_with_previous_fracture+total_fractures_wo_previous_fracture)) + 
                                    total_inpatient_with_prev_frac_cost +
                                    total_outpatient_with_prev_frac_cost + total_ltc_with_prev_frac_cost + total_ed_with_prev_frac_cost +
                                    total_other_with_prev_frac_cost + total_pharmacy_with_prev_frac_cost
total_direct_with_prev_frac_cost_s1 <- (total_dxa_cost_s1 + total_med_cost_s1)*(total_fractures_with_previous_fracture_s1/(total_fractures_with_previous_fracture_s1+total_fractures_wo_previous_fracture_s1)) + 
                        total_inpatient_with_prev_frac_cost_s1 +
                        total_outpatient_with_prev_frac_cost_s1 + total_ltc_with_prev_frac_cost_s1 + total_ed_with_prev_frac_cost_s1 +
                        total_other_with_prev_frac_cost_s1 + total_pharmacy_with_prev_frac_cost_s1

total_direct_wo_prev_frac_cost <- (total_dxa_cost + total_med_cost)*(total_fractures_wo_previous_fracture/(total_fractures_wo_previous_fracture+total_fractures_with_previous_fracture)) + 
                                    total_inpatient_wo_prev_frac_cost +
                                    total_outpatient_wo_prev_frac_cost + total_ltc_wo_prev_frac_cost + total_ed_wo_prev_frac_cost +
                                    total_other_wo_prev_frac_cost + total_pharmacy_wo_prev_frac_cost
total_direct_wo_prev_frac_cost_s1 <- (total_dxa_cost_s1 + total_med_cost_s1)*(total_fractures_wo_previous_fracture_s1/(total_fractures_wo_previous_fracture_s1+total_fractures_with_previous_fracture_s1)) + 
                                        total_inpatient_wo_prev_frac_cost_s1 +
                                        total_outpatient_wo_prev_frac_cost_s1 + total_ltc_wo_prev_frac_cost_s1 + total_ed_wo_prev_frac_cost_s1 +
                                        total_other_wo_prev_frac_cost_s1 + total_pharmacy_wo_prev_frac_cost_s1

total_indirect_cost <-    total_productivity_losses + total_caregiver_losses
total_indirect_cost_s1 <- total_productivity_losses_s1 + total_caregiver_losses_s1

total_indirect_with_prev_frac_cost <-    total_productivity_with_prev_frac_losses + total_caregiver_with_prev_frac_losses
total_indirect_with_prev_frac_cost_s1 <- total_productivity_with_prev_frac_losses_s1 + total_caregiver_with_prev_frac_losses_s1

total_indirect_wo_prev_frac_cost <-    total_productivity_wo_prev_frac_losses + total_caregiver_wo_prev_frac_losses
total_indirect_wo_prev_frac_cost_s1 <- total_productivity_wo_prev_frac_losses_s1 + total_caregiver_wo_prev_frac_losses_s1

grand_total <- total_direct_cost + total_indirect_cost
grand_total_s1 <- total_direct_cost_s1 + total_indirect_cost_s1

grand_total_with_prev_frac <- total_direct_with_prev_frac_cost + total_indirect_with_prev_frac_cost
grand_total_with_prev_frac_s1 <- total_direct_with_prev_frac_cost_s1 + total_indirect_with_prev_frac_cost_s1

grand_total_wo_prev_frac <- total_direct_wo_prev_frac_cost + total_indirect_wo_prev_frac_cost
grand_total_wo_prev_frac_s1 <- total_direct_wo_prev_frac_cost_s1 + total_indirect_wo_prev_frac_cost_s1

## put data frames together
clinical_data <- data.frame(total_hip, total_shoulder, total_vertebral, 
                            total_forearm, total_other, total_fractures)

financial_data <- data.frame(total_dxa_cost, total_med_cost, total_inpatient_cost,
                             total_outpatient_cost, total_ltc_cost, total_ed_cost,
                             total_other_cost, total_pharmacy_cost, total_productivity_losses,
                             total_caregiver_losses, total_direct_cost, total_indirect_cost, 
                             grand_total)

clinical_data_s1 <- data.frame(total_hip_s1, total_shoulder_s1, total_vertebral_s1, 
                            total_forearm_s1, total_other_s1, total_fractures_s1)

financial_data_s1 <- data.frame(total_dxa_cost_s1, total_med_cost_s1, total_inpatient_cost_s1,
                             total_outpatient_cost_s1, total_ltc_cost_s1, total_ed_cost_s1,
                             total_other_cost_s1, total_pharmacy_cost_s1, total_productivity_losses_s1,
                             total_caregiver_losses_s1, total_direct_cost_s1, total_indirect_cost_s1, 
                             grand_total_s1)


prob_data <- data.frame(prob_fracture_given_previous_fractures, prob_fracture_given_no_previous_fractures)
prob_data_s1 <- data.frame(prob_fracture_given_previous_fractures_s1, prob_fracture_given_no_previous_fractures_s1)


prev_frac_data <- data.frame(prev_fracs_per_yr,
                              total_fractures_with_previous_fracture,
                             total_inpatient_with_prev_frac_cost, total_outpatient_with_prev_frac_cost, total_ltc_with_prev_frac_cost,
                             total_ed_with_prev_frac_cost, total_other_with_prev_frac_cost, total_pharmacy_with_prev_frac_cost,
                             total_productivity_with_prev_frac_losses, total_caregiver_with_prev_frac_losses, total_direct_with_prev_frac_cost,
                             total_indirect_with_prev_frac_cost, grand_total_with_prev_frac,
                             #
                             total_fractures_with_previous_fracture_s1, 
                             total_inpatient_with_prev_frac_cost_s1, total_outpatient_with_prev_frac_cost_s1, total_ltc_with_prev_frac_cost_s1,
                             total_ed_with_prev_frac_cost_s1, total_other_with_prev_frac_cost_s1, total_pharmacy_with_prev_frac_cost_s1,
                             total_productivity_with_prev_frac_losses_s1, total_caregiver_with_prev_frac_losses_s1, total_direct_with_prev_frac_cost_s1,
                             total_indirect_with_prev_frac_cost_s1, grand_total_with_prev_frac_s1)

no_prev_frac_data <- data.frame(prev_no_fracs_per_yr,
                                total_fractures_wo_previous_fracture,
                             total_inpatient_wo_prev_frac_cost, total_outpatient_wo_prev_frac_cost, total_ltc_wo_prev_frac_cost,
                             total_ed_wo_prev_frac_cost, total_other_wo_prev_frac_cost, total_pharmacy_wo_prev_frac_cost,
                             total_productivity_wo_prev_frac_losses, total_caregiver_wo_prev_frac_losses, total_direct_wo_prev_frac_cost,
                             total_indirect_wo_prev_frac_cost, grand_total_wo_prev_frac,
                             #
                             total_fractures_wo_previous_fracture_s1,
                             total_inpatient_wo_prev_frac_cost_s1, total_outpatient_wo_prev_frac_cost_s1, total_ltc_wo_prev_frac_cost_s1,
                             total_ed_wo_prev_frac_cost_s1, total_other_wo_prev_frac_cost_s1, total_pharmacy_wo_prev_frac_cost_s1,
                             total_productivity_wo_prev_frac_losses_s1, total_caregiver_wo_prev_frac_losses_s1, total_direct_wo_prev_frac_cost_s1,
                             total_indirect_wo_prev_frac_cost_s1, grand_total_wo_prev_frac_s1)


packaged_data <- data.frame(clinical_data, financial_data, clinical_data_s1, financial_data_s1, prev_frac_data, no_prev_frac_data)*EXTRAPOLATION_FACTOR 
packaged_data <- data.frame(packaged_data, prob_data, prob_data_s1,
                            prob_history_given_fracture, prob_no_history_given_fracture,
                            prob_history_given_fracture_s1, prob_no_history_given_fracture_s1)

return(packaged_data)
}

