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
microsim <- function(POP, ASIAN, BMD_MEAN, BMD_SD, RA_INP,
                     FXR_INP, PARFXR_INP, SMOKER, ALCO, GLUCO_TX, BASECASEID,
                     BASECASETX, S1ID, S1TX, COSTINPT1, COSTINPT2, COSTOUTPT1, COSTOUTPT2,
                     COSTLTC1, COSTLTC2, COSTED1, COSTED2, COSTOTHER1, COSTOTHER2,
                     COSTPHARM1, COSTPHARM2, COSTPROD1, COSTPROD2, COSTCARE1,
                     COSTCARE2, YEAR, CASE,
                     COUNTRY, POPN_PROJECTION
                     ## new inputs so we can have multiple countries in the same app
                     # POPN_INPUTS, COST_INPUTS, SCENARIO_INPUTS,
                     # TREATMENT_MIX, TREATMENT_COSTS, TREATMENT_EFFICACY_HIP, TREATMENT_EFFICACY_OTHER,
                     # FRAX_FILEPATH, AGE_DBN_FILEPATH, POPN_PROJECTION_VECTOR
                     ) {
  
# Setup Hashmap for lookups   
## should move this outside of microsim so we're not looking up ~20 times without country changing
id_to_frax_hash <- hashmap(ID_lookup[[COUNTRY]]$ID, ID_lookup[[COUNTRY]]$`FRAX- HIP`)
MAX_HIP_FRACTURE_RATE <- max(ID_lookup[[COUNTRY]]$`FRAX- HIP`)
id_to_frax_major_hash <- hashmap(ID_lookup[[COUNTRY]]$ID,ID_lookup[[COUNTRY]]$`FRAX- MAJOR`)
MAX_MAJOR_FRACTURE_RATE <- max(ID_lookup[[COUNTRY]]$`FRAX- MAJOR`)

# Gather and Assign inputs
year <- YEAR
index <- which(age_probabilities[[COUNTRY]]$Year == year)
age_prob <- c(age_probabilities[[COUNTRY]][index,2:10])

# adherence stats
MEDICATION_ADHERENCE <- country_other_value(COUNTRY, 'adherence')
NON_ADHERENT_INCREASED_FRACTURE_RISK <- country_other_value(COUNTRY, 'nonadherentRisk')

# Population Size - THIS VALUE IS CURRENTLY FIXED, IT WILL NOT CHANGE WITH UI INPUTS
population_size <- 100000
EXTRAPOLATION_FACTOR <- POP/population_size


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


# Treatment Mix - THIS IS NOT DYNAMIC
# CHINA NOTE: for those with mix > 0 and have a generic, assuming that 1/2 people will
# get name brand, 1/2 will get generic. 

treatment_mix <- treat_mix[[COUNTRY]]




# Monthly Costs

treatment_monthly_cost <- treat_cost[[COUNTRY]] # will probably need to put in price_inflation factors in global.R
                             

treatment_efficacy_hip <- treat_efficacy_hip[[COUNTRY]]
                            

treatment_efficacy_other <- treat_efficacy_other[[COUNTRY]]

# CONSTANTS

risk_factor_prob <- c(rheu_arth_prob, prev_fracture_prob, hist_fracture_prob,
                      smoker_prob, alcohol_prob, gluco_prob)
risk_names <- c('arthritis', 'prevFrac', 'parentFrac', 'smoker', 'alcohol', 'gluco')

# .9 trt_efficacy, .3 adherence, .25 increased risk
HIP_FRACTURE_AVERAGE <- ((1 - (treatment_mix %*% treatment_efficacy_hip))*(1-MEDICATION_ADHERENCE)*(NON_ADHERENT_INCREASED_FRACTURE_RISK-1)) +
  (treatment_mix %*% treatment_efficacy_hip)
ANY_FRACTURE_AVERAGE <- ((1 - (treatment_mix %*% treatment_efficacy_other))*(1-MEDICATION_ADHERENCE)*(NON_ADHERENT_INCREASED_FRACTURE_RISK-1)) +
  (treatment_mix %*% treatment_efficacy_other)

MEDICATION_COST <- treatment_mix %*% treatment_monthly_cost
# HIP_FRACTURE_AVERAGE <- treatment_mix %*% treatment_efficacy_hip * MEDICATION_ADHERENCE +
#                         treatment_mix %*% treatment_efficacy_hip * (1 - MEDICATION_ADHERENCE) * NON_ADHERENT_INCREASED_FRACTURE_RISK
# ANY_FRACTURE_AVERAGE <- treatment_mix %*% treatment_efficacy_other * MEDICATION_ADHERENCE +
#                         treatment_mix %*% treatment_efficacy_other * (1 - MEDICATION_ADHERENCE) * NON_ADHERENT_INCREASED_FRACTURE_RISK   

# Weird Coefficent - This extrapolates the simulated population to the projected 
#                    population (of women).

# THIS IS NOT DYNAMIC
dxa_cost <- country_other_value(COUNTRY, 'dxaScreen')

weird_coefficient <- POPN_PROJECTION

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
age_index <- getAgeIndex(age_index_scores,
                         age_prob,
                         population_size)


bmd_index <- getBMDIndex(population_size,
                         bmdTScoremean,
                         bmdTScoreSD,
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

prev_fracs_per_yr <- sum(risk_factor_index$prev_fracture_incidence)*weird_coefficient[year - 2015]
prev_no_fracs_per_yr <- sum(!risk_factor_index$prev_fracture_incidence)*weird_coefficient[year - 2015]


# use bayes to get counts of populations 
prob_history_given_fracture <- (prob_fracture_given_previous_fractures*prev_fracture_prob)/(prob_fracture_given_no_previous_fractures*(1-prev_fracture_prob) + prob_fracture_given_previous_fractures*prev_fracture_prob)
prob_no_history_given_fracture <- (prob_fracture_given_no_previous_fractures*(1-prev_fracture_prob))/(prob_fracture_given_no_previous_fractures*(1-prev_fracture_prob) + prob_fracture_given_previous_fractures*prev_fracture_prob)
prob_history_given_fracture_s1 <- (prob_fracture_given_previous_fractures_s1*prev_fracture_prob)/(prob_fracture_given_no_previous_fractures_s1*(1-prev_fracture_prob) + prob_fracture_given_previous_fractures_s1*prev_fracture_prob)
prob_no_history_given_fracture_s1 <- (prob_fracture_given_no_previous_fractures_s1*(1-prev_fracture_prob))/(prob_fracture_given_no_previous_fractures_s1*(1-prev_fracture_prob) + prob_fracture_given_previous_fractures_s1*prev_fracture_prob)


# Use hip and other fracture data to extrapolate to other types of fractures
# There is excessive extrapolation here, but it follows the model.

total_other_fracture <- sum(other_fracture)
total_other_fracture_s1 <- sum(other_fracture_s1)


total_hip <- sum(hip_fracture) * MULTI_FRACTURE_FACTOR * weird_coefficient[year-2015]
total_shoulder <- fracture_breakdown[1] * total_other_fracture* MULTI_FRACTURE_FACTOR* weird_coefficient[year-2015]
total_vertebral <- fracture_breakdown[2] * total_other_fracture * MULTI_FRACTURE_FACTOR * weird_coefficient[year-2015]
total_forearm <- fracture_breakdown[3] * total_other_fracture* MULTI_FRACTURE_FACTOR* weird_coefficient[year-2015]

total_hip_s1 <- sum(hip_fracture_s1) * MULTI_FRACTURE_FACTOR * weird_coefficient[year-2015]
total_shoulder_s1 <- fracture_breakdown[1] * total_other_fracture_s1* MULTI_FRACTURE_FACTOR* weird_coefficient[year-2015]
total_vertebral_s1 <- fracture_breakdown[2] * total_other_fracture_s1 * MULTI_FRACTURE_FACTOR * weird_coefficient[year-2015]
total_forearm_s1 <- fracture_breakdown[3] * total_other_fracture_s1* MULTI_FRACTURE_FACTOR* weird_coefficient[year-2015]


total_other <- HIP_FRACTURE_RATIO * total_hip - total_shoulder - total_vertebral - total_forearm
total_fractures <- total_hip + total_shoulder + total_vertebral + total_forearm + total_other
# total_fractures is equivalent to (1 + HIP_FRACTURE_RATIO)*total_hip

total_other_s1 <- HIP_FRACTURE_RATIO * total_hip_s1 - total_shoulder_s1 - total_vertebral_s1 - total_forearm_s1
total_fractures_s1 <- total_hip_s1 + total_shoulder_s1 + total_vertebral_s1 + total_forearm_s1 + total_other_s1


total_fractures_with_previous_fracture <- total_fractures*prob_history_given_fracture
total_fractures_with_previous_fracture_s1 <- total_fractures_s1*prob_history_given_fracture_s1
total_fractures_wo_previous_fracture <- total_fractures*prob_no_history_given_fracture
total_fractures_wo_previous_fracture_s1 <- total_fractures_s1*prob_no_history_given_fracture_s1


# End of Clinical Data, Beginning of Financial Data
# Calculate Costs


# the 6 + 6*med_adhere is from the excel model

total_dxa_cost <- sum(dxa_scans) * dxa_cost* weird_coefficient[year-2015]
total_med_cost <- sum(med_patients) * MEDICATION_COST * (6 + 6*MEDICATION_ADHERENCE) * weird_coefficient[year-2015]

total_dxa_cost_s1 <- sum(dxa_scans_s1) * dxa_cost* weird_coefficient[year-2015]
total_med_cost_s1 <- sum(med_patients_s1) * MEDICATION_COST * (6 + 6*MEDICATION_ADHERENCE) * weird_coefficient[year-2015]

total_inpatient_cost <- getMultiFraxCost(total_fractures,
                                         MULTI_FRACTURE_FACTOR,
                                         inpatient_wo_subsequent_fracture,
                                         inpatient_w_subsequent_fracture)
total_inpatient_cost_s1 <- getMultiFraxCost(total_fractures_s1,
                                         MULTI_FRACTURE_FACTOR,
                                         inpatient_wo_subsequent_fracture,
                                         inpatient_w_subsequent_fracture)

### with new cost function ####

total_inpatient_with_prev_frac_cost <- getCostWP(total_fractures_with_previous_fracture, total_fractures_wo_previous_fracture,
                                                 MULTI_FRACTURE_FACTOR,
                                                 inpatient_wo_subsequent_fracture, inpatient_w_subsequent_fracture)
total_inpatient_with_prev_frac_cost_s1 <- getCostWP(total_fractures_with_previous_fracture_s1, total_fractures_wo_previous_fracture_s1,
                                                 MULTI_FRACTURE_FACTOR,
                                                 inpatient_wo_subsequent_fracture, inpatient_w_subsequent_fracture)
total_inpatient_wo_prev_frac_cost <- getCostWO(total_fractures_wo_previous_fracture, MULTI_FRACTURE_FACTOR, inpatient_wo_subsequent_fracture)
total_inpatient_wo_prev_frac_cost_s1 <- getCostWO(total_fractures_wo_previous_fracture_s1, MULTI_FRACTURE_FACTOR, inpatient_wo_subsequent_fracture)

## end ## 
total_outpatient_cost <- getMultiFraxCost(total_fractures,
                                         MULTI_FRACTURE_FACTOR,
                                         outpatient_wo_subsequent_fracture,
                                         outpatient_w_subsequent_fracture)
total_outpatient_cost_s1 <- getMultiFraxCost(total_fractures_s1,
                                          MULTI_FRACTURE_FACTOR,
                                          outpatient_wo_subsequent_fracture,
                                          outpatient_w_subsequent_fracture)


### with new cost function ####

total_outpatient_with_prev_frac_cost <- getCostWP(total_fractures_with_previous_fracture, total_fractures_wo_previous_fracture,
                                                 MULTI_FRACTURE_FACTOR,
                                                 outpatient_wo_subsequent_fracture, outpatient_w_subsequent_fracture)
total_outpatient_with_prev_frac_cost_s1 <- getCostWP(total_fractures_with_previous_fracture_s1, total_fractures_wo_previous_fracture_s1,
                                                    MULTI_FRACTURE_FACTOR,
                                                    outpatient_wo_subsequent_fracture, outpatient_w_subsequent_fracture)
total_outpatient_wo_prev_frac_cost <- getCostWO(total_fractures_wo_previous_fracture, MULTI_FRACTURE_FACTOR, outpatient_wo_subsequent_fracture)
total_outpatient_wo_prev_frac_cost_s1 <- getCostWO(total_fractures_wo_previous_fracture_s1, MULTI_FRACTURE_FACTOR, outpatient_wo_subsequent_fracture)

## end ## 

total_ltc_cost <- getMultiFraxCost(total_fractures,
                                   MULTI_FRACTURE_FACTOR,
                                   ltc_wo_subsequent_fracture,
                                   ltc_w_subsequent_fracture)
total_ltc_cost_s1 <- getMultiFraxCost(total_fractures_s1,
                                      MULTI_FRACTURE_FACTOR,
                                      ltc_wo_subsequent_fracture,
                                      ltc_w_subsequent_fracture)


### with new cost function ####

total_ltc_with_prev_frac_cost <- getCostWP(total_fractures_with_previous_fracture, total_fractures_wo_previous_fracture,
                                                  MULTI_FRACTURE_FACTOR,
                                                  ltc_wo_subsequent_fracture, ltc_w_subsequent_fracture)
total_ltc_with_prev_frac_cost_s1 <- getCostWP(total_fractures_with_previous_fracture_s1, total_fractures_wo_previous_fracture_s1,
                                                     MULTI_FRACTURE_FACTOR,
                                                     ltc_wo_subsequent_fracture, ltc_w_subsequent_fracture)
total_ltc_wo_prev_frac_cost <- getCostWO(total_fractures_wo_previous_fracture, MULTI_FRACTURE_FACTOR, ltc_wo_subsequent_fracture)
total_ltc_wo_prev_frac_cost_s1 <- getCostWO(total_fractures_wo_previous_fracture_s1, MULTI_FRACTURE_FACTOR, ltc_wo_subsequent_fracture)

## end ## 

total_ed_cost <- getMultiFraxCost(total_fractures,
                                  MULTI_FRACTURE_FACTOR,
                                  ed_wo_subsequent_fracture,
                                  ed_w_subsequent_fracture)
total_ed_cost_s1 <- getMultiFraxCost(total_fractures_s1,
                                     MULTI_FRACTURE_FACTOR,
                                     ed_wo_subsequent_fracture,
                                     ed_w_subsequent_fracture)


### with new cost function ####

total_ed_with_prev_frac_cost <- getCostWP(total_fractures_with_previous_fracture, total_fractures_wo_previous_fracture,
                                           MULTI_FRACTURE_FACTOR,
                                           ed_wo_subsequent_fracture, ed_w_subsequent_fracture)
total_ed_with_prev_frac_cost_s1 <- getCostWP(total_fractures_with_previous_fracture_s1, total_fractures_wo_previous_fracture_s1,
                                              MULTI_FRACTURE_FACTOR,
                                              ed_wo_subsequent_fracture, ed_w_subsequent_fracture)
total_ed_wo_prev_frac_cost <- getCostWO(total_fractures_wo_previous_fracture, MULTI_FRACTURE_FACTOR, ed_wo_subsequent_fracture)
total_ed_wo_prev_frac_cost_s1 <- getCostWO(total_fractures_wo_previous_fracture_s1, MULTI_FRACTURE_FACTOR, ed_wo_subsequent_fracture)

## end ## 

total_other_cost <- getMultiFraxCost(total_fractures,
                                 MULTI_FRACTURE_FACTOR,
                                 other_wo_subsequent_fracture,
                                 other_w_subsequent_fracture)
total_other_cost_s1 <- getMultiFraxCost(total_fractures_s1,
                                     MULTI_FRACTURE_FACTOR,
                                     other_wo_subsequent_fracture,
                                     other_w_subsequent_fracture)


### with new cost function ####

total_other_with_prev_frac_cost <- getCostWP(total_fractures_with_previous_fracture, total_fractures_wo_previous_fracture,
                                           MULTI_FRACTURE_FACTOR,
                                           other_wo_subsequent_fracture, other_w_subsequent_fracture)
total_other_with_prev_frac_cost_s1 <- getCostWP(total_fractures_with_previous_fracture_s1, total_fractures_wo_previous_fracture_s1,
                                              MULTI_FRACTURE_FACTOR,
                                              other_wo_subsequent_fracture, other_w_subsequent_fracture)
total_other_wo_prev_frac_cost <- getCostWO(total_fractures_wo_previous_fracture, MULTI_FRACTURE_FACTOR, other_wo_subsequent_fracture)
total_other_wo_prev_frac_cost_s1 <- getCostWO(total_fractures_wo_previous_fracture_s1, MULTI_FRACTURE_FACTOR, other_wo_subsequent_fracture)

## end ## 

total_pharmacy_cost <- getMultiFraxCost(total_fractures,
                                     MULTI_FRACTURE_FACTOR,
                                     pharmacy_wo_subsequent_fracture,
                                     pharmacy_w_subsequent_fracture)
total_pharmacy_cost_s1 <- getMultiFraxCost(total_fractures_s1,
                                        MULTI_FRACTURE_FACTOR,
                                        pharmacy_wo_subsequent_fracture,
                                        pharmacy_w_subsequent_fracture)


### with new cost function ####

total_pharmacy_with_prev_frac_cost <- getCostWP(total_fractures_with_previous_fracture, total_fractures_wo_previous_fracture,
                                           MULTI_FRACTURE_FACTOR,
                                           pharmacy_wo_subsequent_fracture, pharmacy_w_subsequent_fracture)
total_pharmacy_with_prev_frac_cost_s1 <- getCostWP(total_fractures_with_previous_fracture_s1, total_fractures_wo_previous_fracture_s1,
                                              MULTI_FRACTURE_FACTOR,
                                              pharmacy_wo_subsequent_fracture, pharmacy_w_subsequent_fracture)
total_pharmacy_wo_prev_frac_cost <- getCostWO(total_fractures_wo_previous_fracture, MULTI_FRACTURE_FACTOR, pharmacy_wo_subsequent_fracture)
total_pharmacy_wo_prev_frac_cost_s1 <- getCostWO(total_fractures_wo_previous_fracture_s1, MULTI_FRACTURE_FACTOR, pharmacy_wo_subsequent_fracture)

## end ## 

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
  
  ### with new cost function ####
  
  total_productivity_with_prev_frac_losses <- getCostWP(total_fractures_with_previous_fracture, total_fractures_wo_previous_fracture,
                                                  MULTI_FRACTURE_FACTOR,
                                                  productivity_wo_subsequent_fracture, productivity_w_subsequent_fracture)
  total_productivity_with_prev_frac_losses_s1 <- getCostWP(total_fractures_with_previous_fracture_s1, total_fractures_wo_previous_fracture_s1,
                                                     MULTI_FRACTURE_FACTOR,
                                                     productivity_wo_subsequent_fracture, productivity_w_subsequent_fracture)
  total_productivity_wo_prev_frac_losses <- getCostWO(total_fractures_wo_previous_fracture, MULTI_FRACTURE_FACTOR, productivity_wo_subsequent_fracture)
  total_productivity_wo_prev_frac_losses_s1 <- getCostWO(total_fractures_wo_previous_fracture_s1, MULTI_FRACTURE_FACTOR, productivity_wo_subsequent_fracture)

  ## end ## 
  

  #######################
  
  ### with new cost function ####
  
  total_caregiver_with_prev_frac_losses <- getCostWP(total_fractures_with_previous_fracture, total_fractures_wo_previous_fracture,
                                                             MULTI_FRACTURE_FACTOR,
                                                             caregiver_wo_subsequent_fracture, caregiver_w_subsequent_fracture)
  total_caregiver_with_prev_frac_losses_s1 <- getCostWP(total_fractures_with_previous_fracture_s1, total_fractures_wo_previous_fracture_s1,
                                                                MULTI_FRACTURE_FACTOR,
                                                                caregiver_wo_subsequent_fracture, caregiver_w_subsequent_fracture)
  total_caregiver_wo_prev_frac_losses <- getCostWO(total_fractures_wo_previous_fracture, MULTI_FRACTURE_FACTOR, caregiver_wo_subsequent_fracture)
  total_caregiver_wo_prev_frac_losses_s1 <- getCostWO(total_fractures_wo_previous_fracture_s1, MULTI_FRACTURE_FACTOR, caregiver_wo_subsequent_fracture)

  ## end ## 
  
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
# 
# ## YO YO YO, END THE YO YO YO SECTION. YO.

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


## finished cost data, reallocating primary multi fractures to secondary popn

# to account for the extra .226 fracs being attributed to primary popn instead of secondary
## YO YO YO! I'M GOING TO RENAME THESE TO {total_fractures...} so I don't have to create a lot of new
## objects and logic. YO YO YO!!
# adj_fractures_wo_previous_fracture <- total_fractures_wo_previous_fracture/MULTI_FRACTURE_FACTOR
# adj_fractures_wo_previous_fracture_s1 <- total_fractures_wo_previous_fracture_s1/MULTI_FRACTURE_FACTOR
# adj_fractures_with_previous_fracture <- total_fractures_with_previous_fracture + (total_fractures_wo_previous_fracture - adj_fractures_wo_previous_fracture)
# adj_fractures_with_previous_fracture_s1 <- total_fractures_with_previous_fracture_s1 + (total_fractures_wo_previous_fracture_s1 - adj_fractures_wo_previous_fracture_s1)
# 
# total_fractures_with_previous_fracture <- adj_fractures_with_previous_fracture
# total_fractures_with_previous_fracture_s1 <- adj_fractures_with_previous_fracture_s1
# total_fractures_wo_previous_fracture <- adj_fractures_wo_previous_fracture
# total_fractures_wo_previous_fracture_s1 <- adj_fractures_wo_previous_fracture_s1

## YO YO YO, END THE YO YO YO SECTION. YO.

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

