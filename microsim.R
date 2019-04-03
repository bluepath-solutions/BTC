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
microsim <- function(POP, CAUC, HISP, ASIAN, BLACK, BMD_MEAN, BMD_SD, RA_INP,
                     FXR_INP, PARFXR_INP, SMOKER, ALCO, GLUCO_TX, BASECASEID,
                     BASECASETX, COSTINPT1, COSTINPT2, COSTOUTPT1, COSTOUTPT2,
                     COSTLTC1, COSTLTC2, COSTED1, COSTED2, COSTOTHER1, COSTOTHER2,
                     COSTPHARM1, COSTPHARM2, YEAR, CASE) {

# Setup Hashmap for lookups    
id_to_frax_hash <- hashmap(ID_lookup$ID, ID_lookup$`FRAX- HIP`)
MAX_HIP_FRACTURE_RATE <- max(ID_lookup$`FRAX- HIP`)
id_to_frax_major_hash <- hashmap(ID_lookup$ID,ID_lookup$`FRAX- MAJOR`)
MAX_MAJOR_FRACTURE_RATE <- max(ID_lookup$`FRAX- MAJOR`)
  
year <- YEAR

index <- which(age_probabilities$Year == year)

age_prob <- c(age_probabilities[index,2:37])

EXTRAPOLATION_FACTOR <- 1.0
# 1. Gather Inputs
# Input list
# 

# Population Size
population_size <- 100000 #10000000 #10000000/13
EXTRAPOLATION_FACTOR <- POP/population_size


#if(population_size >= 1000000) {
#  EXTRAPOLATION_FACTOR <- 20
#  population_size <- population_size*(1/EXTRAPOLATION_FACTOR)
#}

# Demographic Percentages
race_prob <- c(CAUC, 
               HISP, 
               ASIAN, 
               BLACK)

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
dxa_prob <- BASECASEID # 11.3% in paper ... 33.1% for one scenario

med_base_prob <-  BASECASETX #0.104 # THIS SHOULD DECAY LOGARITHMICALLY
print(med_base_prob)
# Treatment Mix
treatment_mix <-c(0.439, # Alendronate  PRIMARY (BISPHOSPHONATES?)
                  0.061,  # Ibandronate 150 MG
                  0.03,  # Risedronate
                  0.0,  # Ibandronate IV
                  0.093,  # Zoledronic   PRIMARY
                  0.292, # Denosumab    PRIMARY
                  0.0,  # Conjugated Estrogens/Bazedoxifene
                  0.074,  # Raloxifene
                  0.008,  # Forteo
                  0.003)  # Tymlos


# Monthly Costs
treatment_monthly_cost <-c(10.00,  # Alendronate PRIMARY 10.00
                           14.17,  # Ibandronate 150 MG
                           118.50, # Risedronate
                           80.00,  # Ibandronate IV
                           18.75,  # Zoledronic  PRIMARY
                           195.62, # Denosumab   PRIMARY
                           158.84, # Conjugated Estrogens/Bazedoxifene
                           69.30,  # Raloxifene
                           2997.90,# Forteo 
                           1625.00)# Tymlos

# Is this correct?

treatment_efficacy_hip <- c(0.65, # Alendronate Primary
                            0.73, # Ibandronate 150 MG
                            0.74, # Risedronate
                            0.59, # Ibandronate IV
                            0.59, # Zoledronic
                            0.61, # Denosumab
                            0.59, # Conjugated Estrogens/Bazedoxifene
                            0.59, # Raloxifene
                            0.25, # Forteo
                            0.41) # Tymlos

treatment_efficacy_other <- c(0.66, # Aledronate Primary
                              0.72, # Ibandronate 150 MG
                              0.71, # Risedronate
                              0.55, # Ibandronate IV
                              0.47, # Zoledronic
                              0.58, # Denosumab
                              0.59, # Conjugated Estrogens/Bazedoxifene
                              0.59, # Raloxifene
                              0.34, # Forteo
                              0.34) # Tymlos

# CONSTANTS

risk_factor_prob <- c(rheu_arth_prob, prev_fracture_prob, hist_fracture_prob,
                      smoker_prob, alcohol_prob, gluco_prob)


MEDICATION_COST <- treatment_mix %*% treatment_monthly_cost
HIP_FRACTURE_AVERAGE <- treatment_mix %*% treatment_efficacy_hip  
ANY_FRACTURE_AVERAGE <- treatment_mix %*% treatment_efficacy_other

# Weird Coefficent

dxa_cost <- 41.63

weird_coefficient <- c(25.892946, 26.700267, 27.525255, 28.376817, 29.276951,
                       30.224627, 31.221119, 32.207436, 33.237197, 34.256655,
                       35.256342, 36.291667, 37.283552, 38.213439, 39.112738,
                       39.977522, 40.762367, 41.395753, 41.962435, 42.487148,
                       43.018822, 43.619101, 44.170949, 44.581490, 44.882428,
                       45.124642, 45.392507)

weird_coefficient <- weird_coefficient*1000000/population_size

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

productivity_unit_cost <- 2445
caregiver_costs <- 1770.60

# 2. Create Population based on input parameters
# Simulate population
#set.seed(2)


age_index <- getAgeIndex(minimum_age,
                         maximum_age,
                         population_size,
                         age_prob,
                         age_cutoffs,
                         age_index_scores)

race_index <- getRaceIndex(population_size, 
                           race_prob,
                           race_categories,
                           race_index_scores)

bmd_index <- getBMDIndex(population_size,
                         bmdTScoremean,
                         bmdTScoreSD,
                         centering_mean,
                         bmd_cutoffs,
                         bmd_index_scores
)
risk_factor_index <- getRiskFactorIndex(population_size,
                                        risk_factor_prob)


index <- as.integer(age_index + race_index + bmd_index + risk_factor_index)

frax       <- id_to_frax_hash[[ index ]] # frax hip
frax[is.na(frax)] <- MAX_HIP_FRACTURE_RATE

frax_major <- id_to_frax_major_hash[[ index ]] # frax major
frax_major[is.na(frax_major)] <- MAX_MAJOR_FRACTURE_RATE


if(dxa_prob == 0) {
  dxa_scans <- replicate(population_size,0)
} else {
  dxa_scans    <- frax_major >= quantile(frax_major, 1 - dxa_prob)  
}
if(med_base_prob == 0 || getMedicationUtilization(med_base_prob, year) <= 0) {
  med_patients <- replicate(population_size, 0)
} else {
  med_patients <- frax_major >= quantile(frax_major, 1 - getMedicationUtilization(med_base_prob, year) )
}


samples <- runif(population_size)
any_fracture <- ifelse(med_patients,
                       samples < (ANY_FRACTURE_AVERAGE*(1-exp(-(-log(1-frax_major)/10)))),
                       samples < (1-exp(-(-log(1-frax_major)/10)))  )

samples <- runif(population_size)
hip_fracture <- ifelse(med_patients, 
                       samples < (HIP_FRACTURE_AVERAGE*(1-exp(-(-log(1-frax)/10)))),
                       samples < (1-exp(-(-log(1-frax)/10)))
)


other_fracture <- ifelse(!any_fracture,
                         F,
                         !hip_fracture)

# Should be more parameterized



total_other_fracture <- sum(other_fracture)


total_hip <- sum(hip_fracture) * MULTI_FRACTURE_FACTOR * weird_coefficient[year-2013]
total_shoulder <- fracture_breakdown[1] * total_other_fracture* MULTI_FRACTURE_FACTOR* weird_coefficient[year-2013]
total_vertebral <- fracture_breakdown[2] * total_other_fracture * MULTI_FRACTURE_FACTOR * weird_coefficient[year-2013]
total_forearm <- fracture_breakdown[3] * total_other_fracture* MULTI_FRACTURE_FACTOR* weird_coefficient[year-2013]

total_other <- HIP_FRACTURE_RATIO * total_hip - total_shoulder - total_vertebral - total_forearm
# May explore total_fractures further
total_fractures <- total_hip + total_shoulder + total_vertebral + total_forearm + total_other



total_dxa_cost <- sum(dxa_scans) * dxa_cost* weird_coefficient[year-2013]
total_med_cost <- sum(med_patients) * MEDICATION_COST * MEDICATION_ADHERENCE* weird_coefficient[year-2013]

total_inpatient_cost <- total_fractures * (1/MULTI_FRACTURE_FACTOR)*inpatient_wo_subsequent_fracture + 
                        total_fractures * ((MULTI_FRACTURE_FACTOR-1)/MULTI_FRACTURE_FACTOR)*inpatient_w_subsequent_fracture
total_outpatient_cost <- total_fractures * (1/MULTI_FRACTURE_FACTOR)*outpatient_wo_subsequent_fracture + 
                         total_fractures * ((MULTI_FRACTURE_FACTOR-1)/MULTI_FRACTURE_FACTOR) * outpatient_w_subsequent_fracture


total_ltc_cost <- total_fractures * (1/MULTI_FRACTURE_FACTOR)*ltc_wo_subsequent_fracture + 
                  total_fractures*((MULTI_FRACTURE_FACTOR-1)/MULTI_FRACTURE_FACTOR)*ltc_w_subsequent_fracture
total_ed_cost <- total_fractures * (1/MULTI_FRACTURE_FACTOR)*ed_wo_subsequent_fracture + total_fractures * ((MULTI_FRACTURE_FACTOR-1)/MULTI_FRACTURE_FACTOR)*ed_w_subsequent_fracture

total_other_cost <- total_fractures * (1/MULTI_FRACTURE_FACTOR)*other_wo_subsequent_fracture + 
                    total_fractures * ((MULTI_FRACTURE_FACTOR-1)/MULTI_FRACTURE_FACTOR)*other_w_subsequent_fracture
total_pharmacy_cost <- total_fractures * (1/MULTI_FRACTURE_FACTOR)*pharmacy_wo_subsequent_fracture + 
                       total_fractures * ((MULTI_FRACTURE_FACTOR-1)/MULTI_FRACTURE_FACTOR)*pharmacy_w_subsequent_fracture

total_productivity_losses <- total_fractures * productivity_unit_cost
total_caregiver_losses <- total_fractures *caregiver_costs

total_direct_cost <- total_dxa_cost + total_med_cost + total_inpatient_cost +
  total_outpatient_cost + total_ltc_cost + total_ed_cost +
  total_other_cost + total_pharmacy_cost

total_indirect_cost <- total_productivity_losses + total_caregiver_losses


grand_total <- total_direct_cost + total_indirect_cost


clinical_data <- data.frame(total_hip, total_shoulder, total_vertebral, 
                            total_forearm, total_other, total_fractures)

financial_data <- data.frame(total_dxa_cost, total_med_cost, total_inpatient_cost,
                             total_outpatient_cost, total_ltc_cost, total_ed_cost,
                             total_other_cost, total_pharmacy_cost, total_productivity_losses,
                             total_caregiver_losses, total_direct_cost, total_indirect_cost, 
                             grand_total)

packaged_data <- data.frame(clinical_data, financial_data)
return(packaged_data)#*EXTRAPOLATION_FACTOR)
}
