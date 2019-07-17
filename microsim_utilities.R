############################################################################
# Osteoporosis Projection Model                                            #
# Date: Mar 21, 2019                                                       #
# Author: Max Feinberg                                                     # 
#         maxwell.a.feinberg@gmail.com                                     #
#                                                                          #
# This file contains helper functions for the primary microsimulation      #
# function.                                                                #
############################################################################
# getAgeIndex
#  @param MINIMUM_AGE int, the minimum age to be included in the population
#  @param MAXIMUM_AGE int, the maximum age to be included in the population
#  @param POPULATION_SIZE int, the total number of people to be generated
#  @param AGE_DISTRIBUTION list, a list containing the probability distribution
#                                over the ages, should be same length as the age
#                                range, currently from Census data
#  @param AGE_CUTOFFS list, a list containing the age cutoffs for indexing, currently
#                     currently from the simplified FRAX model
#  @param AGE_INDEX_SCORES, indexing values that correspond to the AGE_CUTOFFS list,
#                     currently from the simplified FRAX model
#  @return ageIndexList, returns a list of size POPULATION_SIZE with the age index 
#                        scores corresponding to the generated population
#  Generates an arbitrary population according to provided ranges and distributions
#  and returns a list with the FRAX age index score.
getAgeIndex <- function(MINIMUM_AGE, 
                      MAXIMUM_AGE, 
                      POPULATION_SIZE,
                      AGE_DISTRIBUTION,
                      AGE_CUTOFFS,
                      AGE_INDEX_SCORES) {
  
  age_array <- sample(MINIMUM_AGE:MAXIMUM_AGE, 
                      size=POPULATION_SIZE,
                      replace=TRUE,
                      prob=AGE_DISTRIBUTION)

  
  age_array[age_array ==  AGE_CUTOFFS[1]] = AGE_INDEX_SCORES[1]
  age_array[age_array >  AGE_CUTOFFS[1] & 
                       age_array < AGE_CUTOFFS[2]] = AGE_INDEX_SCORES[2]
  age_array[age_array >= AGE_CUTOFFS[2] & 
                       age_array < AGE_CUTOFFS[3]] = AGE_INDEX_SCORES[3]
  age_array[age_array >= AGE_CUTOFFS[3] & 
                       age_array < AGE_CUTOFFS[4]] = AGE_INDEX_SCORES[4]
  age_array[age_array >= AGE_CUTOFFS[4] & 
                       age_array < AGE_CUTOFFS[5]] = AGE_INDEX_SCORES[5]
  age_array[age_array >= AGE_CUTOFFS[5] & 
                      age_array <= AGE_CUTOFFS[6]] = AGE_INDEX_SCORES[6]
  
  
  return(age_array)
}
# getRaceIndex
#  @param POPULATION_SIZE int, the total number of people to be generated
#  @param RACE_DISTRIBUTION list, a list containing the probability distribution
#                                over the possible races, it currently must
#                                be of length 4
#  @param RACE_CATEGORIES list, a list containing the possible race definitions,
#                     currently must be Caucasian, Hispanic, Asian, or Black
#  @param RACE_INDEX_SCORES, indexing values that correspond to the RACE_CATEGORIES list,
#                     currently from the simplified FRAX model
#  @return raceIndexList, returns a list of size POPULATION_SIZE with the race index 
#                        scores corresponding to the generated population
#  Generates an arbitrary population according to provided race data
#  and returns a list with the FRAX age index score.
getRaceIndex <- function(POPULATION_SIZE,
                         RACE_DISTRIBUTION,
                         RACE_CATEGORIES,
                         RACE_INDEX_SCORES) {
  race_array <- sample(1:length(RACE_DISTRIBUTION), 
                       size=POPULATION_SIZE,
                       replace=TRUE,
                       prob=RACE_DISTRIBUTION)
  race_array[race_array == RACE_CATEGORIES[1]] = RACE_INDEX_SCORES[1]#20000
  race_array[race_array == RACE_CATEGORIES[2]] = RACE_INDEX_SCORES[2]#10000
  race_array[race_array == RACE_CATEGORIES[3]] = RACE_INDEX_SCORES[3]#30000
  race_array[race_array == RACE_CATEGORIES[4]] = RACE_INDEX_SCORES[4]#40000
  
  return(race_array)
}
# getBMDIndex
#  @param POPULATION_SIZE int, the total number of people to be generated
#  @param BMD_MEAN float, the mean value for bone mineral density in the population
#  @param BMD_STDDEV float, the standard deviation value for bone mineral density in the population
#  @param CENTERING_MEAN float, this is the value for centering the population about 0, it is from research data
#  @param BMD_CUTOFFS list, a list of cutoffs values used for indexing assignments
#  @param BMD_INDEX_SCORES list, a list of index values to be assigned
#  Generates an arbitrary population according to provided BMD Data
#  and returns a list with the FRAX age index score.
getBMDIndex <- function(POPULATION_SIZE,
                        BMD_MEAN,
                        BMD_STDDEV,
                        CENTERING_MEAN,
                        BMD_CUTOFFS,
                        BMD_INDEX_SCORES
                        ) {
  
  bmd_array <- (qnorm(runif(POPULATION_SIZE,0,1), BMD_MEAN, BMD_STDDEV)-CENTERING_MEAN)/BMD_STDDEV

  bmd_array[bmd_array >= BMD_CUTOFFS[1]] = BMD_INDEX_SCORES[1]
  bmd_array[bmd_array >= BMD_CUTOFFS[2] & bmd_array < BMD_CUTOFFS[3]] = BMD_INDEX_SCORES[2]
  bmd_array[bmd_array >= BMD_CUTOFFS[3] & bmd_array < BMD_CUTOFFS[4]] = BMD_INDEX_SCORES[3]
  bmd_array[bmd_array >= BMD_CUTOFFS[4] & bmd_array < BMD_CUTOFFS[5]] = BMD_INDEX_SCORES[4]
  bmd_array[bmd_array >= BMD_CUTOFFS[5] & bmd_array < BMD_CUTOFFS[6]] = BMD_INDEX_SCORES[5]
  bmd_array[bmd_array >= BMD_CUTOFFS[6] & bmd_array < BMD_CUTOFFS[7]] = BMD_INDEX_SCORES[6]
  bmd_array[bmd_array >= BMD_CUTOFFS[7] & bmd_array < BMD_CUTOFFS[8]] = BMD_INDEX_SCORES[7]
  bmd_array[bmd_array >= BMD_CUTOFFS[8] & bmd_array <  BMD_CUTOFFS[9]] = BMD_INDEX_SCORES[8]
  bmd_array[bmd_array >= BMD_CUTOFFS[9] & bmd_array <  BMD_CUTOFFS[10]] = BMD_INDEX_SCORES[9]
  bmd_array[bmd_array >= BMD_CUTOFFS[10] & bmd_array <  BMD_CUTOFFS[11]] = BMD_INDEX_SCORES[10]
  bmd_array[bmd_array <  BMD_CUTOFFS[2]] = BMD_INDEX_SCORES[11]
  
  return(bmd_array)
}
# getRiskFactorIndex
# @param POPULATION_SIZE int, the total number of people to be included
# @param FACTOR_PROBABILITES list, a list of floats corresponding to an arbitrary
#                                  number of additional risk factors
# This function generates an arbitrary population according to provided
# risk factor parameters.  Additional risk factors can be added to scoring by
# providing more risk factor probabilities.
getRiskFactorIndex <- function(POPULATION_SIZE,
                               FACTOR_PROBABILITIES) {
  for(i in 1:length(FACTOR_PROBABILITIES)) {
    if(i == 1) {
      risk_factor_index <- sample(0:1, 
                                  size=POPULATION_SIZE,
                                  replace=TRUE,
                                  prob=c(1-FACTOR_PROBABILITIES[i], FACTOR_PROBABILITIES[i]))
    } else {
      risk_factor_index = risk_factor_index + sample(0:1, 
                                                     size=POPULATION_SIZE,
                                                     replace=TRUE,
                                                     prob=c(1-FACTOR_PROBABILITIES[i], FACTOR_PROBABILITIES[i]))   
    }
  }
  return(risk_factor_index)
}


## MICAH ####
# @param POPULATION_SIZE int, the total number of people to be included
# @param FACTOR_PROBABILITES vector, a vector of floats corresponding to an arbitrary
#                                  number of additional risk factors
# @param FACTOR_NAMES vector of names to be used for the risk columns. Must be same length
#                     as FACTOR_PROBABILITIES
# generates a data.table with the booleans for each risk. columns are named so that you may
# pull out individual risk factor vectors.
getRiskFactors <- function(POPULATION_SIZE,
                           FACTOR_PROBABILITIES,
                           FACTOR_NAMES) { 
  ## function to randomly assign factors to patients
  ## driven by the logic behind getRiskFactorIndex(),
  ## but allows us to retain the risk information for each patient.
  
  dtbl <- setNames(data.table(matrix(nrow = POPULATION_SIZE, ncol = length(FACTOR_NAMES))), FACTOR_NAMES)
  
  for(i in 1:length(FACTOR_NAMES)) {
    dtbl[, FACTOR_NAMES[i] := sample(0:1, 
                                     size=POPULATION_SIZE,
                                     replace=TRUE,
                                     prob=c(1-FACTOR_PROBABILITIES[i], FACTOR_PROBABILITIES[i]))]
  }
  
  return(dtbl[])
  
  
}


countPatientRiskFactorIndex <- function(riskFactorTable){
  
  return(riskFactorTable[, rowSums(.SD)])
}

#####




# getMedicationUtilization
# @param BASE_MEDICATION_ADHERENCE float, the starting medication utilization in 2014
# @param YEAR int, the current year being simulated
# Returns the medication utilization rate based on the current year.  This 
# implementation scheme utilizes logarithmic decay as described in the Excel Model.
getMedicationUtilization <- function(BASE_MEDICATION_UTILIZATION,
                                   YEAR) {
  return(BASE_MEDICATION_UTILIZATION - 0.026 * log(YEAR - 2013))
}
# getDXAScans
# @param POPULATION_SIZE int, the population size being simulated
# @param FRAX_MAJOR list, a list of doubles corresponding to the fracture rates of the population
# @param DXA_PROB double, the identification rate for DXA scans
# Returns a boolean list of length POPULATION_SIZE corresponding to the number of people
# who receive DXA scans.
getDXAScans <- function(POPULATION_SIZE,
                        FRAX_MAJOR,
                        DXA_PROB) {
  if(DXA_PROB == 0) {
    return(replicate(POPULATION_SIZE,0))
  } else {
    return(FRAX_MAJOR >= quantile(FRAX_MAJOR, 1 - DXA_PROB))  
  }
}
# getMedPatients
# @param POPULATION_SIZE int, the population size being simulated
# @param FRAX_MAJOR list, a list of doubles corresponding to the fracture rates of the population
# @param MED_PROB double, the base treatment rate in 2014
# Returns a boolean list of length POPULATION_SIZE corresponding to the number of people
# who receive treatment.
getMedPatients <- function(POPULATION_SIZE,
                           FRAX_MAJOR,
                           MED_PROB,
                           YEAR) {
  if(MED_PROB == 0 || getMedicationUtilization(MED_PROB, YEAR) <= 0) {
    return(replicate(POPULATION_SIZE, 0))
  } else {
    return(FRAX_MAJOR >= quantile(FRAX_MAJOR, 1 - getMedicationUtilization(MED_PROB, YEAR) ))
  }  
}
# getFracture
# @param MED_PATIENTS list, a list of booleans corresponding to treatment and no-treatment
# @param FRACTURE_AVERAGE double, a double corresponding to the fracture average, a CONSTANT
# @param FRAX list, a list of doubles that corresponds to fracture risk percentage
# @param SAMPLE list, a list of pseudo-randomly generated doubles for determining risk
# This function returns a boolean list of the patients who experienced fractures
# over the course of a given year.  Note that 10 year averages are used which 
# accounts for the 10 found in the calculations.
getFracture <- function(MED_PATIENTS,
                        FRACTURE_AVERAGE,
                        FRAX,
                        SAMPLE) {
  return(ifelse(MED_PATIENTS,
                         SAMPLE < (FRACTURE_AVERAGE*(1-exp(-(-log(1-FRAX)/10)))),
                         SAMPLE < (1-exp(-(-log(1-FRAX)/10)))))  
}
# getMultiFraxCost
# @param TOTAL_FRAX double, the total number of fractures that occured in a given year
# @param FRAX_FACTOR double, a factor used to compensate for the people who experience multiple fractures
#                            in a given year, 1.226 in this case, value taken from literature
# @param WO_COST double, the cost corresponding to a given factor for people who experience 1 fracture in a year
# @param W_COST double, the cost corresponding to a given factor for people who experience >1 fractures in a year
# This function returns the cost for a given category provided the total number of fractures in the population
# and the costs associated with that factor.  The justification for the multi-fracture costing scheme can
# be found in the referenced literature and the associated paper.
getMultiFraxCost <- function(TOTAL_FRAX,
                             FRAX_FACTOR,
                             WO_COST,
                             W_COST) {
  return(TOTAL_FRAX * (1/FRAX_FACTOR)*WO_COST + 
           TOTAL_FRAX * ((FRAX_FACTOR-1)/FRAX_FACTOR)*W_COST)  
}