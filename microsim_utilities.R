############################################################################
# Osteoporosis Projection Model                                            #
# Date: Mar 21, 2019                                                       #
# Author: Max Feinberg                                                     # 
#         maxwell.a.feinberg@gmail.com                                     #
#                                                                          #
# This file contains helper functions for the primary microsimulation      #
# function.                                                                #
############################################################################


# getAgeIndex (description outdated 10.2.19, need to update)
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
getAgeIndex <- function(AGE_INDEX_SCORES, AGE_DISTRIBUTION, POPULATION_SIZE) {
  
  
  # directly sample the index scores that would be assigned to someone of that age group
  # weight by the age group distribution
  age_array <- sample(AGE_INDEX_SCORES,
                      size=POPULATION_SIZE,
                      replace=TRUE,
                      prob=AGE_DISTRIBUTION)
  
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
# getRaceIndex <- function(POPULATION_SIZE,
#                          RACE_DISTRIBUTION,
#                          RACE_CATEGORIES,
#                          RACE_INDEX_SCORES) {
#   race_array <- sample(1:length(RACE_DISTRIBUTION), 
#                        size=POPULATION_SIZE,
#                        replace=TRUE,
#                        prob=RACE_DISTRIBUTION)
#   race_array[race_array == RACE_CATEGORIES[1]] = RACE_INDEX_SCORES[1]#20000
#   race_array[race_array == RACE_CATEGORIES[2]] = RACE_INDEX_SCORES[2]#10000
#   race_array[race_array == RACE_CATEGORIES[3]] = RACE_INDEX_SCORES[3]#30000
#   race_array[race_array == RACE_CATEGORIES[4]] = RACE_INDEX_SCORES[4]#40000
#   
#   return(race_array)
# }


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
                        BMD_CUTOFFS,
                        BMD_INDEX_SCORES
                        ) {
  ## this is just drawing from a standard normal.
  ## the description even says it's for centering the population around 0. Why would they do this??
  # bmd_array <- (qnorm(runif(POPULATION_SIZE,0,1), BMD_MEAN, BMD_STDDEV)-CENTERING_MEAN)/BMD_STDDEV

  # null hypothesis
  # bmd_array <- rnorm(POPULATION_SIZE)
  
  # alternative (what they wanted it to do I think)
  bmd_array <- rnorm(POPULATION_SIZE, BMD_MEAN, BMD_STDDEV)
  
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
      new_factor = sample(0:1, 
                          size=POPULATION_SIZE,
                          replace=TRUE,
                          prob=c(1-FACTOR_PROBABILITIES[i], FACTOR_PROBABILITIES[i])) 
      risk_factor_index = risk_factor_index + new_factor
      if (i == 2) {
        prev_fracture_incidence = new_factor
      }
    }
  }
  return(list("risk_factor_index" = risk_factor_index, "prev_fracture_incidence" = prev_fracture_incidence))
}


# getMedicationUtilization
# @param BASE_MEDICATION_ADHERENCE float, the starting medication utilization in 2014
# @param YEAR int, the current year being simulated
# Returns the medication utilization rate based on the current year.  This 
# implementation scheme utilizes logarithmic decay as described in the Excel Model.
getMedicationUtilization <- function(BASE_MEDICATION_UTILIZATION,
                                   YEAR) {
  return(BASE_MEDICATION_UTILIZATION - 0.026 * log(YEAR - 2019))
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
# @param FRAX_MAJOR list, a list of doubles corresponding to the fracture rates of the population
# @param MED_PROB double, the base treatment rate in 2014
# Returns a boolean list of length FRAX_MAJOR corresponding to the number of people
# who receive treatment.
getMedPatients <- function(FRAX_MAJOR,
                           MED_PROB,
                           YEAR) {
  if(MED_PROB == 0 || getMedicationUtilization(MED_PROB, YEAR) <= 0) {
    return(replicate(length(FRAX_MAJOR), 0))
  } else {
    # this line is assigning all FRAX_MAJOR above (or equal to) the quantile corresponding to percent of patients
    # who are not utilizing their meds a 1. So all higher FRAX_MAJOR are being assigned as medical patients.
    # Shouldn't this be randomized amongst the patients?
    return(FRAX_MAJOR >= quantile(FRAX_MAJOR, 1 - getMedicationUtilization(MED_PROB, YEAR) ))
  }  
}


# getFracture
# @param MED_PATIENTS list, a list of booleans corresponding to treatment and no-treatment
# @param FRACTURE_AVERAGE double, a double corresponding to the fracture average, a CONSTANT
# @param FRAX list, a list of doubles that corresponds to fracture risk probability
# @param SAMPLE list, a list of pseudo-randomly generated doubles for determining risk
# This function returns a boolean list of the patients who experienced fractures
# over the course of a given year.  Note that FRAX uses P( > 0 fractures occuring in 10 years).
# Hence the ^.1 in the probability calculation.
getFracture <- function(MED_PATIENTS,
                        FRACTURE_AVERAGE,
                        FRAX,
                        SAMPLE) {
  
  prob_of_fracture_in_given_year <- 1-(1-FRAX)^.1
  
  return(ifelse(MED_PATIENTS,
                         SAMPLE < (FRACTURE_AVERAGE*prob_of_fracture_in_given_year),
                         SAMPLE < prob_of_fracture_in_given_year))  
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





# getCostWO
# get costs of fractures with people who did not have previous fractures. These are people that did not have a prior
# fracture and only get 1 this year.
getCostWO <- function(nFRACS_WO, FRAX_FACTOR, SINGLE_COST) {
  return(nFRACS_WO*(1/FRAX_FACTOR)*SINGLE_COST)
}




# getCostWP
# gets costs of fractures from people that have had a previous fracture. These are people that had a fracture in the past
# and either do or don't get a new frac this year, as well as people that did not have a frac in the past, but have had 
# multiple fractures this year. 
getCostWP <- function(nFRACS_WITH, nFRACS_WO, FRAX_FACTOR, SINGLE_COST, MULTI_COST) {
  costs_singles_with_history <- nFRACS_WITH*(1/FRAX_FACTOR)*SINGLE_COST
  costs_multi_with_history <- nFRACS_WITH*(1-1/FRAX_FACTOR)*MULTI_COST
  costs_multi_without_history <- nFRACS_WO*(1-1/FRAX_FACTOR)*MULTI_COST
  
  return(sum(costs_singles_with_history, costs_multi_with_history, costs_multi_without_history))
  
}

