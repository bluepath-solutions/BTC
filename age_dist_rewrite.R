
## want to assume that age distribution is given by 5-yr age ranges instead of by age-year.
## rewriting this fcn for that.
## then need to adapt age distribution spreadsheet.
# 
# getAgeIndex <- function(MINIMUM_AGE, 
#                         MAXIMUM_AGE, 
#                         POPULATION_SIZE,
#                         AGE_DISTRIBUTION,
#                         AGE_CUTOFFS,
#                         AGE_INDEX_SCORES) {
#   
#   # should sample age group (1,2,3...) (as factor) instead of actual ages
#   age_array <- sample(MINIMUM_AGE:MAXIMUM_AGE, 
#                       size=POPULATION_SIZE,
#                       replace=TRUE,
#                       prob=AGE_DISTRIBUTION)
#   
#   
#   age_array[age_array ==  AGE_CUTOFFS[1]] = AGE_INDEX_SCORES[1]
#   age_array[age_array >  AGE_CUTOFFS[1] & 
#               age_array < AGE_CUTOFFS[2]] = AGE_INDEX_SCORES[2]
#   age_array[age_array >= AGE_CUTOFFS[2] & 
#               age_array < AGE_CUTOFFS[3]] = AGE_INDEX_SCORES[3]
#   age_array[age_array >= AGE_CUTOFFS[3] & 
#               age_array < AGE_CUTOFFS[4]] = AGE_INDEX_SCORES[4]
#   age_array[age_array >= AGE_CUTOFFS[4] & 
#               age_array < AGE_CUTOFFS[5]] = AGE_INDEX_SCORES[5]
#   age_array[age_array >= AGE_CUTOFFS[5] & 
#               age_array < AGE_CUTOFFS[6]] = AGE_INDEX_SCORES[6]
#   age_array[age_array >= AGE_CUTOFFS[6] &
#               age_array < AGE_CUTOFFS[7]] = AGE_INDEX_SCORES[7]
#   age_array[age_array >= AGE_CUTOFFS[7] &
#               age_array < AGE_CUTOFFS[8]] = AGE_INDEX_SCORES[8]
#   age_array[age_array >= AGE_CUTOFFS[8] &
#               age_array <= AGE_CUTOFFS[9]] = AGE_INDEX_SCORES[9]
#   
#   
#   return(age_array)
# }


## things to add into the current files

# global
age_probabilities <- (read_excel("grouped_age_distribution_women_50up.xlsx"))

# age_cutoffs = c(50, 55.5, 60.5, 65.5, 70.5, 75.5, 80.5, 85.5, 100)
age_index_scores <- c(0, 2000, 4000, 6000, 8000, 10000, 12000, 14000, 16000) # assumes 50-54, 55-59, ...85-89, 90+
# lumps 90+

# microsim
age_prob <- c(age_probabilities[index,2:10])


# microsim utilities
getAgeIndex <- function(AGE_INDEX_SCORES, AGE_DISTRIBUTION, POPULATION_SIZE) {
  
  
  # directly sample the index scores that would be assigned to someone of that age group
  # weight by the age group distribution
    age_array <- sample(AGE_INDEX_SCORES,
                        size=POPULATION_SIZE,
                        replace=TRUE,
                        prob=AGE_DISTRIBUTION)
    
    return(age_array)
}




















