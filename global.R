
library(aws.signature)
library(digest)
library(poibin)
library(doParallel)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggvis)
library(gganimate)
library(hashmap)
library(htmlwidgets)
library(magrittr)
library(plotly)
library(prettyR)
library(readxl)
library(rsconnect)
library(RColorBrewer)
library(scales)
library(shiny)
library(shinycssloaders)
library(shinyEffects)
library(shinythemes)
library(shinyLP)
library(shinyBS)
library(shinyjs)
library(shinyalert)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(snow)
library(doSNOW)
library(stringr)
library(tidyverse)


source("microsim_utilities.R")
source("microsim.R")

cl <- makeCluster(detectCores() - 1)
registerDoSNOW(cl)


tab_id <- c("Overview", "Mechanics", "Pop_Inputs", "ClinEcon_Inputs", "Scenarios", "Results", "Assumptions", "Break", "Disclosures", "Terms", "References")

NUM_PAGES <- length(tab_id) + 1

age_probabilities <- (read_excel("grouped_age_distribution_women_50up.xlsx"))

# lumps 90+
age_index_scores <- c(0, 2000, 4000, 6000, 8000, 10000, 12000, 14000, 16000) # assumes 50-54, 55-59, ...85-89, 90+



## zeroed out race index scores because we don't use them in sk_id_lookup.
## also prevents needing to remove all traces of race in the code. 
## can clean up later.
# race_categories = c(1, # Caucasian
#                     2, # Hispanic
#                     3, # Asian
#                     4  ) # Black
# race_index_scores = c(0,
#                       0,
#                       0,
#                       0)

fracture_breakdown = c(0.28, # Shoulder,
                       0.5, # vertebral
                       0.22 # forearm
)


centering_mean = 0.858

bmd_index_scores = c(100, 10, 20, 30, 40, 50, 60, 70, 80, 90, 0)
bmd_cutoffs = c(1.00, -3.51, -3.01, -2.51, -2.01, -1.51, -1.01, -0.51, 0.01,
                0.51)



ID_lookup <- (read_excel("sk_id_lookup.xlsx"))


MEDICATION_ADHERENCE <- 0.2221 #0.418 in US
NON_ADHERENT_INCREASED_FRACTURE_RISK <- 1.1
# HIP FRACTURE RATIO is a hard-coded value from the old excel model
# It extrapolates from the amount of hip fractures the total amount of other fractures
# (including shoulder, vertebral, and forearm which is why we need to subtract them from
# the total_fractures in microsim.R)
HIP_FRACTURE_RATIO <- (45603/5024) # ~9.077
MULTI_FRACTURE_FACTOR <- 1.226


