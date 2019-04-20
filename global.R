library(aws.signature)
library(digest)
library(doParallel)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggvis)
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

# cl <- makeCluster(detectCores() - 1)
# registerDoParallel(cl)
cl <- makeCluster(detectCores() - 1)
registerDoSNOW(cl)


tab_id <- c("Overview", "Inputs", "Fracture", "Scenarios", "Results", "Assumptions", "Break", "Disclosures", "Terms", "References")

NUM_PAGES <- 11

age_probabilities <- (read_excel("age_distribution.xlsx"))

minimum_age <- 65
maximum_age <- 100
age_cutoffs = c(65, 70.5, 75.5, 80.5, 85.5, 100)
age_index_scores <- c(0, 200, 400, 600, 800, 1000)

race_categories = c(1, #Caucasian
                    2, # Hispanic
                    3, # Asian
                    4  ) # Black
race_index_scores = c(20000,
                      10000,
                      30000,
                      40000)

fracture_breakdown = c(0.28, # Shoulder,
                       0.5, #vertebral
                       0.22 # forearm
)


centering_mean = 0.858

bmd_index_scores = c(100, 10, 20, 30, 40, 50, 60, 70, 80, 90, 0)
bmd_cutoffs = c(1.00, -3.51, -3.01, -2.51, -2.01, -1.51, -1.01, -0.51, 0.01,
                0.51)



ID_lookup <- (read_excel("id_lookup.xlsx"))

#id_to_frax_hash <- hashmap(ID_lookup$ID, ID_lookup$`FRAX- HIP`)
#MAX_HIP_FRACTURE_RATE <- max(ID_lookup$`FRAX- HIP`)
#id_to_frax_major_hash <- hashmap(ID_lookup$ID,ID_lookup$`FRAX- MAJOR`)
#MAX_MAJOR_FRACTURE_RATE <- max(ID_lookup$`FRAX- MAJOR`)


MEDICATION_ADHERENCE <- (6+6*0.41)
HIP_FRACTURE_RATIO <- (45603/5024)
MULTI_FRACTURE_FACTOR <- 1.226

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

`%then%` <- shiny:::`%OR%`
