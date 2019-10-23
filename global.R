
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

age_probabilities <- list(China = read_excel('./Data/ageDbn/productionAgeDbnChina.xlsx'),
                          `Hong Kong` = read_excel('./Data/ageDbn/productionAgeDbnHongKong.xlsx'),
                          Taiwan = read_excel('./Data/ageDbn/productionAgeDbnTaiwan.xlsx'),
                          Japan = read_excel('./Data/ageDbn/productionAgeDbnJapan.xlsx'),
                          Thailand = read_excel('./Data/ageDbn/productionAgeDbnThailand.xlsx'),
                          `South Korea` = read_excel('./Data/ageDbn/productionAgeDbnSK.xlsx'))

# lumps 90+
age_index_scores <- c(0, 2000, 4000, 6000, 8000, 10000, 12000, 14000, 16000) # assumes 50-54, 55-59, ...85-89, 90+



fracture_breakdown = c(0.28, # Shoulder,
                       0.5, # vertebral
                       0.22 # forearm
)


bmd_index_scores = c(100, 10, 20, 30, 40, 50, 60, 70, 80, 90, 0)
bmd_cutoffs = c(1.00, -3.51, -3.01, -2.51, -2.01, -1.51, -1.01, -0.51, 0.01,
                0.51)


ID_lookup <- list(China = read_excel('./Data/frax/china_frax.xlsx'),
                  `Hong Kong` = read_excel('./Data/frax/hongkong_frax.xlsx'),
                  Taiwan = read_excel('./Data/frax/taiwan_frax.xlsx'),
                  Japan = read_excel('./Data/frax/japan_frax.xlsx'),
                  Thailand = read_excel('./Data/frax/thailand_frax.xlsx'),
                  `South Korea` = read_excel('./Data/frax/sk_frax.xlsx'))


# https://fred.stlouisfed.org/series/KORCPIALLMINMEI
# Korean CPI Jan 2012-Jan 2019, Jan 2006-Jan 2019
### KOREA VALUES ONLY!!!! ###
price_inflation_2012_2019 <- 104.24/96.184 
price_inflation_2012_2018 <- 103.42/93.07
price_inflation_2006_2019 <- 104.24/79.306
price_inflation_2018_2019 <- 103.42/104.24


# alendronate sodium 10mg, ibandronate sodium 150mg, risedronate sodium oral tablet 5mg,
# ibandronate sodium IV 3mg/3ml, zoledronic acid iv 5mg/100ml, denosumab subQ 60mg/ml,
# conjugated estrogens/bazedoxifene 0.45-20mg, raloxifene Hcl 60mg,
# forteo subQ 600mcg/2.4ml, Tymlos
treat_mix <- list(China = c(76, 0, 9.5, 0, 14.5, 0, 0, 0, 0, 0)/100,
                  `Hong Kong` = c(46.4, 7.7, 25.6, 0, 3.8, .2, 0, 9.8, 6.6, 0)/100,
                  Taiwan = c(21.68, 0, .42, 5.95, 14.57, 43.12, .09, 11.32, 2.86, 0)/100,
                  Japan = c(25.78, 0, 5.05, 0.26, 0, 1.68, 2.46, 18.26, 20.6, 0)/100,
                  Thailand = c(51.38, 12.18, 25.36, 0, 5.2, 0, 0, 4.1, 0.7, 0)/100,
                  `South Korea` = c(16.7, 9.8, 20.9, 24.9, 6, 4.5, 3.3, 9.8, 0.9, 0)/100)
treat_cost <- list(China = c(421, 0, 218, 316, 449, 0, 0, 503, 801, 0)/12,
                   `Hong Kong`= c(460, 640, 460, 400, 600, 270, 0, 675, 7500, 0)/12,
                   Taiwan = c(6.13, 0, 5.94, 78.28, 329.45, 199.41, .96, 1.22, 459.61, 0),
                   Japan = c(231, 0, 0, 0, 0, 470, 0, 0, 0, 0),
                   Thailand = c(150.81, 432.63, 374.88, 369.6, 457.05, 841.5, 0, 578.16, 6408.6, 0),
                   `South Korea` = c(0.67, 17.03, 0.64, 47.94, 273.56, 192.08, 0.94, 0.63, 290.78, 1822.41)*price_inflation_2012_2019) # c(536, 485, 0, 0, 0, 0, 0, 634, 0, 0))
treat_efficacy_hip <- list(China = c(65, 73, 74, 59, 59, 61, 59, 59, 25, 25)/100,
                           `Hong Kong` = c(65, 73, 74, 59, 59, 61, 59, 59, 25, 25)/100,
                           Taiwan = c(65, 73, 74, 59, 59, 61, 59, 59, 25, 25)/100,
                           Japan = c(65, 73, 74, 59, 59, 61, 59, 59, 25, 25)/100,
                           Thailand = c(65, 73, 74, 59, 59, 61, 59, 59, 25, 25)/100,
                           `South Korea` = c(65, 73, 74, 59, 59, 61, 59, 59, 25, 25)/100)
treat_efficacy_other <- list(China = c(65, 73, 74, 59, 59, 61, 59, 59, 25, 25)/100,
                             `Hong Kong` = c(65, 73, 74, 59, 59, 61, 59, 59, 25, 25)/100,
                             Taiwan = c(65, 73, 74, 59, 59, 61, 59, 59, 25, 25)/100,
                             Japan = c(65, 73, 74, 59, 59, 61, 59, 59, 25, 25)/100,
                             Thailand = c(65, 73, 74, 59, 59, 61, 59, 59, 25, 25)/100,
                             `South Korea` = c(65, 73, 74, 59, 59, 61, 59, 59, 25, 25)/100)

# HIP FRACTURE RATIO is a hard-coded value from the old excel model
# It extrapolates from the amount of hip fractures the total amount of other fractures
# (including shoulder, vertebral, and forearm which is why we need to subtract them from
# the total_fractures in microsim.R)
HIP_FRACTURE_RATIO <- (45603/5024) # ~9.077
MULTI_FRACTURE_FACTOR <- 1.226



## global tables for all-JAPAC

country_names <- c('China', 'Hong Kong', 'Taiwan', 'Japan', 'Thailand', 'South Korea')

country_popn_inputs <- data.frame(country = country_names,
                                  popn = c(701077, 4057, 11982, 64724, 35834, 10232.9)*1000,
                                  bmdMean = c(.757, .757, .757, .624, .787, .86),
                                  bmdSD = c(.108, .108, .108, .109, .205, .097),
                                  RAinp = c(5.49, 5.49, 7.4, 17.8, 9, 5.7),
                                  fracInp = c(28.1, 28.1, 20.8, 37.3, 24, 14.3),
                                  parfracInp = c(10.44, 10.44, 10.66, 21.6, 32, 16.6),
                                  smoker = c(1.7, 1.7, 1.5, 9.7, 3.2, 6.3),
                                  alco = c(4.67, 4.67, 0.7, 7.6, 3, 5.5),
                                  gluco = c(1.7, 1.7, 7.3, 4.6, 11.7, 9.9),
                                  stringsAsFactors = FALSE)
country_singlefrac_inputs <- data.frame(country = country_names,
                                        inpatient = c(1557, 4411.16, 2312.93, 22080, 2559, 4698.97),
                                        outpatient = c(342, 1303.91, 508.05, 0, 164, 1243.64),
                                        ltc = c(4,15.25,5.94, 1510, 0, 0),
                                        ed = c(0,0,0,0,0, 0),
                                        other = c(277, 1056.09, 411.48, 0, 0, 0),
                                        pharmacy = c(648, 2470.56, 962.61, 0, 0, 1123.51),
                                        productivity = c(132, 132, 132, 0, 269.99, 240),
                                        cgBurden = c(485, 485, 485, 830, 992.01, 426))
country_multifrac_inputs <- data.frame(country = country_names,
                                       inpatient = round(1.72057 * country_singlefrac_inputs$inpatient, 2),
                                       outpatient = round(1.53135 * country_singlefrac_inputs$outpatient, 2),
                                       ltc = round(2.14550 * country_singlefrac_inputs$ltc, 2),
                                       ed = round(1.39765 * country_singlefrac_inputs$ed, 2), 
                                       other = round(1.71715 * country_singlefrac_inputs$other, 2),
                                       pharmacy = round(1.14438 * country_singlefrac_inputs$pharmacy, 2), 
                                       productivity = country_singlefrac_inputs$productivity,
                                       cgBurden = country_singlefrac_inputs$cgBurden)
country_scenario_inputs <- data.frame(country = country_names,
                                      baseID = c(3.7, 7.19, 1.66, 5, 38.2, 23.16),
                                      baseTreat = c(20.6, 22.9, 29.31, 26.01, 22.3, 14.4),
                                      improvedID = c(18.7, 22.19, 16.66, 20.0, 53.2, 38.16),
                                      improvedTreat = c(27.2, 29.5, 35.91, 32.62, 28.9, 21.0))
country_other_inputs <- data.frame(country = country_names,
                                   adherence = c(28.2, 28.2, 28.2, 25, 25, 22.1)/100,
                                   nonadherentRisk = c(1.1, 1.71, 1.71, 1.1, 1.5, 1.1),
                                   dxaScreen = c(27, 80, 21.29, 45, 70, 33.36))



country_popn_value <- function(country, value) {
  return(country_popn_inputs[country_popn_inputs$country == country, value])
}

country_cost_value <- function(country, value, multi) {
  if(multi) {
    return(country_multifrac_inputs[country_multifrac_inputs$country == country, value])
  } else {
    return(country_singlefrac_inputs[country_singlefrac_inputs$country == country, value])
  }
}

country_scenario_value <- function(country, value) {
  return(country_scenario_inputs[country_scenario_inputs$country == country, value])
}

country_other_value <- function(country, value) {
  return(country_other_inputs[country_other_inputs$country == country, value])
}



















