###########################VARIABLES##############################
abbreviations <- data.frame(abbrev = c("BMD", "CPI", "DXA", "FRAX", "NHANES"),
                            full = c("bone mineral density", "Consumer Price Index", "dual-energy X-ray absorptiometry", "Fracture Risk Assessment Tool", "National Health and Nutrition Examination Survey"))

tab_id <- c("Home", "Overview", "Mechanics", "Pop_Inputs", "ClinEcon_Inputs", "Scenarios", "Results", "Assumptions", "Disclosures", "Terms", "References")

######################### FUNKY FUNCTIONS ######################

update_popn_inputs <- function(session, country) {
  updateNumericInput(session, 'pop_input', value = country_popn_value(country, 'popn'))
  updateNumericInput(session, 'BMD_mean', value = country_popn_value(country, 'bmdMean'))
  updateNumericInput(session, 'BMD_SD', value = country_popn_value(country, 'bmdSD'))
  updateNumericInput(session, 'RA_inp', value = country_popn_value(country, 'RAinp'))
  updateNumericInput(session, 'fxr_inp', value = country_popn_value(country, 'fracInp'))
  updateNumericInput(session, 'parfxr_inp', value = country_popn_value(country, 'parfracInp'))
  updateNumericInput(session, 'smoker', value = country_popn_value(country, 'smoker'))
  updateNumericInput(session, 'alco', value = country_popn_value(country, 'alco'))
  updateNumericInput(session, 'gluco_tx', value = country_popn_value(country, 'gluco'))
}


update_cost_inputs <- function(session, country) {
  ## single fracs
  updateNumericInput(session, 'costinpt1', value = country_cost_value(country, 'inpatient', FALSE))
  updateNumericInput(session, 'costoutpt1', value = country_cost_value(country, 'outpatient', FALSE))
  updateNumericInput(session, 'costLTC1', value = country_cost_value(country, 'ltc', FALSE))
  updateNumericInput(session, 'costED1', value = country_cost_value(country, 'ed', FALSE))
  updateNumericInput(session, 'costother1', value = country_cost_value(country, 'other', FALSE))
  updateNumericInput(session, 'costpharm1', value = country_cost_value(country, 'pharmacy', FALSE))
  updateNumericInput(session, 'costprod1', value = country_cost_value(country, 'productivity', FALSE))
  updateNumericInput(session, 'costcare1', value = country_cost_value(country, 'cgBurden', FALSE))
  
  ## multi fracs
  updateNumericInput(session, 'costinpt2', value = country_cost_value(country, 'inpatient', TRUE))
  updateNumericInput(session, 'costoutpt2', value = country_cost_value(country, 'outpatient', TRUE))
  updateNumericInput(session, 'costLTC2', value = country_cost_value(country, 'ltc', TRUE))
  updateNumericInput(session, 'costED2', value = country_cost_value(country, 'ed', TRUE))
  updateNumericInput(session, 'costother2', value = country_cost_value(country, 'other', TRUE))
  updateNumericInput(session, 'costpharm2', value = country_cost_value(country, 'pharmacy', TRUE))
  updateNumericInput(session, 'costprod2', value = country_cost_value(country, 'productivity', TRUE))
  updateNumericInput(session, 'costcare2', value = country_cost_value(country, 'cgBurden', TRUE))
}


update_scenario_inputs <- function(session, country) {
  updateNumericInput(session, 'basecaseID', value = country_scenario_value(country, 'baseID'))
  updateNumericInput(session, 'basecaseTx', value = country_scenario_value(country, 'baseTreat'))
  updateNumericInput(session, 'scenario1ID', value = country_scenario_value(country, 'improvedID'))
  updateNumericInput(session, 'scenario1Tx', value = country_scenario_value(country, 'improvedTreat'))
}


## other ####

popn_projections <- list(China = c(687997, 691559, 694970, 698159, 701076, # 2016-2020
                                  703694, 706017, 708062, 709863, 711448, # 2021-2025
                                  712815, 713960, 714893, 715630, 716181, # 2026-2030
                                  716555, 716751, 716765, 716587, 716211, # 2031-2035
                                  715637, 714871, 713918, 712784, 711477), # 2036-2040
                        `Hong Kong` = c(3892, 3933, 3976, 4018, 4057,
                                     4094, 4128, 4160, 4192, 4223,
                                     4254, 4285, 4314, 4341, 4363,
                                     4382, 4396, 4407, 4416, 4424,
                                     4431, 4436, 4440, 4443, 4444),
                        Taiwan = c(11835, 11876, 11914, 11950, 11982,
                                   12012, 12039, 12064, 12086, 12106,
                                   12124, 12139, 12152, 12162, 12169,
                                   12173, 12173, 12169, 12160, 12147,
                                   12130, 12107, 12079, 12047, 12009),
                        Japan = c(65345, 65221, 65076, 64910, 64723,
                                  64516, 64289, 64044, 63781, 63503,
                                  63211, 62904, 62586, 62258, 61921,
                                  61578, 61228, 60872, 60509, 60138,
                                  59761, 59378, 58991, 58599, 58205),
                        Thailand = c(35311, 35458, 35595, 35721, 35834,
                                     35934, 36021, 36096, 36159, 36211,
                                     36251, 36281, 36300, 36308, 36306,
                                     36293, 36271, 36238, 36194, 36141,
                                     36077, 36003, 35918, 35823, 35716))
## made a reactive variable in server function to update with current country popn_projection
## country_popn_projection

################ START. THE. COOOOOODE!!!! #################################################
function(input, output, session) {
  output$value <- renderText({ input$inpt })
  
  # stop app when browser is closed
  session$onSessionEnded(stopApp)
  
###############DEFAULT ACTIONS###################################
  observe({
    lapply(c("Next", "Previous"),
           toggle,
           condition = input[["tabs"]] != "Home")
  })
  
  Current <- reactiveValues(
    Tab = "Home"
  )
  
  observeEvent(
    input[["tabs"]],
    {
      Current$Tab <- input[["tabs"]]
    }
  )
  
  observeEvent(
    input[['Id_enter']],
    {
      updateTabItems(session, 'tabs', 'Overview')
    }
  )
  
  observeEvent(
    input[["Previous"]],
    {
      tab_id_position <- match(Current$Tab, tab_id) - 1
      if (tab_id_position == 0) tab_id_position <- length(tab_id)
      Current$Tab <- tab_id[tab_id_position]
      updateTabItems(session, "tabs", tab_id[tab_id_position]) 
    }
  )
  
  observeEvent(
    input[["Next"]],
    {
      tab_id_position <- match(Current$Tab, tab_id) + 1
      if (tab_id_position > length(tab_id)) tab_id_position <- 1
      Current$Tab <- tab_id[tab_id_position]
      updateTabItems(session, "tabs", tab_id[tab_id_position]) 
    }
  )
  
  
  observeEvent(input$restoreall, {
	sendSweetAlert(
      session = session,
      title = "Restored",
      text = "All Default Inputs Have Been Restored",
      type = "info"
    )
    reset("pop_input")
    reset("BMD_mean")
    reset("BMD_SD")
    reset("RA_inp")
    reset("fxr_inp")
    reset("parfxr_inp")
    reset("smoker")
    reset("alco")
    reset("gluco_tx")
   
    reset("costinpt1")
    reset("costinpt2")
    reset("costoutpt1")
    reset("costoutpt2")
    reset("costLTC1")
    reset("costLTC2")
    reset("costED1")
    reset("costED2")
    reset("costother1")
    reset("costother2")
    reset("costpharm1")
    reset("costpharm2")
    reset("costprod1")
    reset("costprod2")
    reset("costcare1")
    reset("costcare2")
	  reset("basecaseID")
    reset("basecaseTx")
    reset("scenario1ID")
    reset("scenario1Tx")
  })  
  
  observeEvent(input$restorepop, {
	sendSweetAlert(
      session = session,
      title = "Restored",
      text = "Population & Demographic Default Inputs Have Been Restored",
      type = "info"
    )
    reset("pop_input")
    reset("BMD_mean")
    reset("BMD_SD")
    reset("RA_inp")
    reset("fxr_inp")
    reset("parfxr_inp")
    reset("smoker")
    reset("alco")
    reset("gluco_tx")
  })  
  
  observeEvent(input$restorefxrcosts, {
	sendSweetAlert(
      session = session,
      title = "Restored",
      text = "All Cost Default Inputs Have Been Restored",
      type = "info"
    )
    reset("costinpt1")
    reset("costinpt2")
    reset("costoutpt1")
    reset("costoutpt2")
    reset("costLTC1")
    reset("costLTC2")
    reset("costED1")
    reset("costED2")
    reset("costother1")
    reset("costother2")
    reset("costpharm1")
    reset("costpharm2")
    reset("costprod1")
    reset("costprod2")
    reset("costcare1")
    reset("costcare2")
  })
  
    observeEvent(input$restorescenarios, {
    sendSweetAlert(
      session = session,
      title = "Restored",
      text = "All Default Scenario Inputs Have Been Restored",
      type = "info"
    )
    reset("basecaseID")
    reset("basecaseTx")
    reset("scenario1ID")
    reset("scenario1Tx")
  })
    
    
    observeEvent(input$basecaseID, {
      isolate(updateNumericInput(session,
                                 inputId = 'scenario1ID',
                                 label = NULL,
                                 value = input$basecaseID + 15))
    },
    ignoreInit = T, 
    priority = 1000)
    
    
  ## Beginning logic section to add all countries in one app ####
    
  
    
  ## well, that's what this was, but then I moved all of the machinery outside of
  ## this section and into global or server pre-function.

  ## section to initialize and update numeric inputs
  updateSelectizeInput(session, 'countrySelect', selected = 'China',
                       choices = country_names)
  
  
  ## section to update inputs when country has been changed
  observeEvent(input$countrySelect, {
    update_popn_inputs(session, input$countrySelect)
    update_cost_inputs(session, input$countrySelect) 
    update_scenario_inputs(session, input$countrySelect)
  })
    
  country_popn_projections <- reactive({
    projections <- unlist(unname(popn_projections[input$countrySelect]))
    return(projections)
    })

  ## other sections ####
  
  observeEvent(input$scenario1ID, {
    isolate(updateNumericInput(session,
                       inputId = 'scenario1Tx',
                       label = NULL,
                       value = 0.44*(input$scenario1ID - input$basecaseID) + input$basecaseTx))
  },
  ignoreInit = T, 
  priority = 1000)
  
  observeEvent(input$gluco_tx, {
    simulation_data$sim <- sim_data()
  }, ignoreInit = F,
  priority = 1500,
  once = T,  
  autoDestroy = T
  )
  
  observeEvent(input$run_simulation, {
    simulation_data$sim <- sim_data()
  }, 
  ignoreInit = F,
  priority = 1,
  autoDestroy = F, once = F)
###############INPUT VALIDATION#####################
  # valid <- reactiveValues(pop = T)
  pop_input <- reactive({
    validate(
      need(input$pop_input > 0, 
           'Population must be greater than 0.')
    )
    return(input$pop_input)
  })
  observeEvent(input$pop_input, {
    if(!is.numeric(input$pop_input) || input$pop_input <= 0) {
      shinyalert("Population Error", "Population must be greater than 0.", type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)
  
  
  asian_rate <- function(){return(1)}
  
  
  BMD_mean <- reactive({
    validate(
      need(input$BMD_mean >= -4 && input$BMD_mean <= 1.0, 
           "Mean bone mineral density must be within the range [-4, 1.0]"))
    return(input$BMD_mean)
  })
  observeEvent(input$BMD_mean, {
    if(!is.numeric(input$BMD_mean) || input$BMD_mean < -4 || input$BMD_mean > 1.0) {
      shinyalert("BMD Parameter Error", 
                 "Mean bone mineral density must be within the range [-4, 1.0].", 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)
  
  BMD_SD <- reactive({
    validate(
      need(input$BMD_SD > 0 && input$BMD_SD <= 10.0, 
           "Bone mineral density standard deviation must be within the range (0, 10.0]"))
    return(input$BMD_SD)
  })  
  observeEvent(input$BMD_SD, {
    if(!is.numeric(input$BMD_SD) || input$BMD_SD <= 0 || input$BMD_SD > 10.0) {
      shinyalert("BMD Parameter Error", 
                 "Bone mineral density standard deviation must be within the range (0, 10.0].", 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)
  
  RA_rate <- reactive({
    validate(
      need(input$RA_inp > 0 && input$RA_inp <= 100.0, 
           "Rheumatoid arthritis percentage must be within the range (0, 100.0]"))
    return(input$RA_inp/100.0)
  })
  observeEvent(input$RA_inp, {
    if(!is.numeric(input$RA_inp) || input$RA_inp <= 0 || input$RA_inp > 100.0) {
      shinyalert("Risk Factor Error", 
                 "Rheumatoid arthritis percentage must be within the range (0, 100.0].", 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)
  
  FXR_rate <- reactive({
    validate(
      need(input$fxr_inp > 0 && input$fxr_inp <= 100.0, 
           "Previous fracture percentage must be within the range (0, 100.0]"))
    return(input$fxr_inp/100.0)
  })
  observeEvent(input$fxr_inp, {
    if(!is.numeric(input$fxr_inp) || input$fxr_inp <= 0 || input$fxr_inp > 100.0) {
      shinyalert("Risk Factor Error", 
                 "Previous fracture percentage must be within the range (0, 100.0].", 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)
  
  PARFXR_rate <- reactive({
    validate(
      need(input$parfxr_inp > 0 && input$parfxr_inp <= 100.0, 
           "Parent history of hip fracture percentage must be within the range (0, 100.0]"))
    return(input$parfxr_inp/100.0)
  })
  observeEvent(input$parfxr_inp, {
    if(!is.numeric(input$parfxr_inp) || input$parfxr_inp <= 0 || input$parfxr_inp > 100.0) {
      shinyalert("Risk Factor Error", 
                 "Parent history of hip fracture percentage must be within the range (0, 100.0].", 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)
  
  SMOKER_rate <- reactive({
    validate(
      need(input$smoker > 0 && input$smoker <= 100.0, 
           "Smoker percentage must be within the range (0, 100.0]"))
    return(input$smoker/100.0)
  })
  observeEvent(input$smoker, {
    if(!is.numeric(input$smoker) || input$smoker <= 0 || input$smoker > 100.0) {
      shinyalert("Risk Factor Error", 
                 "Smoker percentage must be within the range (0, 100.0].", 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)
  
  ALCO_rate <- reactive({
    validate(
      need(input$alco > 0 && input$alco <= 100.0, 
           "Excessive alcohol use percentage must be within the range (0, 100.0]"))
    return(input$alco/100.0)
  })
  observeEvent(input$alco, {
    if(!is.numeric(input$alco) || input$alco <= 0 || input$alco > 100.0) {
      shinyalert("Risk Factor Error", 
                 "Excessive alcohol use percentage must be within the range (0, 100.0].", 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)
  
  GLUCO_rate <- reactive({
    validate(
      need(input$gluco_tx > 0 && input$gluco_tx <= 100.0, 
           "Long-term glucocorticoid therapy percentage must be within the range (0, 100.0]"))
    return(input$gluco_tx/100.0)
  })
  observeEvent(input$gluco_tx, {
    if(!is.numeric(input$gluco_tx) || input$gluco_tx <= 0 || input$gluco_tx > 100.0) {
      shinyalert("Risk Factor Error", 
                 "Long-term glucocorticoid therapy percentage must be within the range (0, 100.0].", 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)
  
  Base_Case_ID <- reactive({
    validate(
      need(input$basecaseID >= 0 && input$basecaseID <= 100.0, 
           "Base case identification rate must be within the range [0, 100.0]"))
    return(input$basecaseID/100.0)
  })
  observeEvent(input$basecaseID, {
    if(!is.numeric(input$basecaseID) ||input$basecaseID < 0 || input$basecaseID > 100.0) {
      shinyalert("Identification Rate Error", 
                 "Base case identification rate must be within the range [0, 100.0].", 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)
  
  Base_Case_Treatment <- reactive({
    validate(
      need(input$basecaseTx >= 0 && input$basecaseTx <= 100.0, 
           "Base case treatment percentage must be within the range [0, 100.0]"))
    return(input$basecaseTx/100.0)
  })
  observeEvent(input$basecaseTx, {
    if(!is.numeric(input$basecaseTx) || input$basecaseTx < 0 || input$basecaseTx > 100.0) {
      shinyalert("Treatment Rate Error", 
                 "Base case treatment percentage must be within the range [0, 100.0].", 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)  
  
  S1_ID <- reactive({
    validate(
      need(input$scenario1ID >= input$basecaseID && input$scenario1ID <= 100.0, 
           paste0("New scenario identification rate must be within the range [", input$basecaseID,", 100.0].")))
    return(input$scenario1ID/100.0)
  })
  observeEvent(input$scenario1ID, {
    if(!is.numeric(input$scenario1ID) || input$scenario1ID < input$basecaseID || input$scenario1ID > 100.0) {
      shinyalert("Identification Rate Error", 
                 paste0("New scenario identification rate must be within the range [", input$basecaseID,
                        ", 100.0] and treatment percentage within [", input$basecaseTx,", 100.0]."), 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)
  
  S1_Treatment <- reactive({
    validate(
      need(input$scenario1Tx >= input$basecaseTx && input$scenario1Tx <= 100.0, 
           paste0("New scenario treatment percentage must be within the range [", input$basecaseTx,", 100.0].")))
    return(input$scenario1Tx/100.0)
  })
  observeEvent(input$scenario1Tx, {
    if(!is.numeric(input$scenario1Tx) || input$scenario1Tx < input$basecaseTx || input$scenario1Tx > 100.0) {
      shinyalert("Treatment 
                 Rate Error", 
                 paste0("New scenario identification rate must be within the range [", input$basecaseID,
                        ", 100.0] and treatment percentage within [", input$basecaseTx,", 100.0]."), 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)
  
  
  costinpt1 <- reactive({
    validate(
      need(input$costinpt1 >= 0, 
           "Inpatient costs must be greater than or equal to 0."))
    return(input$costinpt1)
  })
  observeEvent(input$costinpt1 , {
    if(!is.numeric(input$costinpt1) || input$costinpt1 < 0) {
      shinyalert("Cost Error", 
                 "Inpatient costs must be greater than or equal to 0.", 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)
  
  costinpt2 <- reactive({
    validate(
      need(input$costinpt2 >= 0, 
           "Inpatient costs must be greater than or equal to 0."))
    return(input$costinpt2)
  })
  observeEvent(input$costinpt2 , {
    if(!is.numeric(input$costinpt2) || input$costinpt2 < 0) {
      shinyalert("Cost Error", 
                 "Inpatient costs must be greater than or equal to 0.", 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)
  
  costoutpt1 <- reactive({
    validate(
      need(input$costoutpt1 >= 0, 
           "outpatient costs must be greater than or equal to 0."))
    return(input$costoutpt1)
  })
  observeEvent(input$costoutpt1 , {
    if(!is.numeric(input$costoutpt1) || input$costoutpt1 < 0) {
      shinyalert("Cost Error", 
                 "Outpatient costs must be greater than or equal to 0.", 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)
  
  costoutpt2 <- reactive({
    validate(
      need(input$costoutpt2 >= 0, 
           "Outpatient costs must be greater than or equal to 0."))
    return(input$costoutpt2)
  })
  observeEvent(input$costoutpt2 , {
    if(!is.numeric(input$costoutpt2) || input$costoutpt2 < 0) {
      shinyalert("Cost Error", 
                 "Outpatient costs must be greater than or equal to 0.", 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)  
  
  costLTC1 <- reactive({
    validate(
      need(input$costLTC1 >= 0, 
           "Long-term care costs must be greater than or equal to 0."))
    return(input$costLTC1)
  })
  observeEvent(input$costLTC1 , {
    if(!is.numeric(input$costLTC1) || input$costLTC1 < 0) {
      shinyalert("Cost Error", 
                 "Long-term care costs must be greater than or equal to 0.", 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)
  
  costLTC2 <- reactive({
    validate(
      need(input$costLTC2 >= 0, 
           "Long-term care costs must be greater than or equal to 0."))
    return(input$costLTC2)
  })
  observeEvent(input$costLTC2 , {
    if(!is.numeric(input$costLTC2) || input$costLTC2 < 0) {
      shinyalert("Cost Error", 
                 "Long-term care costs must be greater than or equal to 0.", 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)
  
  costED1 <- reactive({
    validate(
      need(input$costED1 >= 0, 
           "Emergency department costs must be greater than or equal to 0."))
    return(input$costED1)
  })
  observeEvent(input$costED1 , {
    if(!is.numeric(input$costED1) || input$costED1 < 0) {
      shinyalert("Cost Error", 
                 "Emergency department costs must be greater than or equal to 0.", 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)
  
  costED2 <- reactive({
    validate(
      need(input$costED2 >= 0, 
           "Emergency department costs must be greater than or equal to 0."))
    return(input$costED2)
  })
  observeEvent(input$costED2 , {
    if(!is.numeric(input$costED2) || input$costED2 < 0) {
      shinyalert("Cost Error", 
                 "Emergency department costs must be greater than or equal to 0.", 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)
  
  costOTHER1 <- reactive({
    validate(
      need(input$costother1 >= 0, 
           "Other costs must be greater than or equal to 0."))
    return(input$costother1)
  })
  observeEvent(input$costother1 , {
    if(!is.numeric(input$costother1) || input$costother1 < 0) {
      shinyalert("Cost Error", 
                 "Other costs must be greater than or equal to 0.", 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)
  
  costOTHER2 <- reactive({
    validate(
      need(input$costother2 >= 0, 
           "Other costs must be greater than or equal to 0."))
    return(input$costother2)
  })
  observeEvent(input$costother2 , {
    if(!is.numeric(input$costother2) || input$costother2 < 0) {
      shinyalert("Cost Error", 
                 "Other costs must be greater than or equal to 0.", 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)
  
  
  costpharm1 <- reactive({
    validate(
      need(input$costpharm1 >= 0, 
           "Pharmacy costs must be greater than or equal to 0."))
    return(input$costpharm1)
  })
  observeEvent(input$costpharm1 , {
    if(!is.numeric(input$costpharm1) || input$costpharm1 < 0) {
      shinyalert("Cost Error", 
                 "Pharmacy costs must be greater than or equal to 0.", 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)
  
  costpharm2 <- reactive({
    validate(
      need(input$costpharm2 >= 0, 
           "Pharmacy costs must be greater than or equal to 0."))
    return(input$costpharm2)
  })
  observeEvent(input$costpharm2 , {
    if(!is.numeric(input$costpharm2) || input$costpharm2 < 0) {
      shinyalert("Cost Error", 
                 "Pharmacy costs must be greater than or equal to 0.", 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)
  
  costprod1 <- reactive({
    validate(
      need(input$costprod1 >= 0, 
           "Productivity loss costs must be greater than or equal to 0."))
    return(input$costprod1)
  })
  observeEvent(input$costprod1 , {
    if(!is.numeric(input$costprod1) || input$costprod1 < 0) {
      shinyalert("Cost Error", 
                 "Productivity loss costs must be greater than or equal to 0.", 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)
  
  costprod2 <- reactive({
    validate(
      need(input$costprod2 >= 0, 
           "Productivity loss costs must be greater than or equal to 0."))
    return(input$costprod2)
  })
  observeEvent(input$costprod2 , {
    if(!is.numeric(input$costprod2) || input$costprod2 < 0) {
      shinyalert("Cost Error", 
                 "Productivity loss costs must be greater than or equal to 0.", 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)
  
  costcare1 <- reactive({
    validate(
      need(input$costcare1 >= 0, 
           "Caregiver costs must be greater than or equal to 0."))
    return(input$costcare1)
  })
  observeEvent(input$costcare1 , {
    if(!is.numeric(input$costcare1) || input$costcare1 < 0) {
      shinyalert("Cost Error", 
                 "Caregiver costs must be greater than or equal to 0.", 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)
  
  costcare2 <- reactive({
    validate(
      need(input$costcare2 >= 0, 
           "Caregiver costs must be greater than or equal to 0."))
    return(input$costcare2)
  })
  observeEvent(input$costcare2 , {
    if(!is.numeric(input$costcare2) || input$costcare2 < 0) {
      shinyalert("Cost Error", 
                 "Caregiver costs must be greater than or equal to 0.", 
                 type = "error")
    }
  },
  ignoreInit = T,
  priority = 500)
###############REACTIVE ACTIONS#####################

# Reactive Function for Simulation Data
sim_data <- reactive({
  progress <- shiny::Progress$new()
  progress$set(message = "Simulating Population", value = 0)
  on.exit(progress$close())
  # Validate Inputs
  population <-     pop_input()
  asian_rate <-     asian_rate() 
  
  bmd_mean <-       BMD_mean()
  bmd_sd <-         BMD_SD()
  
  ra_rate <-        RA_rate()
  fxr_rate <-       FXR_rate()
  parfxr_rate <-    PARFXR_rate()
  smoker_rate <-    SMOKER_rate()
  alcohol_rate <-   ALCO_rate()
  gluco_rate <-     GLUCO_rate()
  
  enable_indirect_costs <- input$IndirectCosts
  dxa_prob <-       Base_Case_ID()
  med_base_prob <-  Base_Case_Treatment()
  
  dxa_prob_s1 <-       S1_ID()
  med_base_prob_s1 <-  S1_Treatment()
  
  costinpt1 <- costinpt1()
  costinpt2 <- costinpt2()
  
  costoutpt1 <- costoutpt1()
  costoutpt2 <- costoutpt2()
  
  costLTC1 <- costLTC1()
  costLTC2 <- costLTC2()
  
  costED1 <- costED1()
  costED2 <- costED2()
  
  costother1 <- costOTHER1()
  costother2 <- costOTHER2()
  
  costpharm1 <- costpharm1()
  costpharm2 <- costpharm2()
  
  costprod1 <- costprod1()
  costprod2 <- costprod2()
  
  costcare1 <- costcare1()
  costcare2 <- costcare2()
  
  ## all-JAPAC additions
  # country <- input$countrySelect
  projection <- country_popn_projections()
  
  
  ##
  
  start_year <- 2018
  end_year   <- as.integer(substring(input$endYear, 1, 4))
  
  progressB <- function() progress$set(value = progress$getValue() + (progress$getMax() - progress$getValue())/(end_year - start_year), detail = "Preparing Plot")
  opts <- list(progress = progressB)
  # Utilize parallelization to increase speed
  return(foreach(i=start_year:2040,
                              .packages = c('readxl',
                                            'hashmap',
                                            'data.table'),
                              .export=c('microsim',
                                        'age_probabilities',
                                        'age_index_scores',
                                        'fracture_breakdown',
                                        'bmd_index_scores',
                                        'bmd_cutoffs',
                                        'ID_lookup',
                                        'HIP_FRACTURE_RATIO',
                                        'MULTI_FRACTURE_FACTOR',
                                        'input',
                                        'isolate',
                                        'getAgeIndex',
                                        'getBMDIndex',
                                        'getRiskFactorIndex',
                                        'getMedicationUtilization',
                                        'getDXAScans',
                                        'getMedPatients',
                                        'getFracture',
                                        'getMultiFraxCost',
                                        'getCostWO',
                                        'getCostWP',
                                        'country_popn_value',
                                        'update_popn_inputs',
                                        'country_cost_value',
                                        'update_cost_inputs',
                                        'country_scenario_value',
                                        'update_scenario_inputs',
                                        'country_other_value',
                                        'country_other_inputs',
                                        'treat_mix',
                                        'treat_cost', 
                                        'treat_efficacy_hip',
                                        'treat_efficacy_other'
                                        ), .verbose = F,
                                        .options.snow = opts) %dopar% {
                               isolate({microsim(population,
                                                 asian_rate,
                                                 bmd_mean,
                                                 bmd_sd,
                                                 ra_rate,
                                                 fxr_rate,
                                                 parfxr_rate,
                                                 smoker_rate,
                                                 alcohol_rate,
                                                 gluco_rate,
                                                 dxa_prob,
                                                 med_base_prob,
                                                 dxa_prob_s1,
                                                 med_base_prob_s1,
                                                 
                                                 costinpt1,
                                                 costinpt2,
                                                 
                                                 costoutpt1,
                                                 costoutpt2,
                                                 
                                                 costLTC1,
                                                 costLTC2,
                                                 
                                                 costED1,
                                                 costED2,
                                                 
                                                 costother1,
                                                 costother2,
                                                 
                                                 costpharm1,
                                                 costpharm2,
                                                 
                                                 costprod1,
                                                 costprod2,
                                                 
                                                 costcare1,
                                                 costcare2,
                                                 i, enable_indirect_costs,
                                                 input$countrySelect, projection)})
                              })
})

###############RENDERING BOXES & PLOTS######################
uiOutput("nlp_sentences_tree")
simulation_data <- reactiveValues(sim = NULL)
  
  
# Summary Info Box Details
output$totalfxr_content <- renderText({
  base_case <- simulation_data$sim
  inp_year <- as.Date(input$endYear, "%Y")
  inp_year <- format(inp_year, "%Y")
  total_frax <- 0
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1
  for(i in 1:duration) {
    total_frax <- total_frax + (base_case[[i]]$total_fractures_with_previous_fracture_s1 - base_case[[i]]$total_fractures_with_previous_fracture)}
  formatted_fxrs <- formatC(abs(round(total_frax)), format = 'd', big.mark=',')
  paste("The total number of fractures is estimated to ", 
                                            ifelse(total_frax > 0, "increase by ", "decrease by "), 
                                             formatted_fxrs, 
                                             " during the years 2018-", inp_year, sep = "", collapse = NULL)
                                            })
output$totalcost_content <- renderText({
  base_case <- simulation_data$sim
  inp_year <- as.Date(input$endYear, "%Y")
  inp_year <- format(inp_year, "%Y")
  total_frax_cost <- 0
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1
  for(i in 1:duration) {
    total_frax_cost <- total_frax_cost + (base_case[[i]]$grand_total_with_prev_frac_s1 - base_case[[i]]$grand_total_with_prev_frac)}
  paste("The total cost is estimated to ", ifelse(total_frax_cost > 0, "increase by ", "decrease by "),
        dollar_format()(abs(total_frax_cost)), 
        " during the years 2018-", inp_year, sep = "", collapse = NULL)
})

output$primaryfxr_content <- renderText({
  base_case <- simulation_data$sim
  inp_year <- as.Date(input$endYear, "%Y")
  inp_year <- format(inp_year, "%Y")
  total_frax <- 0
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1
  for(i in 1:duration) {
    total_frax <- total_frax + (base_case[[i]]$total_fractures_wo_previous_fracture_s1 - base_case[[i]]$total_fractures_wo_previous_fracture)}
  formatted_fxrs <- formatC(abs(round(total_frax)), format = 'd', big.mark=',')
  paste("The total number of primary fractures is estimated to ", 
        ifelse(total_frax > 0, "increase by ", "decrease by "), 
        formatted_fxrs, 
        " during the years 2018-", inp_year, sep = "", collapse = NULL)
})
output$primarycost_content <- renderText({
  base_case <- simulation_data$sim
  inp_year <- as.Date(input$endYear, "%Y")
  inp_year <- format(inp_year, "%Y")
  total_frax_cost <- 0
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1
  for(i in 1:duration) {
    total_frax_cost <- total_frax_cost + (base_case[[i]]$grand_total_wo_prev_frac_s1 - base_case[[i]]$grand_total_wo_prev_frac)}
  paste("The total cost of primary fractures is estimated to ", ifelse(total_frax_cost > 0, "increase by ", "decrease by "),
        dollar_format()(abs(total_frax_cost)), 
        " during the years 2018-", inp_year, sep = "", collapse = NULL)
})


output$secondaryfxr_content <- renderText({
  base_case <- simulation_data$sim
  inp_year <- as.Date(input$endYear, "%Y")
  inp_year <- format(inp_year, "%Y")
  total_frax <- 0
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1
  for(i in 1:duration) {
    total_frax <- total_frax + (base_case[[i]]$total_fractures_with_previous_fracture_s1 - base_case[[i]]$total_fractures_with_previous_fracture)}
  formatted_fxrs <- formatC(abs(round(total_frax)), format = 'd', big.mark=',')
  paste("The total number of primary fractures is estimated to ", 
        ifelse(total_frax > 0, "increase by ", "decrease by "), 
        formatted_fxrs, 
        " during the years 2018-", inp_year, sep = "", collapse = NULL)
})
output$secondarycost_content <- renderText({
  base_case <- simulation_data$sim
  inp_year <- as.Date(input$endYear, "%Y")
  inp_year <- format(inp_year, "%Y")
  total_frax_cost <- 0
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1
  for(i in 1:duration) {
    total_frax_cost <- total_frax_cost + (base_case[[i]]$grand_total_with_prev_frac_s1 - base_case[[i]]$grand_total_with_prev_frac)}
  paste("The total cost of primary fractures is estimated to ", ifelse(total_frax_cost > 0, "increase by ", "decrease by "),
        dollar_format()(abs(total_frax_cost)), 
        " during the years 2018-", inp_year, sep = "", collapse = NULL)
})



output$reocc_text_1 <- renderText({
  base_case <- simulation_data$sim
  total_reoccurence_prob <- 0.0
  base_occur_dist <- c()
  s1_occur_dist <- c()
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1
  for(i in 1:duration) {
    total_reoccurence_prob <- (total_reoccurence_prob) + base_case[[i]]$prob_fracture_given_previous_fractures
    base_occur_dist <- c(base_occur_dist, base_case[[i]]$prob_fracture_given_previous_fractures)
    s1_occur_dist <- c(s1_occur_dist, base_case[[i]]$prob_fracture_given_previous_fractures_s1)
  }
  reoccurence_prob <- 1 - dpoibin(kk = 0, pp = base_occur_dist)
  reoccurence_prob_s1 <- 1 - dpoibin(kk = 0, pp = s1_occur_dist)
  
  change_in_fracture_reocc <- reoccurence_prob_s1 - reoccurence_prob
  
  base <- paste("In the base case, patients with previous fractures experienced subsequent fractures at a rate of", percent(reoccurence_prob), sep = ' ')
  next_text <- paste(base, 'while in the new scenario patients with the same history experienced fractures at a rate of', sep = ' ')
  end_text <- paste (next_text, percent(reoccurence_prob_s1), sep = ' ')
  paste(end_text, '.', sep = '')
  
})

output$fracture_risk_text <- renderText({
  base_case <- simulation_data$sim
  total_reoccurence_prob <- 0.0
  new_frac_prob <- 0.0
  
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1
  for(i in 1:duration) {
    total_reoccurence_prob <- (total_reoccurence_prob) + base_case[[i]]$prob_fracture_given_previous_fractures
    new_frac_prob <- (new_frac_prob) + base_case[[i]]$prob_fracture_given_no_previous_fractures
  }
  risk_ratio <- total_reoccurence_prob/new_frac_prob
  
  base <- paste("In the base case, patients with previous fractures experienced subsequent fractures at a rate of", percent(total_reoccurence_prob/duration), sep = ' ')
  next_text <- paste(base, 'annually while patients without a prior fracture experienced fractures at a rate of', sep = ' ')
  end_text <- paste (next_text, percent(new_frac_prob/duration), sep = ' ')
  paste(end_text, 'annually.', sep = ' ')
  
})

output$fracture_risk_text_s1 <- renderText({
  base_case <- simulation_data$sim
  total_reoccurence_prob <- 0.0
  new_frac_prob <- 0.0
  
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1
  for(i in 1:duration) {
    total_reoccurence_prob <- (total_reoccurence_prob) + base_case[[i]]$prob_fracture_given_previous_fractures_s1
    new_frac_prob <- (new_frac_prob) + base_case[[i]]$prob_fracture_given_no_previous_fractures_s1
  }
  risk_ratio <- total_reoccurence_prob/new_frac_prob
  
  base <- paste("In the new scenario, patients with previous fractures experienced subsequent fractures at a rate of", percent(total_reoccurence_prob/duration), sep = ' ')
  next_text <- paste(base, 'annually while patients without a prior fracture experienced subsequent fractures at a rate of', sep = ' ')
  end_text <- paste (next_text, percent(new_frac_prob/duration), sep = ' ')
  paste(end_text, 'annually.', sep = ' ')
  
})

output$FraxBox_R <- renderInfoBox({
  base_case <- simulation_data$sim
  total_frax <- 0
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1
  for(i in 1:duration) {
    total_frax <- total_frax + (base_case[[i]]$total_fractures_with_previous_fracture_s1 - base_case[[i]]$total_fractures_with_previous_fracture)
  }
  subtitle_text <- ifelse(total_frax > 0, "Efforts to Improve PMO Management Result in Secondary Fracture Incidence Increasing", "Efforts to Improve PMO Management Result in Secondary Fracture Incidence Decreasing")
  inp_year <- as.Date(input$endYear, "%Y")
  inp_year <- format(inp_year, "%Y")
  title_text <- paste("Change in Secondary Fracture Occurrence, 2018-", inp_year, sep = "", collapse = NULL)
  infoBox(
    title = title_text,
    subtitle = subtitle_text, 
    value = formatC(round(total_frax), format = 'd', big.mark=','),
    icon = icon("list"),
    color = "blue", fill = T, width = NULL#3
  )
})

## Primary Frac Info
output$nPrimaryBox <- renderInfoBox({
  base_case <- simulation_data$sim
  total_frax <- 0
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1
  for(i in 1:duration) {
    total_frax <- total_frax + (base_case[[i]]$n_patients_wo_previous_fracture_s1 - base_case[[i]]$n_patients_wo_previous_fracture)
  }
  subtitle_text <- ifelse(total_frax > 0, "Efforts to Improve PMO Management Result in Primary Fracture Patients Increasing", "Efforts to Improve PMO Management Result in Primary Fracture Patients Decreasing")
  inp_year <- as.Date(input$endYear, "%Y")
  inp_year <- format(inp_year, "%Y")
  title_text <- paste("Difference in Primary Fracture Patients, 2018-", inp_year, sep = "", collapse = NULL)
  infoBox(
    title = title_text,
    subtitle = subtitle_text, 
    value = formatC(round(total_frax), format = 'd', big.mark=','),
    icon = icon("list"),
    color = "blue", fill = T, width = NULL#3
  )
})


output$nNoPriorsBox <- renderInfoBox({
  base_case <- simulation_data$sim
  total_frax <- 0
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1
  for(i in 1:duration) {
    total_frax <- total_frax + base_case[[i]]$prev_no_fracs_per_yr
  }
  inp_year <- as.Date(input$endYear, "%Y")
  inp_year <- format(inp_year, "%Y")
  title_text <- paste("Cumulative number of patients with no prior history of fractures, 2018-", inp_year, sep = "", collapse = NULL)
  infoBox(
    title = title_text,
    value = formatC(round(total_frax), format = 'd', big.mark=','),
    icon = icon("list"),
    color = "blue", fill = T, width = NULL#3
  )
})

output$primaryFracBox <- renderInfoBox({
  base_case <- simulation_data$sim
  total_frax <- 0
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1
  for(i in 1:duration) {
    total_frax <- total_frax + (base_case[[i]]$total_fractures_wo_previous_fracture_s1 - base_case[[i]]$total_fractures_wo_previous_fracture)
  }
  subtitle_text <- ifelse(total_frax > 0, "Efforts to Improve PMO Management Result in Primary Fractures Increasing", "Efforts to Improve PMO Management Result in Primary Fractures Decreasing")
  inp_year <- as.Date(input$endYear, "%Y")
  inp_year <- format(inp_year, "%Y")
  title_text <- paste("Difference in Primary Fractures, 2018-", inp_year, sep = "", collapse = NULL)
  infoBox(
    title = title_text,
    subtitle = subtitle_text, 
    value = formatC(round(total_frax), format = 'd', big.mark=','),
    icon = icon("list"),
    color = "green", fill = T, width = NULL#3
  )
})

output$primaryFracCostBox <- renderInfoBox({
  base_case <- simulation_data$sim
  total_frax <- 0
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1
  for(i in 1:duration) {
    total_frax <- total_frax + (base_case[[i]]$grand_total_wo_prev_frac_s1 - base_case[[i]]$grand_total_wo_prev_frac)
  }
  subtitle_text <- ifelse(total_frax > 0, "Efforts to Improve PMO Management Result in Costs of Primary Fractures Increasing", "Efforts to Improve PMO Management Result in Costs of Primary Fractures Decreasing")
  inp_year <- as.Date(input$endYear, "%Y")
  inp_year <- format(inp_year, "%Y")
  title_text <- paste("Difference in Primary Fracture Costs, 2018-", inp_year, sep = "", collapse = NULL)
  infoBox(
    title = title_text,
    subtitle = subtitle_text, 
    value = dollar_format(negative_parens = TRUE)((total_frax)),
    icon = icon("list"),
    color = "orange", fill = T, width = NULL#3
  )
})


## Secondary Frac boxes
output$nPrevBox <- renderInfoBox({
  base_case <- simulation_data$sim
  total_frax <- 0
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1
  for(i in 1:duration) {
    total_frax <- total_frax + (base_case[[i]]$n_patients_with_previous_fracture_s1 - base_case[[i]]$n_patients_with_previous_fracture)
  }
  subtitle_text <- ifelse(total_frax > 0, "Efforts to Improve PMO Management Result in Secondary Fracture Patients Increasing", "Efforts to Improve PMO Management Result in Secondary Fracture Patients Decreasing")
  inp_year <- as.Date(input$endYear, "%Y")
  inp_year <- format(inp_year, "%Y")
  title_text <- paste("Difference in Secondary Fracture Patients, 2018-", inp_year, sep = "", collapse = NULL)
  infoBox(
    title = title_text,
    subtitle = subtitle_text, 
    value = formatC(round(total_frax), format = 'd', big.mark=','),
    icon = icon("list"),
    color = "blue", fill = T, width = NULL#3
  )
})


output$nPriorsBox <- renderInfoBox({
  base_case <- simulation_data$sim
  total_frax <- 0
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1
  for(i in 1:duration) {
    total_frax <- total_frax + base_case[[i]]$prev_fracs_per_yr
  }
  inp_year <- as.Date(input$endYear, "%Y")
  inp_year <- format(inp_year, "%Y")
  title_text <- paste("Cumulative number of patients with prior history of fractures, 2018-", inp_year, sep = "", collapse = NULL)
  infoBox(
    title = title_text,
    value = formatC(round(total_frax), format = 'd', big.mark=','),
    icon = icon("list"),
    color = "blue", fill = T, width = NULL#3
  )
})


output$prevFracBox <- renderInfoBox({
  base_case <- simulation_data$sim
  total_frax <- 0
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1
  for(i in 1:duration) {
    total_frax <- total_frax + (base_case[[i]]$total_fractures_with_previous_fracture_s1 - base_case[[i]]$total_fractures_with_previous_fracture)
  }
  subtitle_text <- ifelse(total_frax > 0, "Efforts to Improve PMO Management Result in Secondary Fractures Increasing", "Efforts to Improve PMO Management Result in Secondary Fractures Decreasing")
  inp_year <- as.Date(input$endYear, "%Y")
  inp_year <- format(inp_year, "%Y")
  title_text <- paste("Difference in Secondary Fractures, 2018-", inp_year, sep = "", collapse = NULL)
  infoBox(
    title = title_text,
    subtitle = subtitle_text, 
    value = formatC(round(total_frax), format = 'd', big.mark=','),
    icon = icon("list"),
    color = "green", fill = T, width = NULL#3
  )
})

output$prevFracCostBox <- renderInfoBox({
  base_case <- simulation_data$sim
  total_frax <- 0
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1
  for(i in 1:duration) {
    total_frax <- total_frax + (base_case[[i]]$grand_total_with_prev_frac_s1 - base_case[[i]]$grand_total_with_prev_frac)
  }
  subtitle_text <- ifelse(total_frax > 0, "Efforts to Improve PMO Management Result in Costs of Secondary Fractures Increasing", "Efforts to Improve PMO Management Result in Costs of Secondary Fractures Decreasing")
  inp_year <- as.Date(input$endYear, "%Y")
  inp_year <- format(inp_year, "%Y")
  title_text <- paste("Difference in Secondary Fracture Costs, 2018-", inp_year, sep = "", collapse = NULL)
  infoBox(
    title = title_text,
    subtitle = subtitle_text, 
    value = dollar_format(negative_parens = TRUE)((total_frax)),
    icon = icon("list"),
    color = "orange", fill = T, width = NULL#3
  )
})

output$CostBox_R <- renderInfoBox({
  base_case <- simulation_data$sim
  total_frax_cost <- (0)
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1
  for(i in 1:duration) {
    total_frax_cost <- (total_frax_cost) + (base_case[[i]]$grand_total_with_prev_frac_s1 - base_case[[i]]$grand_total_with_prev_frac)
  }
  subtitle_text <- ifelse(total_frax_cost > 0, "Efforts to Improve PMO Management Result in Cost Increases", "Efforts to Improve PMO Management Result in Cost Decreases")
  inp_year <- as.Date(input$endYear, "%Y")
  inp_year <- format(inp_year, "%Y")
  title_text <- paste("Change in Total Costs, 2018-", inp_year, sep = "", collapse = NULL)
  infoBox(
    title = title_text,
    subtitle = subtitle_text, 
    value = dollar_format(negative_parens = TRUE)((total_frax_cost)),
    icon = icon("list"),
    color = "orange", fill = T, width = NULL#3
  )
})

output$FraxBox <- renderInfoBox({
  base_case <- simulation_data$sim
  total_frax <- 0
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1
  for(i in 1:duration) {
    total_frax <- total_frax + (base_case[[i]]$total_fractures_with_previous_fracture_s1 - base_case[[i]]$total_fractures_with_previous_fracture)
  }
  subtitle_text <- ifelse(total_frax > 0, "Efforts to Improve PMO Management Result in Fracture Incidence Increasing", "Efforts to Improve PMO Management Result in Fracture Incidence Decreasing")
  inp_year <- as.Date(input$endYear, "%Y")
  inp_year <- format(inp_year, "%Y")
  title_text <- paste("Change in Fracture Occurrence, 2018-", inp_year, sep = "", collapse = NULL)
  infoBox(
    title = title_text,
    subtitle = subtitle_text, 
    value = formatC(round(total_frax), format = 'd', big.mark=','),
    icon = icon("list"),
    color = "blue", fill = T, width = NULL#3
  )
})

output$CostBox <- renderInfoBox({
  base_case <- simulation_data$sim
  total_frax_cost <- (0)
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1
  for(i in 1:duration) {
    total_frax_cost <- (total_frax_cost) + (base_case[[i]]$grand_total_with_prev_frac_s1 - base_case[[i]]$grand_total_with_prev_frac)
  }
  subtitle_text <- ifelse(total_frax_cost > 0, "Efforts to Improve PMO Management Result in Cost Increases", "Efforts to Improve PMO Management Result in Cost Decreases")
  inp_year <- as.Date(input$endYear, "%Y")
  inp_year <- format(inp_year, "%Y")
  title_text <- paste("Change in Total Costs, 2018-", inp_year, sep = "", collapse = NULL)
  infoBox(
    title = title_text,
    subtitle = subtitle_text, 
    value = dollar_format(negative_parens = TRUE)((total_frax_cost)),
    icon = icon("list"),
    color = "orange", fill = T, width = NULL#3
  )
})

output$PrimaryFraxBox <- renderInfoBox({
  base_case <- simulation_data$sim
  total_frax <- 0
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1
  for(i in 1:duration) {
    total_frax <- total_frax + (base_case[[i]]$total_fractures_wo_previous_fracture_s1 - base_case[[i]]$total_fractures_wo_previous_fracture)
  }
  subtitle_text <- ifelse(total_frax > 0, "Efforts to Improve PMO Management Result in Primary Fracture Incidence Increasing", "Efforts to Improve PMO Management Result in Primary Fracture Incidence Decreasing")
  inp_year <- as.Date(input$endYear, "%Y")
  inp_year <- format(inp_year, "%Y")
  title_text <- paste("Change in Primary Fracture Occurrence, 2018-", inp_year, sep = "", collapse = NULL)
  infoBox(
    title = title_text,
    subtitle = subtitle_text, 
    value = formatC(round(total_frax), format = 'd', big.mark=','),
    icon = icon("list"),
    color = "blue", fill = T, width = NULL#3
  )
})

output$PrimaryCostBox <- renderInfoBox({
  base_case <- simulation_data$sim
  total_frax_cost <- (0)
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1
  for(i in 1:duration) {
    total_frax_cost <- (total_frax_cost) + (base_case[[i]]$grand_total_wo_prev_frac_s1 - base_case[[i]]$grand_total_wo_prev_frac)
  }
  subtitle_text <- ifelse(total_frax_cost > 0, "Efforts to Improve PMO Management Result in Primary Cost Increases", "Efforts to Improve PMO Management Result in Primary Cost Decreases")
  inp_year <- as.Date(input$endYear, "%Y")
  inp_year <- format(inp_year, "%Y")
  title_text <- paste("Change in Total Primary Costs, 2018-", inp_year, sep = "", collapse = NULL)
  infoBox(
    title = title_text,
    subtitle = subtitle_text, 
    value = dollar_format(negative_parens = TRUE)((total_frax_cost)),
    icon = icon("list"),
    color = "orange", fill = T, width = NULL#3
  )
})


output$SecondaryFraxBox <- renderInfoBox({
  base_case <- simulation_data$sim
  total_frax <- 0
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1
  for(i in 1:duration) {
    total_frax <- total_frax + (base_case[[i]]$total_fractures_with_previous_fracture_s1 - base_case[[i]]$total_fractures_with_previous_fracture)
  }
  subtitle_text <- ifelse(total_frax > 0, "Efforts to Improve PMO Management Result in Secondary Fracture Incidence Increasing", "Efforts to Improve PMO Management Result in Secondary Fracture Incidence Decreasing")
  inp_year <- as.Date(input$endYear, "%Y")
  inp_year <- format(inp_year, "%Y")
  title_text <- paste("Change in Secondary Fracture Occurrence, 2018-", inp_year, sep = "", collapse = NULL)
  infoBox(
    title = title_text,
    subtitle = subtitle_text, 
    value = formatC(round(total_frax), format = 'd', big.mark=','),
    icon = icon("list"),
    color = "blue", fill = T, width = NULL#3
  )
})

output$SecondaryCostBox <- renderInfoBox({
  base_case <- simulation_data$sim
  total_frax_cost <- (0)
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1
  for(i in 1:duration) {
    total_frax_cost <- (total_frax_cost) + (base_case[[i]]$grand_total_with_prev_frac_s1 - base_case[[i]]$grand_total_with_prev_frac)
  }
  subtitle_text <- ifelse(total_frax_cost > 0, "Efforts to Improve PMO Management Result in Secondary Cost Increases", "Efforts to Improve PMO Management Result in Secondary Cost Decreases")
  inp_year <- as.Date(input$endYear, "%Y")
  inp_year <- format(inp_year, "%Y")
  title_text <- paste("Change in Total Secondary Costs, 2018-", inp_year, sep = "", collapse = NULL)
  infoBox(
    title = title_text,
    subtitle = subtitle_text, 
    value = dollar_format(negative_parens = TRUE)((total_frax_cost)),
    icon = icon("list"),
    color = "orange", fill = T, width = NULL#3
  )
})



  output$costp <-  renderPlotly({
    costid <- rbind("One per Year", "> One per Year", stringAsFactors = TRUE)
    inpt <- rbind(as.numeric(input$costinpt1), as.numeric(input$costinpt2))
    outpt <- rbind(as.numeric(input$costoutpt1), as.numeric(input$costoutpt2))
    LTC <- rbind(as.numeric(input$costLTC1), as.numeric(input$costLTC2))
    ED <- rbind(as.numeric(input$costED1), as.numeric(input$costED2))
    other <- rbind(as.numeric(input$costother1), as.numeric(input$costother2))
    pharm <- rbind(as.numeric(input$costpharm1), as.numeric(input$costpharm2))
    allcost.df <- data.frame(inpt, outpt, LTC, ED, other, pharm, stringsAsFactors=FALSE)
    
    costs <- rbind(as.numeric(input$costinpt1), as.numeric(input$costoutpt1), as.numeric(input$costLTC1), as.numeric(input$costED1), as.numeric(input$costother1), as.numeric(input$costpharm1), as.numeric(input$costinpt2), as.numeric(input$costoutpt2), as.numeric(input$costLTC2), as.numeric(input$costED2), as.numeric(input$costother2), as.numeric(input$costpharm2))
    costsd <- prettyNum(costs, big.mark =",",scientific=FALSE)
    costsx <- c("One per Year", "> One per Year")
    costsx <- factor(costsx, levels = costsx)
    allcostdf <- allcost.df[, c(input$inpt, input$outpt, input$LTC, input$ED, input$other, input$pharm),]
    plot_ly(allcost.df, x = ~costsx, y = ~inpt, type = 'bar', name = 'Inpatient', colors = "GnBu", color = as.factor('first_trace'), 
            hoverinfo = 'text', 
            text = ~paste('Cost:', inpt),
            hoverformat ="$,f") %>%
       config(displayModeBar = F)%>%
      add_trace(y = ~outpt, name = 'Outpatient', color = as.factor('second_trace')) %>%
      add_trace(y = ~LTC, name = 'Long-term Care', color = as.factor('third_trace')) %>%
      add_trace(y = ~ED, name = 'Emergency Department', color = as.factor('fourth_trace')) %>%
      add_trace(y = ~other, name = 'Other', color = as.factor('fifth_trace')) %>%
      add_trace(y = ~pharm, name = 'Pharmacy', color = as.factor('sixth_trace')) %>%
      layout(
        yaxis = list(title = 'Costs ($)'),
        xaxis = list(title = 'Fracture Frequency', zeroline = FALSE, showgrid = FALSE), 
        barmode = 'stack')
  })
  # Fractures Plot
  output$fxrplot <- renderPlotly({
    
    # progress <- shiny::Progress$new()
    # progress$set(message = "Simulating Population", value = 0)
    # on.exit(progress$close())
    
    
    sim <- simulation_data$sim
    # progress$set(value = progress$getValue() + (progress$getMax() - progress$getValue())/3, detail = "Preparing Plot")

    start_year <- 2018
    end_year   <- as.integer(substring(input$endYear, 1, 4))
    
    xbc <- c(start_year:end_year)
    ybc <- c()
    ys1 <- c()


    for(i in 1:length(xbc)) {
      if(i > 1) {
        ybc <- cbind(ybc, sim[[i]]$total_fractures + ybc[i-1])
        ys1 <- cbind(ys1, sim[[i]]$total_fractures_s1 + ys1[i-1])  
      } else {
        ybc <- cbind(ybc, sim[[i]]$total_fractures)
        ys1 <- cbind(ys1, sim[[i]]$total_fractures_s1)
      }
    }
    dummybc <- data.frame(xbc, ybc, ys1)
    # progress$set(value = progress$getValue() + (progress$getMax() - progress$getValue())/3, detail = "Preparing Plot")
    color_pal <- brewer.pal(3, "Paired")
    p <- plot_ly(dummybc, x = ~xbc) %>% 
      add_trace(y = ~ybc, name = "Base Case", type = 'scatter', mode = 'lines', line = list(color = color_pal[1]), text = ~paste('<br>Base Case'), hoverinfo="text+x+y" ) %>% 
      add_trace(y = ~ys1, name = "Improved PMO Management", type = 'scatter', mode = 'lines', line = list(color = color_pal[2]),text = ~paste('<br>Improved PMO Management'), hoverinfo="text+x+y") %>%
      config(displayModeBar = F) %>%
      layout(
        title = "Cumulative Fractures vs. Time",
        xaxis = list(showgrid = FALSE,
                     title = "Year",
                     zeroline = TRUE
        ),
        yaxis = list(
          title = "Total Number of Fractures",
          zeroline = TRUE
        ),
        xaxis = list(
          title = "Year",
          zeroline = TRUE
        )
      )
    return(p)
  })
  
  # Previous Fractures Plot
  output$prevFracPlot <- renderPlotly({
    
    
    sim <- simulation_data$sim
    
    start_year <- 2018
    end_year   <- as.integer(substring(input$endYear, 1, 4))
    
    xbc <- c(start_year:end_year)
    ybc <- c()
    ys1 <- c()
    
    print(sim)
    for(i in 1:length(xbc)) {
      if(i > 1) {
        ybc <- cbind(ybc, sim[[i]]$total_fractures_with_previous_fracture + ybc[i-1])
        ys1 <- cbind(ys1, sim[[i]]$total_fractures_with_previous_fracture_s1 + ys1[i-1])
      } else {
        ybc <- cbind(ybc, sim[[i]]$total_fractures_with_previous_fracture)
        ys1 <- cbind(ys1, sim[[i]]$total_fractures_with_previous_fracture_s1)
      }
    }
    dummybc <- data.frame(xbc, ybc, ys1)
    color_pal <- brewer.pal(3, "Paired")
    p <- plot_ly(dummybc, x = ~xbc) %>% 
      add_trace(y = ~as.integer(ybc), name = "Base Case", type = 'scatter', mode = 'lines', line = list(color = color_pal[1]), text = ~paste('<br>Base Case'), hoverinfo="text+x+y" ) %>% 
      add_trace(y = ~as.integer(ys1), name = "Improved PMO Management", type = 'scatter', mode = 'lines', line = list(color = color_pal[2]),text = ~paste('<br>Improved PMO Management'), hoverinfo="text+x+y") %>%
      config(displayModeBar = F) %>%
      layout(
        title = "Cumulative Secondary Fractures vs. Time",
        xaxis = list(showgrid = FALSE,
                     title = "Year",
                     zeroline = TRUE
        ),
        yaxis = list(
          title = "Total Number of Secondary Fractures",
          zeroline = TRUE
        ),
        xaxis = list(
          title = "Year",
          zeroline = TRUE
        )
      )
    return(p)
  })
  
  # No Previous Fractures Plot
  output$primaryFracPlot <- renderPlotly({
    
    
    sim <- simulation_data$sim
    
    start_year <- 2018
    end_year   <- as.integer(substring(input$endYear, 1, 4))
    
    xbc <- c(start_year:end_year)
    ybc <- c()
    ys1 <- c()
    
    for(i in 1:length(xbc)) {
      if(i > 1) {
        ybc <- cbind(ybc, sim[[i]]$total_fractures_wo_previous_fracture + ybc[i-1])
        ys1 <- cbind(ys1, sim[[i]]$total_fractures_wo_previous_fracture_s1 + ys1[i-1])  
      } else {
        ybc <- cbind(ybc, sim[[i]]$total_fractures_wo_previous_fracture)
        ys1 <- cbind(ys1, sim[[i]]$total_fractures_wo_previous_fracture_s1)
      }
    }
    dummybc <- data.frame(xbc, ybc, ys1)
    color_pal <- brewer.pal(3, "Paired")
    p <- plot_ly(dummybc, x = ~xbc) %>% 
      add_trace(y = ~as.integer(ybc), name = "Base Case", type = 'scatter', mode = 'lines', line = list(color = color_pal[1]), text = ~paste('<br>Base Case'), hoverinfo="text+x+y" ) %>% 
      add_trace(y = ~as.integer(ys1), name = "Improved PMO Management", type = 'scatter', mode = 'lines', line = list(color = color_pal[2]),text = ~paste('<br>Improved PMO Management'), hoverinfo="text+x+y") %>%
      config(displayModeBar = F) %>%
      layout(
        title = "Cumulative Primary Fractures vs. Time",
        xaxis = list(showgrid = FALSE,
                     title = "Year",
                     zeroline = TRUE
        ),
        yaxis = list(
          title = "Total Number of Primary Fractures",
          zeroline = TRUE
        ),
        xaxis = list(
          title = "Year",
          zeroline = TRUE
        )
      )
    return(p)
  })
  
  # Number of Primary Fracture patient plots
  output$nPrimaryPlot <- renderPlotly({
    
    
    sim <- simulation_data$sim
    
    start_year <- 2018
    end_year   <- as.integer(substring(input$endYear, 1, 4))
    
    xbc <- c(start_year:end_year)
    ybc <- c()
    ys1 <- c()
    
    for(i in 1:length(xbc)) {
      if(i > 1) {
        ybc <- cbind(ybc, sim[[i]]$n_patients_wo_previous_fracture + ybc[i-1])
        ys1 <- cbind(ys1, sim[[i]]$n_patients_wo_previous_fracture_s1 + ys1[i-1])
      } else {
        ybc <- cbind(ybc, sim[[i]]$n_patients_wo_previous_fracture)
        ys1 <- cbind(ys1, sim[[i]]$n_patients_wo_previous_fracture_s1)
      }
    }
    dummybc <- data.frame(xbc, ybc, ys1)
    color_pal <- brewer.pal(3, "Paired")
    p <- plot_ly(dummybc, x = ~xbc) %>% 
      add_trace(y = ~as.integer(ybc), name = "Base Case", type = 'scatter', mode = 'lines', line = list(color = color_pal[1]), text = ~paste('<br>Base Case'), hoverinfo="text+x+y" ) %>% 
      add_trace(y = ~as.integer(ys1), name = "Improved PMO Management", type = 'scatter', mode = 'lines', line = list(color = color_pal[2]),text = ~paste('<br>Improved PMO Management'), hoverinfo="text+x+y") %>%
      config(displayModeBar = F) %>%
      layout(
        title = "Cumulative Primary Fracture Patients vs. Time",
        xaxis = list(showgrid = FALSE,
                     title = "Year",
                     zeroline = TRUE
        ),
        yaxis = list(
          title = "Total Number of Primary Fracture Patients",
          zeroline = TRUE
        ),
        xaxis = list(
          title = "Year",
          zeroline = TRUE
        )
      )
    return(p)
  })
  
  # number of people each year with no prior fractures
  output$nNoPriorsPlot <- renderPlotly({
    
    
    sim <- simulation_data$sim
    
    start_year <- 2018
    end_year   <- as.integer(substring(input$endYear, 1, 4))
    
    xbc <- c(start_year:end_year)
    ybc <- c()
    ys1 <- c()
    
    for(i in 1:length(xbc)) {
      if(i > 1) {
        ybc <- cbind(ybc, sim[[i]]$prev_no_fracs_per_yr + ybc[i-1])
      } else {
        ybc <- cbind(ybc, sim[[i]]$prev_no_fracs_per_yr)
      }
    }
    dummybc <- data.frame(xbc, ybc)
    color_pal <- brewer.pal(3, "Paired")
    p <- plot_ly(dummybc, x = ~xbc) %>% 
      add_trace(y = ~as.integer(ybc), name = "Base Case", type = 'scatter', mode = 'lines', line = list(color = color_pal[1]), text = ~paste('<br>Base Case'), hoverinfo="text+x+y" ) %>% 
      config(displayModeBar = F) %>%
      layout(
        title = "Cumulative Population without Previous Fracture vs. Time",
        xaxis = list(showgrid = FALSE,
                     title = "Year",
                     zeroline = TRUE
        ),
        yaxis = list(
          title = "Total Population without Previous Fracture",
          zeroline = TRUE
        ),
        xaxis = list(
          title = "Year",
          zeroline = TRUE
        )
      )
    return(p)
  })
  
  # Number of Secondary Fracture patient plots
  output$nPrevPlot <- renderPlotly({
    
    
    sim <- simulation_data$sim
    
    start_year <- 2018
    end_year   <- as.integer(substring(input$endYear, 1, 4))
    
    xbc <- c(start_year:end_year)
    ybc <- c()
    ys1 <- c()
    
    for(i in 1:length(xbc)) {
      if(i > 1) {
        ybc <- cbind(ybc, sim[[i]]$n_patients_with_previous_fracture + ybc[i-1])
        ys1 <- cbind(ys1, sim[[i]]$n_patients_with_previous_fracture_s1 + ys1[i-1])
      } else {
        ybc <- cbind(ybc, sim[[i]]$n_patients_with_previous_fracture)
        ys1 <- cbind(ys1, sim[[i]]$n_patients_with_previous_fracture_s1)
      }
    }
    dummybc <- data.frame(xbc, ybc, ys1)
    color_pal <- brewer.pal(3, "Paired")
    p <- plot_ly(dummybc, x = ~xbc) %>% 
      add_trace(y = ~as.integer(ybc), name = "Base Case", type = 'scatter', mode = 'lines', line = list(color = color_pal[1]), text = ~paste('<br>Base Case'), hoverinfo="text+x+y" ) %>% 
      add_trace(y = ~as.integer(ys1), name = "Improved PMO Management", type = 'scatter', mode = 'lines', line = list(color = color_pal[2]),text = ~paste('<br>Improved PMO Management'), hoverinfo="text+x+y") %>%
      config(displayModeBar = F) %>%
      layout(
        title = "Cumulative Secondary Fracture Patients vs. Time",
        xaxis = list(showgrid = FALSE,
                     title = "Year",
                     zeroline = TRUE
        ),
        yaxis = list(
          title = "Total Number of Secondary Fracture Patients",
          zeroline = TRUE
        ),
        xaxis = list(
          title = "Year",
          zeroline = TRUE
        )
      )
    return(p)
  })
  
  # number of population each year that had previous fractures
  output$nPriorsPlot <- renderPlotly({
    
    
    sim <- simulation_data$sim
    
    start_year <- 2018
    end_year   <- as.integer(substring(input$endYear, 1, 4))
    
    xbc <- c(start_year:end_year)
    ybc <- c()
    ys1 <- c()
    
    for(i in 1:length(xbc)) {
      if(i > 1) {
        ybc <- cbind(ybc, sim[[i]]$prev_fracs_per_yr + ybc[i-1])
      } else {
        ybc <- cbind(ybc, sim[[i]]$prev_fracs_per_yr)
      }
    }
    dummybc <- data.frame(xbc, ybc)
    color_pal <- brewer.pal(3, "Paired")
    p <- plot_ly(dummybc, x = ~xbc) %>% 
      add_trace(y = ~as.integer(ybc), name = "Base Case", type = 'scatter', mode = 'lines', line = list(color = color_pal[1]), text = ~paste('<br>Base Case'), hoverinfo="text+x+y" ) %>% 
      config(displayModeBar = F) %>%
      layout(
        title = "Cumulative Population with Previous Fracture vs. Time",
        xaxis = list(showgrid = FALSE,
                     title = "Year",
                     zeroline = TRUE
        ),
        yaxis = list(
          title = "Total Population with Previous Fracture",
          zeroline = TRUE
        ),
        xaxis = list(
          title = "Year",
          zeroline = TRUE
        )
      )
    return(p)
  })
  
  # Cumulative Cost Plot
  output$costplot <- renderPlotly({
    
    sim <- simulation_data$sim
    
    start_year <- 2018
    end_year   <- as.integer(substring(input$endYear, 1, 4))
    
    xbc <- c(start_year:end_year)
    costybc <- c()
    costys1 <- c()

    for(i in 1:length(xbc)) {
      if(i > 1) {
        costybc <- cbind(costybc, sim[[i]]$grand_total + costybc[i-1])
        costys1 <- cbind(costys1, sim[[i]]$grand_total_s1 + costys1[i-1])
      } else {
        costybc <- cbind(costybc, sim[[i]]$grand_total)
        costys1 <- cbind(costys1, sim[[i]]$grand_total_s1) 
      }
    }
    
    dummybc <- data.frame(xbc, costybc, costys1)#, frames)
    color_pal <- brewer.pal(3, "Paired")
    p <- plot_ly(dummybc, 
                 x = ~xbc) %>%
         add_trace(y = ~costybc, name = "Base Case", type = 'scatter', 
                   type = 'scatter', mode = "markers", marker = list(color = color_pal[1]), text = ~paste('<br>Base Case'), hoverinfo="text+x+y" ) %>%
         add_trace(y = ~costys1, name = "Improved PMO Management", type = 'scatter',
                   type = 'scatter', mode = "markers", marker = list(color = color_pal[2]), text = ~paste('<br>Improved PMO Management'), hoverinfo="text+x+y") %>%
              
              config(displayModeBar = F) %>%
              layout(
                title = "Cumulative Total Cost vs. Time",
                xaxis = list(showgrid = FALSE,
                  title = "Year",
                  zeroline = TRUE
                ),
                yaxis = list(
                  title = "Total Cost ($)",
                  zeroline = TRUE
                )
              ) 
  })
  
  
  # Cumulative Subsequent Fractures Cost Plot
  output$prevFracCost <- renderPlotly({

    sim <- simulation_data$sim

    start_year <- 2018
    end_year   <- as.integer(substring(input$endYear, 1, 4))

    xbc <- c(start_year:end_year)
    costybc <- c()
    costys1 <- c()

    for(i in 1:length(xbc)) {
      if(i > 1) {
        costybc <- cbind(costybc, sim[[i]]$grand_total_with_prev_frac + costybc[i-1])
        costys1 <- cbind(costys1, sim[[i]]$grand_total_with_prev_frac_s1 + costys1[i-1])
      } else {
        costybc <- cbind(costybc, sim[[i]]$grand_total_with_prev_frac)
        costys1 <- cbind(costys1, sim[[i]]$grand_total_with_prev_frac_s1)
      }
    }

    dummybc <- data.frame(xbc, costybc, costys1)
    color_pal <- brewer.pal(3, "Paired")
    p <- plot_ly(dummybc,
                 x = ~xbc) %>%
      add_trace(y = ~costybc, name = "Base Case", type = 'scatter',
                type = 'scatter', mode = "markers", marker = list(color = color_pal[1]), text = ~paste('<br>Base Case'), hoverinfo="text+x+y" ) %>%
      add_trace(y = ~costys1, name = "Improved PMO Management", type = 'scatter',
                type = 'scatter', mode = "markers", marker = list(color = color_pal[2]), text = ~paste('<br>Improved PMO Management'), hoverinfo="text+x+y") %>%

      config(displayModeBar = F) %>%
      layout(
        title = "Cumulative Total Costs among Secondary Fracture Population by Year",
        xaxis = list(showgrid = FALSE,
                     title = "Year",
                     zeroline = TRUE
        ),
        yaxis = list(
          title = "Total Cost ($)",
          zeroline = TRUE
        )
      )
  })

  # Cumulative Primary Fractures Cost Plot
  output$primaryFracCost <- renderPlotly({

    sim <- simulation_data$sim

    start_year <- 2018
    end_year   <- as.integer(substring(input$endYear, 1, 4))

    xbc <- c(start_year:end_year)
    costybc <- c()
    costys1 <- c()

    for(i in 1:length(xbc)) {
      if(i > 1) {
        costybc <- cbind(costybc, sim[[i]]$grand_total_wo_prev_frac + costybc[i-1])
        costys1 <- cbind(costys1, sim[[i]]$grand_total_wo_prev_frac_s1 + costys1[i-1])
      } else {
        costybc <- cbind(costybc, sim[[i]]$grand_total_wo_prev_frac)
        costys1 <- cbind(costys1, sim[[i]]$grand_total_wo_prev_frac_s1)
      }
    }

    dummybc <- data.frame(xbc, costybc, costys1)
    color_pal <- brewer.pal(3, "Paired")
    p <- plot_ly(dummybc,
                 x = ~xbc) %>%
      add_trace(y = ~costybc, name = "Base Case", type = 'scatter',
                type = 'scatter', mode = "markers", marker = list(color = color_pal[1]), text = ~paste('<br>Base Case'), hoverinfo="text+x+y" ) %>%
      add_trace(y = ~costys1, name = "Improved PMO Management", type = 'scatter',
                type = 'scatter', mode = "markers", marker = list(color = color_pal[2]), text = ~paste('<br>Improved PMO Management'), hoverinfo="text+x+y") %>%

      config(displayModeBar = F) %>%
      layout(
        title = "Cumulative Total Costs Among the Primary Fracture Population by Year",
        xaxis = list(showgrid = FALSE,
                     title = "Year",
                     zeroline = TRUE
        ),
        yaxis = list(
          title = "Total Cost ($)",
          zeroline = TRUE
        )
      )
  })
  
  
  output$FractureReoccurence <- renderInfoBox({
    base_case <- simulation_data$sim
    total_reoccurence_prob <- 0.0
    base_occur_dist <- c()
    s1_occur_dist <- c()
    duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1
    for(i in 1:duration) {
      total_reoccurence_prob <- (total_reoccurence_prob) + base_case[[i]]$prob_fracture_given_previous_fractures
      base_occur_dist <- c(base_occur_dist, base_case[[i]]$prob_fracture_given_previous_fractures)
      s1_occur_dist <- c(s1_occur_dist, base_case[[i]]$prob_fracture_given_previous_fractures_s1)
    }
    #reoccurence_prob <- dpoibin(kk = 1, pp = base_occur_dist)
    #reoccurence_prob_s1 <- dpoibin(kk = 1, pp = s1_occur_dist)
    reoccurence_prob <- 1 - dpoibin(kk = 0, pp = base_occur_dist)
    reoccurence_prob_s1 <- 1 - dpoibin(kk = 0, pp = s1_occur_dist)
    
    change_in_fracture_reocc <- reoccurence_prob_s1 - reoccurence_prob
    
    subtitle_text <- ifelse(change_in_fracture_reocc > 0,
                            "Change to New Scenario Results in Increased Probability of a Fracture Given a Previous Fracture",
                            "Change to New Scenario Results in Decreased Probability of a Fracture Given a Previous Fracture")
    inp_year <- as.Date(input$endYear, "%Y")
    inp_year <- format(inp_year, "%Y")
    title_text <- paste("Change in Fracture Reccurence Probability, 2018-", inp_year, sep = "", collapse = NULL)
    infoBox(
      title = title_text,
      subtitle = subtitle_text, 
      value = percent(change_in_fracture_reocc),
      icon = icon("list"),
      color = "teal", fill = T, width = NULL#3
    )
  })
  
  output$FractureRatio <- renderInfoBox({
    base_case <- simulation_data$sim
    total_reoccurence_prob <- 0.0
    new_frac_prob <- 0.0

    duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1 
    for(i in 1:duration) {
      total_reoccurence_prob <- (total_reoccurence_prob) + base_case[[i]]$prob_fracture_given_previous_fractures
      new_frac_prob <- (new_frac_prob) + base_case[[i]]$prob_fracture_given_no_previous_fractures
    }
    risk_ratio <- total_reoccurence_prob/new_frac_prob
    
    
    subtitle_text <- ifelse(risk_ratio > 1,
                            "Patients with a History of Previous Fractures are More Likely to Experience Subsequent Fractures",
                            "Patients with a History of Previous Fractures are Less Likely to Experience Subsequent Fractures")
    inp_year <- as.Date(input$endYear, "%Y")
    inp_year <- format(inp_year, "%Y")
    title_text <- paste("Base Case Fracture Risk Ratio, 2018-", inp_year, sep = "", collapse = NULL)
    infoBox(
      title = title_text,
      subtitle = subtitle_text, 
      value = format(round(risk_ratio, 2), nsmall = 2),
      icon = icon("list"),
      color = "purple", fill = T, width = NULL#3
    )
  })
  
  output$FractureRatioS1 <- renderInfoBox({
    base_case <- simulation_data$sim
    total_reoccurence_prob <- 0.0
    new_frac_prob <- 0.0
    
    duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018 + 1
    for(i in 1:duration) {
      total_reoccurence_prob <- (total_reoccurence_prob) + base_case[[i]]$prob_fracture_given_previous_fractures_s1
      new_frac_prob <- (new_frac_prob) + base_case[[i]]$prob_fracture_given_no_previous_fractures_s1
    }
    risk_ratio <- total_reoccurence_prob/new_frac_prob
    
    
    subtitle_text <- ifelse(risk_ratio > 1,
                            "Patients with a History of Previous Fractures are More Likely to Experience Subsequent Fractures",
                            "Patients with a History of Previous Fractures are Less Likely to Experience Subsequent Fractures")
    inp_year <- as.Date(input$endYear, "%Y")
    inp_year <- format(inp_year, "%Y")
    title_text <- paste("New Scenario Fracture Risk Ratio, 2018-", inp_year, sep = "", collapse = NULL)
    infoBox(
      title = title_text,
      subtitle = subtitle_text, 
      value = format(round(risk_ratio, 2), nsmall = 2),
      icon = icon("list"),
      color = "purple", fill = T, width = NULL#3
    )
  })
  
  output$PrevFracPerYear <- renderInfoBox({
    
    sim <- simulation_data$sim
    
    start_year <- 2018
    end_year   <- as.integer(substring(input$endYear, 1, 4))
    
    xbc <- c(start_year:end_year)
    costybc <- c()
    costys1 <- c()
    
    for(i in 1:length(xbc)) {
      if(i > 1) {
        costybc <- cbind(costybc, sim[[i]]$prev_fracs_per_year + costybc[i-1])
      } else {
        costybc <- cbind(costybc, sim[[i]]$prev_fracs_per_year)
      }
    }
    
    infoBox(
      title = 'cum. prev. fracs per year',
      subtitle = 'subtitle_text', 
      value = format(round(costybc, 2), nsmall = 2),
      icon = icon("list"),
      color = "purple", fill = T, width = NULL#3
    )  
    
    })
  
  
  
###############Priority Manipulation###################################
outputOptions(output, "costp", priority = 1)
outputOptions(output, "fxrplot", priority = 1)
outputOptions(output, "totalfxr_content", priority = 0)
outputOptions(output, 'totalcost_content', priority = 0)
outputOptions(output, 'FraxBox', priority = 0)
outputOptions(output, 'CostBox', priority = 0)
outputOptions(output, 'FraxBox_R', priority = 0)
outputOptions(output, 'CostBox_R', priority = 0)
}



