###########################VARIABLES##############################
abbreviations <- data.frame(abbrev = c("BMD", "CPI", "DXA", "FRAX", "NHANES"),
                            full = c("bone mineral density", "Consumer Price Index", "dual-energy X-ray absorptiometry", "Fracture Risk Assessment Tool", "National Health and Nutrition Examination Survey"))

tab_id <- c("Home", "Overview", "Inputs", "Fracture", "Scenarios", "Results", "Assumptions", "Disclosures", "Terms", "References")



#################################################################
function(input, output, session) {
  output$value <- renderText({ input$inpt })
  

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
    reset("pop_input")
    reset("BMD_mean")
    reset("BMD_SD")
    reset("RA_inp")
    reset("fxr_inp")
    reset("parfxr_inp")
    reset("smoker")
    reset("alco")
    reset("gluco_tx")
    reset("RE_cauc")
    reset("RE_hisp")
    reset("RE_asian")
    reset("RE_black")
   
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
  
  observeEvent(input$restorepop, {
    reset("pop_input")
    reset("BMD_mean")
    reset("BMD_SD")
    reset("RA_inp")
    reset("fxr_inp")
    reset("parfxr_inp")
    reset("smoker")
    reset("alco")
    reset("gluco_tx")
    reset("RE_cauc")
    reset("RE_hisp")
    reset("RE_asian")
    reset("RE_black")
  })  
  
  observeEvent(input$restorefxrcosts, {
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

###############REACTIVE ACTIONS#####################
# TODO
# Make Copies of microsim function for 3 additional scenarios
# Put scenarios in graph
# Cumulative plot at bottom
# labels in charts, pretty them up
   
  
sim_data <- reactive({
  population <- input$pop_input
  caucasian_rate <- input$RE_cauc/100.0
  hispanic_rate <-  input$RE_hisp/100.0
  asian_rate <-     input$RE_asian/100.0 
  black_rate <-     input$RE_black/100.0
  
  bmd_mean <-       input$BMD_mean
  bmd_sd <-         input$BMD_SD
  
  ra_rate <-        input$RA_inp/100.0
  fxr_rate <-       input$fxr_inp/100.0
  parfxr_rate <-    input$parfxr_inp/100.0
  smoker_rate <-    input$smoker/100.0
  alcohol_rate <-   input$alco/100.0
  gluco_rate <-     input$gluco_tx/100.0
  
  dxa_prob <-       input$basecaseID/100.0
  med_base_prob <-  input$basecaseTx/100.0
  
  dxa_prob_s1 <-       input$scenario1ID/100.0
  med_base_prob_s1 <-  input$scenario1Tx/100.0
  
  costinpt1 <- input$costinpt1
  costinpt2 <- input$costinpt2
  
  costoutpt1 <- input$costoutpt1
  costoutpt2 <- input$costoutpt2
  
  costLTC1 <- input$costLTC1
  costLTC2 <- input$costLTC2
  
  costED1 <- input$costED1
  costED2 <- input$costED2
  
  costother1 <- input$costother1
  costother2 <- input$costother2
  
  costpharm1 <- input$costpharm1
  costpharm2 <- input$costpharm2
  
  start_year <- 2018
  end_year   <- as.integer(substring(input$endYear, 1, 4))
  
  print(start_year)
  print(end_year)
  # TODO make years dynamic, single slider for years?
  return(foreach(i=start_year:end_year,
                              .packages = c('readxl',
                                            'hashmap'),
                              .export=c('microsim',
                                        'age_probabilities',
                                        'minimum_age',
                                        'maximum_age',
                                        'age_cutoffs',
                                        'age_index_scores',
                                        'race_categories',
                                        'race_index_scores',
                                        'fracture_breakdown',
                                        'centering_mean',
                                        'bmd_index_scores',
                                        'bmd_cutoffs',
                                        'ID_lookup',
                                        #'id_to_frax_hash', TODO investigate if hashmaps can be passed directly
                                        #'MAX_HIP_FRACTURE_RATE',
                                        #'id_to_frax_major_hash',
                                        #'MAX_MAJOR_FRACTURE_RATE',
                                        'MEDICATION_ADHERENCE',
                                        'HIP_FRACTURE_RATIO',
                                        'MULTI_FRACTURE_FACTOR',
                                        'input',
                                        'isolate',
                                        'getAgeIndex',
                                        'getRaceIndex',
                                        'getBMDIndex',
                                        'getRiskFactorIndex',
                                        'getMedicationUtilization',
                                        'getDXAScans',
                                        'getMedPatients',
                                        'getFracture',
                                        'getMultiFraxCost'
                                        ), .verbose = T) %dopar% {
                               isolate({microsim(population,
                                                 caucasian_rate,
                                                 hispanic_rate,
                                                 asian_rate,
                                                 black_rate,
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
                                                 i, 0)})
                              })
})

###############RENDERING BOXES & PLOTS######################
uiOutput("nlp_sentences_tree")

output$totalfxr_content <- renderText({
  base_case <- sim_data()
  inp_year <- as.Date(input$endYear, "%Y")
  inp_year <- format(inp_year, "%Y")
  total_frax <- 0
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018
  for(i in 1:duration) {
    total_frax <- total_frax + (base_case[[i]]$total_fractures_s1 - base_case[[i]]$total_fractures)}
  formatted_fxrs <- formatC(round(total_frax), format = 'd', big.mark=',')
  paste("The total number of fractures is estimated to ", 
                                            ifelse(total_frax > 0, "increase by ", "decrease by "), 
                                             formatted_fxrs, 
                                             " during the years 2018-", inp_year, sep = "", collapse = NULL)
                                            })

output$FraxBox_R <- renderInfoBox({
  base_case <- sim_data()
  total_frax <- 0
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018
  for(i in 1:duration) {
    total_frax <- total_frax + (base_case[[i]]$total_fractures_s1 - base_case[[i]]$total_fractures)
  }
  subtitle_text <- ifelse(total_frax > 0, "Change to New Scenario Results in Fracture Incidence Increasing", "Change to New Scenario Results in Fracture Incidence Decreasing")
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

output$CostBox_R <- renderInfoBox({
  base_case <- sim_data()
  total_frax_cost <- (0)
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018
  for(i in 1:duration) {
    total_frax_cost <- (total_frax_cost) + ((base_case[[i]]$grand_total_s1/1000000) - (base_case[[i]]$grand_total/1000000))
  }
  print(total_frax_cost)
  subtitle_text <- ifelse(total_frax_cost > 0, "Change to New Scenario Results in Cost Increases ($MM)", "Change to New Scenario Results in Cost Decreases ($MM)")
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
  base_case <- sim_data()
  total_frax <- 0
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018
  for(i in 1:duration) {
    total_frax <- total_frax + (base_case[[i]]$total_fractures_s1 - base_case[[i]]$total_fractures)
  }
  subtitle_text <- ifelse(total_frax > 0, "Change to New Scenario Results in Fracture Incidence Increasing", "Change to New Scenario Results in Fracture Incidence Decreasing")
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
  base_case <- sim_data()
  total_frax_cost <- (0)
  duration <-  as.integer(substring(input$endYear, 1, 4)) - 2018
  for(i in 1:duration) {
    total_frax_cost <- (total_frax_cost) + ((base_case[[i]]$grand_total_s1/1000000) - (base_case[[i]]$grand_total/1000000))
  }
  print(total_frax_cost)
  subtitle_text <- ifelse(total_frax_cost > 0, "Change to New Scenario Results in Cost Increases ($MM)", "Change to New Scenario Results in Cost Decreases ($MM)")
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
    #allcostdf$costsx <- ordered(allcostdf$costsx, c("One per Year", "> One per Year"))
    
    #custom_palette = brewer.pal(6,"Paired")#c('#46005D','#7A4791','#CFA8D5','#D58DC8','#B964AA')
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
  
  #observeEvent(input$add,{
  #  
  #  
  #  #output$coststbl <- renderTable({
  #  #  allcost.df[, c(input$inpt, input$outpt, input$LTC, input$ED, input$other, input$pharm), drop = FALSE]
  #  #}, rownames = TRUE)
  #  
  #  
  #})
  
  output$fxrplot <- renderPlotly({
    
    progress <- shiny::Progress$new()
    progress$set(message = "Simulating Population", value = 0)
    
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    
    sim <- sim_data()
    progress$set(value = progress$getValue() + (progress$getMax() - progress$getValue())/3, detail = "Preparing Plot")
  
    start_year <- 2018
    end_year   <- as.integer(substring(input$endYear, 1, 4))
    
    xbc <- c(start_year:end_year)
    ybc <- c()
    ys1 <- c()

    
    print(sim)
    for(i in 1:length(xbc)) {
      if(i > 1) {
        ybc <- cbind(ybc, sim[[i]]$total_fractures + ybc[i-1])
        ys1 <- cbind(ys1, sim[[i]]$total_fractures_s1 + ys1[i-1])  
      } else {
        ybc <- cbind(ybc, sim[[i]]$total_fractures)
        ys1 <- cbind(ys1, sim[[i]]$total_fractures_s1)
      }
    }

    
    
    dummybc <- data.frame(xbc, ybc, ys1)#, frames)

    
    progress$set(value = progress$getValue() + (progress$getMax() - progress$getValue())/3, detail = "Preparing Plot")
    color_pal <- brewer.pal(3, "Paired")
    p <- plot_ly(dummybc, x = ~xbc) %>% 
      add_trace(y = ~ybc, name = "Base Case", mode = 'lines', line = list(color = color_pal[1]) ) %>% 
      add_trace(y = ~ys1, name = "New Scenario", mode = 'lines', line = list(color = color_pal[2])) %>%
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
    
    progress$set(value = progress$getValue() + (progress$getMax() - progress$getValue())/3, detail = "Preparing Plot")
    return(p)
  })
  
  
  output$costplot <- renderPlotly({
    
    sim <- sim_data()
    
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
                   mode = "markers", marker = list(color = color_pal[1]) ) %>%
         add_trace(y = ~costys1, name = "New Scenario", type = 'scatter',
                   mode = "markers", marker = list(color = color_pal[2]) ) %>%
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
}