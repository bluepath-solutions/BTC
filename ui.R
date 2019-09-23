abbreviations <- data.frame(abbrev = c("BMD", "CPI", "DXA", "FRAX", "NHANES", "RANKL", "SERMs"),
                            full = c("bone mineral density", "Consumer Price Index", "dual-energy X-ray absorptiometry", "Fracture Risk Assessment Tool", "National Health and Nutrition Examination Survey",  
                                     "receptor activator of nuclear factor kappa-B ligand", "selective estrogen receptor modulators"))
references <- data.frame(refs = c("Lewiecki EM, Adler R, Curtis J, Gagel R, Saag K, Singer A, et al. Hip Fractures and Declining DXA Testing: At a Breaking Point? J Bone Miner Res. 2016 Sep;31(S1):S1–411.", "Freemantle N, Cooper C, Diez-Perez A, Gitlin M, Radcliffe H, Shepherd S, et al. Results of indirect and mixed treatment comparison of fracture efficacy for osteoporosis treatments: a meta-analysis. Osteoporos Int. 2013 Jan;24(1):209–1", "Centre for Metabolic Bone Diseases, University of Sheffield, UK. FRAX® Fracture Risk Assessment Tool [Internet]. [cited 2017 Nov 14]. Available from: https://www.sheffield.ac.uk/FRAX/", "Centers for Disease Control and Prevention. NHANES - National Health and Nutrition Examination Survey [Internet]. [cited 2017 May 3]. Available from: https://www.cdc.gov/nchs/nhanes/", "Weaver J, Sajjan S, Lewiecki EM, Harris ST, Marvos P. Prevalence and Cost of Subsequent Fractures Among U.S. Patients with an Incident Fracture. J Manag Care Spec Pharm. 2017 Apr;23(4):461–71.", "Colby, Sandra L., Ortman, Jennifer M. Projections of the Size and Composition of the U.S. Population: 2014 to 2060. US Census Bur Wash DC. 2014 Mar;Current Population Reports, P25-1143:13.  www.census.gov /population/projections/data/national/2014.html>.", "United States Census Bureau [Internet]. Available from: https://www.census.gov/", "Imaz I, Zegarra P, González-Enríquez J, Rubio B, Alcazar R, Amate JM. Poor bisphosphonate adherence for treatment of osteoporosis increases fracture risk: systematic review and meta-analysis. Osteoporos Int. 2010 Nov;21(11):1943–51.", "Durden E, Pinto L, Lopez-Gonzalez L, Juneau P, Barron R. Two-year persistence and compliance with osteoporosis therapies among postmenopausal women in a commercially insured population in the United States. Arch Osteoporos [Internet].2017 Dec [cited 2018 Jun 20];12(1). Available from: http://link.springer.com/10.1007/s11657-017-0316-5", "King AB, Saag KG, Burge RT, Pisu M, Goel N. Fracture Reduction Affects Medicare Economics (FRAME): Impact of increased osteoporosis diagnosis and treatment. Osteoporos Int. 2005 Dec;16(12):1545–57.", "Pike, CT, Birnbaum HG, Schiller M, Swallow E, Burge RT, Edgell ET. Prevalence and costs of osteoporotic patients with subsequent non-vertebral fractures in the US.  Osteoporos Int. 2011;22:2611–2621.", "United States Department of Labor, Bureau of Labor Statistics. Medicare Care CPI (Consumer Price Index) Data Tables. 2019. Available from: https://data.bls.gov/pdq/SurveyOutputServlet", "Pike C, Birnbaum HG, Schiller M, Sharma H, Burge R, Edgell ET. Direct and indirect costs of non-vertebral fracture patients with osteoporosis in the US. PharmacoEconomics. 2010;28(5):395–409.", "Vanness DJ, Tosteson ANA. Estimating the Opportunity Costs of Osteoporosis in the United States: Top Geriatr Rehabil. 2005 Jan;21(1):4–16.", "Leader Jr. D, Williams SA, Curtis JR, Gut R. Osteoporosis-Related Fracture Events in the U.S (M19). AMCP Nexus; 2017 Oct 16; Dallas, TX, USA. S78.", "IBM Micromedex RED BOOK 2019.", "Center for Medicare and Medicaid Services (CMS) Physician Fee Schedule Current Procedural Terminology (CPT) payment rates. Access on March 29, 2019. https://www.cms.gov/apps/physician-fee-schedule/license-agreement.aspx"
))
#########################################################################################################################################
fluidPage(
  #setting up tables for alignment
  tags$head(
    tags$style(
      HTML(
        "
        #inputs-table {
        border-collapse: collapse;
        }
        
        #inputs-table td {
        padding: 10px;
        vertical-align: bottom;
        }
        "
      ) #/ HTML
      ) #/ style
      ), #/ head
  useShinyjs(),
  useShinyalert(),
  useSweetAlert(),
  dashboardPagePlus(
    skin = "blue",
    header = dashboardHeaderPlus(
      titleWidth=270,
      #tags$li(class = "dropdown", actionBttn(inputId="Next", label=icon("arrow-right"),color="primary",style="minimal", size="xs")),
      enable_rightsidebar = FALSE,
      # rightSidebarIcon = "grip-lines-vertical",
      title = tagList(span(class = "logo-lg", "Bending the Curve"),
                      img(src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRb4Eku7HH9tk3KfqEtF5BXU5obNhUmWRT9rS_z8_U_U_0pWf-m")),
      left_menu = tagList(
        dropdownBlock(
          id = "defaultDD",
          title = "Restore Defaults",
          badgeStatus = NULL,
          div(style="display:inline-block", actionBttn(inputId="restoreall", label="Reset All Default Inputs",color="primary",style="minimal", size="xs"), style="float:right"),
          actionBttn(inputId="restorepop", label="Reset Population Inputs",color="primary",style="minimal", size="xs"),
          actionBttn(inputId="restorefxrcosts", label="Reset Fracture Cost Inputs",color="primary",style="minimal", size="xs"),
          actionBttn(inputId="restorescenarios", label="Reset Scenario Inputs",color="primary",style="minimal", size="xs")
        ))
    ),
    sidebar = 
      dashboardSidebar(
        width = 270,
        sidebarMenu(id = "tabs",
                    menuItem("Home", tabName = "Home", icon = icon("home")),
                    menuItem("Overview", icon = icon("sitemap"), tabName = "Overview"),
                    menuItem("Simulation Mechanics", icon = icon("code-branch"), tabName = "Mechanics"),
                    menuItem("Population Inputs", icon = icon("file-prescription"), tabName = "Pop_Inputs"),
                    menuItem("Treatment & Economic Inputs", icon = icon("file-invoice-dollar"), tabName = "ClinEcon_Inputs"),
                    menuItem("Results", icon = icon("th-list"), tabName = "Scenarios"),
                    menuItem("Conclusions", icon = icon("sign-out-alt"), tabName = "Results"),
                    menuItem("Assumptions & Limitations", icon = icon("exclamation-triangle"), tabName = "Assumptions"),
                    menuItem("_______________________________", tabName = "Break"),
                    menuItem("Disclosures & Study Descriptions", icon = icon("file-alt"), tabName = "Disclosures"),
                    #menuItem("Abbreviations & Terminology", icon = icon("book-open"), tabName = "Terms"),
                    menuItem("References", icon = icon("asterisk"), tabName = "References")
        )
      ),
    # rightsidebar = rightSidebar(
    #   background = "dark",
    #   rightSidebarTabContent(
    #     id = 1,
    #     icon = "sitemap",
    #     title = "Model Overview",
    #     active = TRUE,
    #     rightSidebarMenu(
    #       rightSidebarMenuItem(
    #         icon = menuIcon(
    #           name = "lightbulb",
    #           color = "red"
    #         ),
    #         info = menuInfo(
    #           title = "Model Information",
    #           description = tags$p("The model analyzes US women age 65 years and older. Each patient is randomly assigned a unique set of demographics and characteristics based on a probabilsitic distribution. The resulting average across the patients reflects the population evarage inputs customized on the left. 
    #                                Based on the profile of each patient, a 10-year fracture risk is applied and adjusted to reflect an annual risk. This risk is used to estimate the probability of fracture. 
    #                                Total fractures are aggreated by type and monetized using direct and indirect costs. For each calendar year, hypothetical cohorts of a specified number of women are simulated within each model scenario.
    #                                ",style = "font-size: 90%;")
    #           ))))
    #           ),
    
    body <- dashboardBody(
      
      hidden(actionBttn(inputId="Previous", label=icon("arrow-left"),color="primary",style="float", size="xs")),  
      hidden(actionBttn(inputId="Next", label=icon("arrow-right"),color="primary",style="float", size="xs")),  
      
      setShadow("box"),
      
      tabItems(
        tabItem(tabName = "Home",
                #h2("", align = "center"),
                br(),
                fluidRow(
                  widgetUserBox(
                    title = "",
                    subtitle = "",
                    type = NULL,
                    width = 12,
                    height = 350,
                    src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRb4Eku7HH9tk3KfqEtF5BXU5obNhUmWRT9rS_z8_U_U_0pWf-m",
                    color = "gray",
                    boxToolSize = "lg",
                    background = TRUE,
                    backgroundUrl = "https://dlg7f0e93aole.cloudfront.net/wp-content/uploads/Osteoporosis.jpg",
                    collapsible = FALSE,
                    closable = FALSE,
                    HTML(paste(br(),"BENDING THE CURVE<br/>
                               The Estimated Long-Term Value of Improving <br/>
                               Patient Identification and Treatment Rates in <br/>
                               Post-Menopausal Osteoporosis", br())), align = "center", style = "font-size: 190%;"
                    
                    #footer = "This model estimates the economic burden of osteoporosis in a representative population and is intended for formulary committees. The model includes estimated treatment costs by drug class and does not specify individual drug costs.", align = "center"
                    )),
                #fluidRow(h4("")),
                fluidRow(
                  actionBttn(
                    inputId = "Id_enter",
                    label = "ENTER", 
                    style = "stretch",
                    color = "primary",
                    icon = icon("external-link-square-alt")
                  ), align = "center"),
                headerPanel(""),
                fluidRow(boxPlus(
                  title = "Model Rationale", 
                  closable = FALSE, 
                  status = "info",
                  width = 12,
                  solidHeader = FALSE, 
                  collapsible = TRUE,
                  p(HTML("This model estimates the economic burden of osteoporosis in a representative population and is intended for audiences involved in population health decision making. The model includes estimated treatment costs by drug class and does not specify individual drug costs.<br/><br/>
                    <h4>Important Disclosure</h4>
                    <h4></h4>
                         The information contained in this resource is not intended to assess or compare efficacy or safety of products. The data and results associated with this model are based on estimates.")
                ))),
                headerPanel(""),
                #headerPanel(""),
                fluidRow(boxPlus(
                  title = "USA-785-80269",
                  closable = FALSE, 
                  status = "info",
                  width = 12,
                  height = 4,
                  solidHeader = FALSE, 
                  collapsible = FALSE
                ))
                  ),
        
        tabItem(tabName = "Overview",
                fluidRow(
                  h3(tags$div("Bending the Curve:",
                              tags$br(),
                              "The Estimated Long-Term Value of Improving Patient Identification and Treatment Rates in Post-Menopausal Osteoporosis"), align = "center")),
                fluidRow(
                  box(id = "Model_Ov_Tbl",
                      title = "Model Overview", width = 12,  status = "primary",
                      footer = fluidRow(
                        column(
                          width = 12,
                          descriptionBlock(    
                            tags$img(src="Model_Overview.PNG", width = "80%", height = "80%", style="display: block; margin-left: auto; margin-right: auto;")),
                          text = "Model Overview")
                      ),
                      bsPopover("Model_Ov_Tbl", title='<font size="2">Source:', content='<font size="3">DOF, Amgen, Bend the Curve PMO Microsimulation, 2019', placement="left", options = list(container = "body")))
                ),
                tags$img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Amgen.svg/1280px-Amgen.svg.png", width = "100px", height = "30px", style="display: block; margin-left: auto; margin-right: auto;")
        ),
        tabItem(tabName = "Mechanics",
                fluidRow(
                  h3(tags$div("Bending the Curve:",
                              tags$br(),
                              "The Estimated Long-Term Value of Improving Patient Identification and Treatment in Post-Menopausal Osteoporosis"), align = "center")),             # fluidRow(
                box(id = "mod_mech", title = "Model Overview", width = 12, status = "primary",
                    footer = fluidRow(
                      column(
                        width = 12,
                        descriptionBlock(
                          tags$img(src="Simulation_Mechanics.png", width = "95%", height = "90%", style="display: block; margin-left: auto; margin-right: auto;")),
                        text = "Model Flow")
                    )),
                bsPopover("mod_mech", title='<font size="2">Source:', content='<font size="3">DOF, Amgen, Bend the Curve PMO Microsimulation, 2019', placement="left", options = list(container = "body")),
                tags$img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Amgen.svg/1280px-Amgen.svg.png", width = "100px", height = "30px", style="display: block; margin-left: auto; margin-right: auto;")
        ),
        tabItem(tabName = "Pop_Inputs",
                fluidRow(
                  width = NULL, background = "black"
                ),
                h3(tags$div("Bending the Curve:",
                            tags$br(),
                            "The Estimated Long-Term Value of Improving Patient Identification and Treatment Rates in Post-Menopausal Osteoporosis"), align = "center"),
                fluidRow(
                  boxPlus(id = "Pop_Demo", title = "Population Size & Demographics", width = 12, closable = FALSE, collapsible = TRUE,
                          bsPopover("Pop_Demo", title='<font size="2">Source:', content='<font size="3">DOF, Amgen, Bend the Curve PMO Microsimulation, 2019', placement="left", options = list(container = "body")),
                          enable_dropdown = FALSE, dropdown_icon = "question-circle",
                          dropdown_menu = dropdownItemList(
                            dropdownDivider(),
                            dropdownItem(name = HTML("Using the Census projection estimates for females of any race or ethnicity, <br/>
                                                     the probability of each age starting at 65 years old was estimated using the <br/>
                                                     Census projections for each calendar year included in the model (2018-2040). <br/>
                                                     The distribution of race and ethnicity were estimated from Census data. <br/>
                                                     The estimates for race and ethnicity were generated to be mutually exclusive <br/>
                                                      and sum to 100%. Therefore, some races are not included in the model as the  <br/>
                                                      FRAX&reg; algorithm only contains risk tables for three races (Caucasian, Black, Asian) <br/>
                                                     and ethnicity (Hispanic)."))),
                          tags$table(id = "inputs-table"
                                     , style = "width: 100%"
                                     , tags$tr(
                                       tags$td(style = "width: 12.5%; text-align: left", p("Population, Women Aged 50 Years and Older")),
                                       tags$td(style = "width: 12.5%; text-align: left",
                                               tags$style("#pop_input {background-color:#dfdfdf;}"), numericInput(inputId = "pop_input", label = "", value = 10232900, min = 0, max = 30000000)),
                                       tags$td(style = "width: 25%; text-align: left",
                                               h5("")),
                                       tags$td(style = "width: 25%; text-align: left",
                                               tags$img(src="grey_boxes.PNG", width = "190px", height = "60px", style="display: block; margin-left: auto; margin-right: 0px;"))
                                     ))#,
                          # bsTooltip("pop_input", "Enter eligible population. Default value of 1 million is based on ....","right", options = list(container = "body"))),
                          
                          # tags$table(id = "inputs-table"
                          #            , style = "width: 100%"
                          #            , tags$tr(
                          #              tags$td(style = "width: 11.8%; text-align: left", p("Race/Ethnicity", br(), "Distribution (%)")),
                          #              tags$td(style = "width: 11.8%; text-align: left",
                          #                      tags$style("#RE_cauc {background-color:#dfdfdf;}"), numericInput(inputId = "RE_cauc", label = "Caucasian", value = 0, min = 0, max = 100, step = 0.1)),
                          #              tags$td(style = "width: 11.8%; text-align: left",
                          #                      tags$style("#RE_hisp {background-color:#dfdfdf;}"), numericInput(inputId = "RE_hisp", label = "Hispanic", value = 0, min = 0, max = 100, step = 0.1)),
                          #              tags$td(style = "width: 11.8%; text-align: left",
                          #                      tags$style("#RE_asian {background-color:#dfdfdf;}"), numericInput(inputId = "RE_asian", label = "Asian", value = 100, min = 0, max = 100, step = 0.1)),
                          #              tags$td(style = "width: 11.8%; text-align: left",
                          #                      tags$style("#RE_black {background-color:#dfdfdf;}"), numericInput(inputId = "RE_black", label = "Black", value = 0, min = 0, max = 100, step = 0.1)),
                          #              tags$td(style = "width: 11.8%; text-align: left",
                          #                      h5(textOutput("sum_RE"), align="center",style="margin-right: 0px; padding-bottom:15px"))
                          #            ))
                          ),
                  
                  boxPlus(id = "Risk_Fact", title = "Risk Factors", width = 12, closable = FALSE, collapsible = TRUE,
                          bsPopover("Risk_Fact", title='<font size="2">Source:', content='<font size="3">DOF, Amgen, Bend the Curve PMO Microsimulation, 2019', placement="left", options = list(container = "body")),
                          enable_dropdown = FALSE, dropdown_icon = "question-circle",
                          dropdown_menu = dropdownItemList(
                            dropdownDivider(),
                            dropdownItem(name = HTML("Risk factor prevalence was estimated from analyzing National Health and Nutrition Examination Survey (NHANES), <br/>
                                                     a nationally-representative population-based survey conducted every 2 years. An analysis of the 2013-2014 data <br/>
                                                     was conducted to estimate the population-level estimates of risk factors for the female population aged ≥ 65 years. <br/>
                                                     Estimates for the prevalence of the following widely accepted fracture risk factors: smoking; rheumatoid arthritis; <br/>
                                                     long-term glucocorticoid use; excessive alcohol use; parental history of a hip fracture; and previous fracture were <br/>
                                                     generated for women ≥ 65 years old. NHANES data were used to estimate mean BMD and the standard deviation."))),
                          tags$br(),
                          tags$table(id = "inputs-table"
                                     , style = "width: 100%"
                                     , tags$tr(
                                       tags$td(style = "width: 25%; text-align: left",
                                               tags$style("#BMD_mean {background-color:#dfdfdf}"), numericInput(inputId = "BMD_mean", label = "Mean Bone Mineral Density", value = 0.86, min = 0, max = 1, step = 0.01)),
                                       tags$td(style = "width: 25%; text-align: left", 
                                               tags$style("#BMD_SD {background-color:#dfdfdf}"), numericInput(inputId = "BMD_SD", label = "Bone Mineral Density Standard Deviation", value = 0.097, min = 0, max = 1, step = 0.01)),
                                       tags$td(style = "width: 25%; text-align: left",
                                               tags$style("#RA_inp {background-color:#dfdfdf}"), numericInput(inputId = "RA_inp", label = "Rheumatoid Arthritis (%)", value = 5.7, min = 0, max = 100, step = 0.01)),
                                       tags$td(style = "width: 25%; text-align: left",
                                               tags$style("#fxr_inp {background-color:#dfdfdf}"), numericInput(inputId = "fxr_inp", label = "Previous Fracture (%)", value = 14.3, min = 0, max = 100, step = 0.01)))),                      
                          tags$table(id = "inputs-table"
                                     , style = "width: 100%"
                                     , tags$tr(
                                       tags$td(style = "width: 25%; text-align: left",
                                               tags$style("#parfxr_inp {background-color:#dfdfdf;}"), numericInput(inputId = "parfxr_inp", label = "Parent History of Hip Fracture (%)", value = 16.6, min = 0, max = 100, step = 0.01)),
                                       tags$td(style = "width: 25%; text-align: left",
                                               tags$style("#smoker {background-color:#dfdfdf;}"), numericInput(inputId = "smoker", label = "Smoker (%)", value = 6.3, min = 0, max = 100, step = 0.01)),
                                       tags$td(style = "width: 25%; text-align: left",
                                               tags$style("#alco {background-color:#dfdfdf;}"), numericInput(inputId = "alco", label = "Excessive Alcohol Use (%)", value = 5.5, min = 0, max = 100, step = 0.01)),
                                       tags$td(style = "width: 25%; text-align: left",
                                               tags$style("#gluco_tx {background-color:#dfdfdf;}"), numericInput(inputId = "gluco_tx", label = "Long-Term Glucocorticoid Therapy (%)", value = 9.9, min = 0, max = 100, step = 0.01))))      
                            ),
                  
                  tags$img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Amgen.svg/1280px-Amgen.svg.png", width = "100px", height = "30px", style="display: block; margin-left: auto; margin-right: auto;")
                            )
                  ),
        tabItem(tabName = "ClinEcon_Inputs",
                fluidRow(
                  width = NULL, background = "black"),
                h3(tags$div("Bending the Curve:",
                            tags$br(),
                            "The Estimated Long-Term Value of Improving Patient Identification and Treatment Rates in Post-Menopausal Osteoporosis"), align = "center"),
                fluidRow(
                  boxPlus(id="FxrCostInp", title = "Fracture Costs", width = 12, closable = FALSE, collapsible = TRUE,
                          enable_dropdown = FALSE, dropdown_icon = "question-circle",
                          dropdown_menu = dropdownItemList(
                            dropdownItem(name = HTML("Direct medical costs related to fracture were derived from Weaver et al. This retrospective claims analysis included<br/>
                                                     45,603 patients (mean age of 78.1 years) with Humana Medicare Advantage who experienced an incident fracture between <br/>
                                                     January 1, 2008, and December 31, 2013. <br/>
                                                     <br/>
                                                     All-cause health care costs (i.e., not limited to osteoporosis-specific resource use) incurred in the year following<br/>
                                                     fracture were included. Outpatient services included claims for radiology, primary care, outpatient hospital visits, <br/>
                                                     orthopedic specialist visits, and rehabilitation services. Long-term care services were defined as at least 1 long <br/>
                                                     term care stay in a rehabilitation or skilled nursing facility. Pharmacy costs included all prescription drug usage, <br/>
                                                     which likely corresponds to duplicate capture of osteoporosis medication costs in the model. “Other” costs were not<br/> 
                                                     specified in the source but include remainder of services not included in the specified categories.
                                                     <br/>
                                                     The costs estimated from Weaver et al. and were adjusted from 2014 to 2018 based on the Bureau of Labor Statistics Medical <br/>
                                                     Care Consumer Price Index.")),
                            dropdownDivider(),
                            dropdownItem(name = HTML("The costs associated with Productivity Losses were based on Pike et al. 2010. This retrospective analysis evaluated <br/>
                                                     incremental lost wages due to medically related absenteeism among patients with a prior fracture compared to matched <br/>
                                                     controls (n = 4,764; based on privately insured patients within an employer claims data from 1999-2006). <br/>
                                                     <br/>
                                                     Informal caregiver costs were estimated based on Vanness et al. 2005. Based on the publication, an average of 3 hours per <br/>
                                                     patient per week incurred by caregivers was multiplied by an average 2002 wage of $9.16/hour to estimate caregiver costs.<br/>
                                                     <br/>
                                                     All direct costs must be included in the analyses, but indirect costs can be excluded. If the 'Include Indirect Costs' box <br/>
                                                     is unchecked, both components of indirect costs will be excluded. When costing fractures, the source costs differentiate <br/>
                                                     between those with a single versus multiple fractures in a calendar year. The estimates are applied across all fracture <br/>
                                                     types." ))
                            ),
                          bsPopover("FxrCostInp", title='<font size="2">Source:', content='<font size="3">DOF, Amgen, Bend the Curve PMO Microsimulation, 2019', placement="left", options = list(container = "body")),
                          fluidRow(
                            column(width = 4, tags$img(src="grey_boxes.PNG", width = "150px", height = "50px", style="display: block; margin-left: 0px; margin-right: auto;")),
                            column(width = 8, h5(strong("Costs ($)")), align = "center", style="padding-top:35px;")),
                          fluidRow(
                            column(width = 4, h5(strong("Direct Cost Inputs")), align = "left"),
                            column(width = 4, h5(strong("One Fracture per Year")), align = "center"),
                            column(width = 4, h5(strong("> One Fracture Per Year")), align = "center")),
                          fluidRow(
                            column(width = 4, h5("Inpatient Stay", align = "left")),
                            column(width = 4, tags$style("#costinpt1 {background-color:#dfdfdf;}"), numericInput(inputId = "costinpt1", label = NULL, value = "4698.97", step = 1)),
                            column(width = 4, tags$style("#costinpt2 {background-color:#dfdfdf;}"), numericInput(inputId = "costinpt2", label = NULL, value = "8084.90"))),
                          fluidRow(
                            column(width = 4, h5("Outpatient Visit", align = "left")),
                            column(width = 4, tags$style("#costoutpt1 {background-color:#dfdfdf;}"), numericInput(inputId = "costoutpt1", label = NULL, value = "1243.64")),
                            column(width = 4, tags$style("#costoutpt2 {background-color:#dfdfdf;}"), numericInput(inputId = "costoutpt2", label = NULL, value = "1904.44"))),
                          fluidRow(
                            column(width = 4, h5("Long-Term Care", align = "left")),
                            column(width = 4, tags$style("#costLTC1 {background-color:#dfdfdf;}"), numericInput(inputId = "costLTC1", label = NULL, value = "0")),
                            column(width = 4, tags$style("#costLTC2 {background-color:#dfdfdf;}"), numericInput(inputId = "costLTC2", label = NULL, value = "0"))),
                          fluidRow(
                            column(width = 4, h5("Emergency Department Visit", align = "left")),
                            column(width = 4, tags$style("#costED1 {background-color:#dfdfdf;}"), numericInput(inputId = "costED1", label = NULL, value = "0")),
                            column(width = 4, tags$style("#costED2 {background-color:#dfdfdf;}"), numericInput(inputId = "costED2", label = NULL, value = "0"))),
                          fluidRow(
                            column(width = 4, h5("Other", align = "left")),
                            column(width = 4, tags$style("#costother1 {background-color:#dfdfdf;}"), numericInput(inputId = "costother1", label = NULL, value = "0")),
                            column(width = 4, tags$style("#costother2 {background-color:#dfdfdf;}"), numericInput(inputId = "costother2", label = NULL, value = "0"))),
                          fluidRow(
                            column(width = 4, h5("Pharmacy", align = "left")),
                            column(width = 4, tags$style("#costpharm1 {background-color:#dfdfdf;}"), numericInput(inputId = "costpharm1", label = NULL, value = "1123.51")),
                            column(width = 4, tags$style("#costpharm2 {background-color:#dfdfdf;}"), numericInput(inputId = "costpharm2", label = NULL, value = "1285.73"))),
                          fluidRow(
                            box(id="IndirectCosts", "", color = "blue", width = 12, height = 4),
                            bsPopover("IndirectCosts", title='<font size="2">Source:', content='<font size="3">DOF, Amgen, Bend the Curve PMO Microsimulation, 2019', placement="left", options = list(container = "body")),
                            column(width = 4, prettyCheckbox(inputId = "IndirectCosts", strong("Include Indirect Costs"), value = TRUE, shape = "square"))),
                          fluidRow(
                            column(width = 4, h5("Productivity Losses", align = "left")),
                            column(width = 4, tags$style("#costprod1 {background-color:#dfdfdf;}"), numericInput(inputId = "costprod1", label = NULL, value = "240")),
                            column(width = 4, tags$style("#costprod2 {background-color:#dfdfdf;}"), numericInput(inputId = "costprod2", label = NULL, value = "240"))),
                          fluidRow(
                            column(width = 4, h5("Informal Caregiver", align = "left")),
                            column(width = 4, tags$style("#costcare1 {background-color:#dfdfdf;}"), numericInput(inputId = "costcare1", label = NULL, value = "426")),
                            column(width = 4, tags$style("#costcare2 {background-color:#dfdfdf;}"), numericInput(inputId = "costcare2", label = NULL, value = "426")))
                            )
                            ),
                fluidRow(
                  boxPlus(id = "clin_inp", title = "Treatment Inputs", width = 12, closable = FALSE, collapsible = TRUE, collapsed = TRUE,
                          enable_dropdown = FALSE, dropdown_icon = "question-circle",
                          dropdown_menu = dropdownItemList(
                            dropdownItem(name = HTML("A pre-set market basket of treatments is analyzed with the market mix, monthly cost, and efficacy shown here. The treatment mix comes from an Amgen internal market analysis as of January 2019.<br/>
                                                     <br/>
                                                     Monthly cost represent a weighted average using wholesale acquisition cost with available National Drug Code adjusted to reflect a 30-day supply. Note that WAC reflects the price offered to wholesalers and does not take into account discounts, <br/>
                                                     rebates, or other price concessions that may ultimately affect the net price. In addition, WAC is subject to change at any time. <br/>
                                                     <br/>
                                                     The source efficacy data for hip and other fractures came from a secondary analysis of the literature with the aim of estimating the relative efficacy of osteoporosis treatment among postmenopausal women (Freemantle et al 2013). The relative <br/>
                                                      fracture rate compared to placebo for each agent was classified into the therapeutic classes and then weighted according to internal market share data to calculate the efficacy rate for the therapeutic class by fracture type. For the other <br/>
                                                     fracture types, a simple average of all non-hip related efficacy was used to estimate an average for all other fracture types excluding hip. Other types of fractures included new vertebral, clinical vertebral,  non-vertebral, and wrist. Efficacy <br/>
                                                     rates may vary from rates included in the FDA approved product information. Hip rates were not included for SERMS as efficacy has not been established.")),
                                                     dropdownDivider(),
                            dropdownItem(name = HTML("Medication costs and efficacy are adjusted using evidence from Durden et al. 2017 on 12-month adherence. Assuming an overall adherence rate of 41.8% at 12 months, the weighted monthly treatment cost is adjusted assuming that only half of the year is <br/>
                                                     impacted by non-adherence. For example, if the weighted treatment costs monthly is $5,000, the adherence estimate of 41% is applied to 6 of the 12 months (6*$5000+(6*0.41*$5,000)) = Total annual weighted cost. Efficacy rates by therapeutic class are further <br/>
                                                     adjusted for non-adherence. Assuming 58.2% non-adherence, the risk of fracture was increased by 46% based on evidence by Imaz et al. 2010. For example, if the weighted treatment efficacy for a therapeutic class is 0.50, then 41.8% is applied to the 0.5 and <br/>
                                                     58.2% is applied to the 0.5 adjusted for reduced efficacy (0.5*1.46). The resulting equation is (0.5 * 0.418 + 0.5 *1.46 * 0.582) = Total adherence adjusted efficacy."))
                            
                            ),
                          bsPopover("clin_inp", title='<font size="2">Source:', content='<font size="3">DOF, Amgen, Bend the Curve PMO Microsimulation, 2019', placement="left", options = list(container = "body")),
                          
                          # fluidRow(
                          #   column(width = 6, h5(" ")),
                          #   column(width = 2, h5(" ")),
                          #   column(width = 2, h5(strong("Efficacy")), align = "center"),
                          #   column(width = 2, h5(strong("Adherence-Adjusted Efficacy")), align = "center")),
                          fluidRow(
                            column(width = 4, h5(strong("Medication")), align = "left"),
                            column(width = 2, h5(strong("Treatment Mix (%)")), align = "center"), 
                            column(width = 2, h5(strong("Monthly Cost ($)")), align = "center"),
                            # column(width = 1, h5(strong("Hip")), align = "center"),
                            column(width = 2, h5(strong("Efficacy")), align = "center"),
                            # column(width = 1, h5(strong("Hip")), align = "center"),
                            column(width = 2, h5(strong("Adherence-Adjusted Efficacy")), align = "center")),
                          
                          fluidRow(
                            column(width = 4, h5("Anti-Resorptive Agents")),
                            column(width = 2, box(id="AR_MS", tags$p("82.8%", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4)),
                            # bsTooltip("AR_MS", "Market share inputs are based on utilization trends as of 2018, DOF.",
                            #           "right", options = list(container = "body")),
                            column(width = 2, box(id="AR_cost", tags$p("$46.99", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4)),
                            # bsTooltip("AR_cost", "Monthly costs are based on a weighted averge of costs - weighted by market share - from Medicare Part D files as of 2018.",
                            #           "right", options = list(container = "body")),
                            # column(width = 1, box(id="AR_eff", tags$p("0.63", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4)),
                            # bsTooltip("AR_eff", "Class efficacy is based on a weighted average, weighted by estimated market share, with individual therapy efficacies based on Freemantle et al. 2013.",
                            #           "left", options = list(container = "body")),
                            column(width = 2, box(id="AR_effoth", tags$p("0.66", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4)),
                            # bsTooltip("AR_effoth", "Other fractures include vertebral, non-vertebral, and wrist (Freemantle et al. 2013).",
                            #         "left", options = list(container = "body")),
                            # column(width = 1, box(id="AR_eff_adh", tags$p("0.80", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4)),
                            # bsTooltip("AR_effoth", "Other fractures include vertebral, non-vertebral, and wrist (Freemantle et al. 2013).",
                            #       "left", options = list(container = "body")),
                            column(width = 2, box(id="AR_effoth_adh", tags$p("0.83", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4))),
                          # bsTooltip("AR_effoth", "Other fractures include vertebral, non-vertebral, and wrist (Freemantle et al. 2013).",
                          #           "left", options = list(container = "body")),
                          fluidRow(
                            column(width = 4, h5("Selective Estrogen Receptor Modulators (SERMs)")),
                            column(width = 2, box(id="SERM_MS", tags$p("13.10%", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4)),
                            # bsTooltip("SERM_MS", "Market share inputs are based on utilization trends as of 2018, DOF.",
                            #           "right", options = list(container = "body")),
                            column(width = 2, box(id="SERM_cost", tags$p("$0.70", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4)),
                            # bsTooltip("SERM_cost", "Monthly costs are based on a weighted averge of costs - weighted by market share - from Medicare Part D files as of 2018.",
                            #           "right", options = list(container = "body")),
                            # column(width = 1, box(id="SERM_eff", tags$p("--", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4)),
                            #bsTooltip("SERM_eff", "Research on efficacy of SERMs in reducing risk of hip fracture is limited.",
                            #          "left", options = list(container = "body")),
                            column(width = 2, box(id="SERM_effoth", tags$p("0.59", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4)),
                            # bsTooltip("SERM_effoth", "Other fractures include vertebral, non-vertebral, and wrist (Freemantle et al. 2013).",
                            #           "left", options = list(container = "body")),
                            # column(width = 1, box(id="SERM_eff_adh", tags$p("--", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4)),
                            # bsTooltip("SERM_effoth", "Other fractures include vertebral, non-vertebral, and wrist (Freemantle et al. 2013).",
                            #           "left", options = list(container = "body")),
                            column(width = 2, box(id="SERM_effoth_adh", tags$p("0.75", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4))),
                          # bsTooltip("SERM_effoth", "Other fractures include vertebral, non-vertebral, and wrist (Freemantle et al. 2013).",
                          #           "left", options = list(container = "body")),
                          fluidRow(
                            column(width = 4, h5("Anabolics")),
                            column(width = 2, box(id="PTH_MS", tags$p("0.9%", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4)),
                            # bsTooltip("PTH_MS", "Market share inputs are based on utilization trends as of 2018, DOF.",
                            #           "right", options = list(container = "body")),
                            column(width = 2, box(id="PTH_cost", tags$p("$290.78", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4)),
                            # bsTooltip("PTH_cost", "Monthly costs are based on a weighted averge of costs - weighted by market share - from Medicare Part D files as of 2018",
                            #           "right", options = list(container = "body")),
                            # column(width = 1, box(id="PTH_eff", tags$p("0.25", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4)),
                            # bsTooltip("PTH_eff", "Class efficacy is based on a weighted average, weighted by estimated market share, with individual therapy efficacies based on Freemantle et al. 2013.",
                            #           "left", options = list(container = "body")),
                            column(width = 2, box(id="PTH_effoth", tags$p("0.25", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4)),
                            # bsTooltip("PTH_effoth", "Other fractures include vertebral, non-vertebral, and wrist (Freemantle et al. 2013).",
                            #           "left", options = list(container = "body")),
                            # column(width = 1, box(id="PTH_eff_adh", tags$p("0.32", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4)),
                            # bsTooltip("PTH_effoth", "Other fractures include vertebral, non-vertebral, and wrist (Freemantle et al. 2013).",
                            #         "left", options = list(container = "body")),
                            column(width = 2, box(id="PTH_effoth_adh", tags$p("0.32", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4))
                            # bsTooltip("PTH_effoth", "Other fractures include vertebral, non-vertebral, and wrist (Freemantle et al. 2013).",
                            #     "left", options = list(container = "body"))
                          ))
                            ),
                # fluidRow(boxPlus(width=12,
                #                  plotlyOutput("costp"),
                #                  #tableOutput("coststbl"),
                #                  fill=FALSE)),
                
                tags$img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Amgen.svg/1280px-Amgen.svg.png", width = "100px", height = "30px", style="display: block; margin-left: auto; margin-right: auto;")
                            ),
        tabItem(tabName = "Scenarios",
                fluidRow(
                  width = NULL, background = "black"),
                h3(tags$div("Bending the Curve:",
                            tags$br(),
                            "The Estimated Long-Term Value of Improving Patient Identification and Treatment Rates in Post-Menopausal Osteoporosis"), align = "center"),
                fluidRow(
                  boxPlus(id = "scenarios_box", title = "Treatment, Identification Rates & Time Horizon", width = 12, closable = FALSE, collapsible = TRUE,
                          enable_dropdown = FALSE, dropdown_icon = "question-circle",
                          dropdown_menu = dropdownItemList(
                            # TODO: UPDATE THIS INFO
                            dropdownItem(name = HTML("Identification Rate in the Base Case scenario was based on Lewiecki et al. 2016. This analysis identified the proportion of Medicare patients with <br/>
                                                     ≥ 1 DXA scan each year from 2002-2014 (11.3% in 2014), based on health care claims and enrollment data from the 5% sample of Medicare fee-for-service  <br/>
                                                     beneficiaries. Current treatment rates and trends were based on unpublished market share data, which indicated 9% of those ages 65+ years were treated  <br/>
                                                     (i.e., filled at least one osteoporosis medication) in 2017, with the rate steadily declining since 2011. Thus, it was assumed that the overall treatment  <br/>
                                                     rate was 9% in 2018 and declined year over year through 2040.  In the Improved PMO Management scenario, the selected treatment rate remains constant  <br/>
                                                     over the specified time period. ")),
                            dropdownDivider(),
                            dropdownItem(name = HTML("The microsimulation is estimating the effects of identification and treatment across a cohort of the size entered in the <br/>
                                                     inputs sheet. Please allow time for the simulation to run."))),
                          bsPopover("scenarios_box", title='<font size="2">Source:', content='<font size="3">DOF, Amgen, Bend the Curve PMO Microsimulation, 2019', placement="left", options = list(container = "body")),
                          
                          fluidRow(
                            column(width = 4, tags$img(src="grey_boxes.PNG", width = "150px", height = "50px", style="display: block; margin-left: 0px; margin-right: auto; ")),
                            column(width = 4, h5(strong("Identification Rate (%)")), align = "center", style="padding-top:25px;"),
                            column(width = 4, h5(strong("Treatment Rate (%)")), align = "center", style="padding-top:25px;")),
                          fluidRow(
                            column(width = 4, align="left", blockQuote("    Base Case")),
                            column(width = 4, tags$style("#basecaseID {background-color:#dfdfdf;}"), numericInput(inputId = "basecaseID", label = NULL, value = "23.16", step = 0.1)),
                            column(width = 4, tags$style("#basecaseTx {background-color:#dfdfdf;}"), numericInput(inputId = "basecaseTx", label = NULL, value = "14.4", step = 0.1))),
                          fluidRow(
                            column(width = 4, align="left", blockQuote("    Improved PMO Management")),
                            column(width = 4, tags$style("#scenario1ID {background-color:#dfdfdf;}"), numericInput(inputId = "scenario1ID", label = NULL, value = "38.16", step = 0.1)),
                            bsTooltip("scenario1ID", "The model only allows scenarios to be evlauted that increase rates of identification and treatment. Please enter a value above the value entered for base case.", "left", options = list(container = "body")),
                            column(width = 4, tags$style("#scenario1Tx {background-color:#dfdfdf;}"), numericInput(inputId = "scenario1Tx", label = NULL, value = "21.0", step = 0.1)),
                            bsTooltip("scenario1Tx", "The model only allows scenarios to be evlauted that increase rates of identification and treatment. Please enter a value above the value entered for base case.", "left", options = list(container = "body"))),
                          fluidRow(
                            column(width = 4, align="left", blockQuote("    Time Horizon")),
                            column(width = 6,
                                   sliderInput(inputId = "endYear", 
                                               label = NULL,
                                               min = as.Date("2022", '%Y'),
                                               max = as.Date("2040", '%Y'),
                                               value = as.Date("2040", '%Y'),
                                               step = 365,
                                               timeFormat = "%Y"), align = "center"),
                            column(width = 2,
                                   actionBttn(inputId = 'run_simulation',
                                              label = 'Run Simulation',
                                              color="success",
                                              style="material-flat",
                                              size = "sm"),
                                   align = 'center')
                          ))),
                
                boxPlus(width = 12, closable = FALSE, collapsible = TRUE,
                        # fluidRow(
                        #   column(width = 6, infoBoxOutput("FraxBox_R"), tags$style("#FraxBox_R {width:100%}")),
                        #   column(width = 6, infoBoxOutput("CostBox_R"), tags$style("#CostBox_R {width:100%}") )),
                        # fluidRow(
                        #   column(width = 6, infoBoxOutput("nNoPriorsBox"), tags$style("#nNoPriorsBox {width:100%}")),
                        #   column(width = 6, infoBoxOutput("nPriorsBox"), tags$style("#nPriorsBox {width:100%}") )),
                        fluidRow(
                          column(width = 6, infoBoxOutput("primaryFracBox"), tags$style("#primaryFracBox {width:100%}")),
                          column(width = 6, infoBoxOutput("prevFracBox"), tags$style("#prevFracBox {width:100%}") )),
                        fluidRow(
                          column(width = 6, infoBoxOutput("primaryFracCostBox"), tags$style("#primaryFracCostBox {width:100%}")),
                          column(width = 6, infoBoxOutput("prevFracCostBox"), tags$style("#prevFracCostBox {width:100%}") ))
                        ),
                # fluidRow(
                #   boxPlus(withSpinner(plotlyOutput("fxrplot")), width = 6),
                #   boxPlus(withSpinner(plotlyOutput("costplot")), width = 6)),
                
                fluidRow(
                  boxPlus(withSpinner(plotlyOutput("primaryFracPlot")), width = 6),
                  boxPlus(withSpinner(plotlyOutput("prevFracPlot")), width = 6)),
                
                fluidRow(
                  boxPlus(withSpinner(plotlyOutput("primaryFracCost")), width = 6),
                  boxPlus(withSpinner(plotlyOutput("prevFracCost")), width = 6)),
                
                # uncomment this chunk to see number of population with and without fracs
                boxPlus(width = 12, closable = FALSE, collapsible = TRUE, collapsed = TRUE,
                        fluidRow(
                          column(width = 6, infoBoxOutput("nNoPriorsBox"), tags$style("#nNoPriorsBox {width:100%}")),
                          column(width = 6, infoBoxOutput("nPriorsBox"), tags$style("#nPriorsBox {width:100%}") ))
                ),

                tags$img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Amgen.svg/1280px-Amgen.svg.png", width = "100px", height = "30px", style="display: block; margin-left: auto; margin-right: auto;")
                ),
        tabItem(tabName = "Results",
                fluidRow(
                  width = NULL, background = "black"),
                h3("Bending the Curve: The Estimated Long-Term Value of Improving Patient Identification and Treatment in Post-Menopausal Osteoporosis", align = "center"),
                # fluidRow(
                #   boxPlus(title = "Total Estimated Fractures", 
                #           closable = FALSE, status = "info", solidHeader = FALSE, collapsible = TRUE, width = 7,
                #           p(textOutput('totalfxr_content'))
                #   ),
                #   column(width=5,infoBoxOutput("FraxBox"), tags$style("#FraxBox {width:100%; word-break: keep-all; overflow-wrap: anywhere;}"))
                #          ), 
                # fluidRow(
                #   boxPlus(title = "Total Estimated Costs", 
                #           closable = FALSE, status = "info", solidHeader = FALSE, collapsible = TRUE, width = 7,
                #           p(textOutput('totalcost_content'))
                #   ),
                #   column(width=5,infoBoxOutput("CostBox"), tags$style("#CostBox {width:100%; word-break: keep-all; overflow-wrap: anywhere;}"))
                #         ),
                fluidRow(
                  boxPlus(title = "Primary Estimated Fractures", 
                          closable = FALSE, status = "info", solidHeader = FALSE, collapsible = TRUE, width = 7,
                          p(textOutput('primaryfxr_content'))
                  ),
                  column(width=5,infoBoxOutput("PrimaryFraxBox"), tags$style("#PrimaryFraxBox {width:100%; word-break: keep-all; overflow-wrap: anywhere;}"))
                ), 
                fluidRow(
                  boxPlus(title = "Primary Estimated Costs", 
                          closable = FALSE, status = "info", solidHeader = FALSE, collapsible = TRUE, width = 7,
                          p(textOutput('primarycost_content'))
                  ),
                  column(width=5,infoBoxOutput("PrimaryCostBox"), tags$style("#PrimaryCostBox {width:100%; word-break: keep-all; overflow-wrap: anywhere;}"))
                ),
                fluidRow(
                  boxPlus(title = "Secondary Estimated Fractures", 
                          closable = FALSE, status = "info", solidHeader = FALSE, collapsible = TRUE, width = 7,
                          p(textOutput('secondaryfxr_content'))
                  ),
                  column(width=5,infoBoxOutput("SecondaryFraxBox"), tags$style("#SecondaryFraxBox {width:100%; word-break: keep-all; overflow-wrap: anywhere;}"))
                ), 
                fluidRow(
                  boxPlus(title = "Secondary Estimated Costs", 
                          closable = FALSE, status = "info", solidHeader = FALSE, collapsible = TRUE, width = 7,
                          p(textOutput('secondarycost_content'))
                  ),
                  column(width=5,infoBoxOutput("SecondaryCostBox"), tags$style("#SecondaryCostBox {width:100%; word-break: keep-all; overflow-wrap: anywhere;}"))
                ),
                
                # fluidRow(
                #   boxPlus(title = "Base Case Fracture Reccurence", 
                #           closable = FALSE, status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 8,
                #           p(textOutput('reocc_text_1'))
                #   ),
                #   infoBoxOutput("FractureReoccurence")),
                
                
                # fluidRow(
                #   boxPlus(title = "Base Case Fracture Risk Ratio", 
                #           closable = FALSE, status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 8,
                #           p(textOutput('fracture_risk_text'))
                #   ),
                #   infoBoxOutput("FractureRatio")),
                # fluidRow(
                #   boxPlus(title = "New Scenario Fracture Risk Ratio", 
                #           closable = FALSE, status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 8,
                #           p(textOutput('fracture_risk_text_s1'))
                #   ),
                #   infoBoxOutput("FractureRatioS1")),

                fluidRow(
                  boxPlus(title = "Thank you for visiting.", 
                          closable = FALSE, status = "info", solidHeader = FALSE, collapsible = TRUE, width = 12,
                          p("Thank you for visiting the Bending the Curve Microsimulation Site.")
                  )),
                
                tags$img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Amgen.svg/1280px-Amgen.svg.png", width = "100px", height = "30px", style="display: block; margin-left: auto; margin-right: auto;")
        ),
        
        tabItem(tabName = "Assumptions",
                fluidRow(
                  width = NULL, background = "black"),
                h3("Bending the Curve: The Estimated Long-Term Value of Improving Patient Identification and Treatment in Post-Menopausal Osteoporosis", align = "center"),
                fluidRow(
                  boxPlus(
                    width = 12,
                    title = "Assumptions", 
                    closable = FALSE, 
                    status = "warning", 
                    solidHeader = TRUE, 
                    collapsible = TRUE,
                    p(HTML("<ul>
                            <li>When determining whether an individual was designated DXA and treatment, it assumed that those of highest risk as defined by FRAX would be prioritized. While in practice it is likely the case that some individuals treated are at lower risk than other individuals who are untreated, the assumption of treating those at highest risk  is aligned with clinical practice objectives to reduce fracture risk, versus assuming treatment is randomly distributed.</li>
                           <li>It was assumed that treated patients would receive a “market basket” of branded and generic agents based on market share data as of January 2019. The market basket is used to weight treatment efficacy and medication costs. The model reflects approved therapies generic/branded as of the end of 2018. Therefore, this model may not reflect generic or brand therapies that have been recently approved.</li>
                           <li>It was assumed that DXA scanning rates would remain constant over each year of model while treatment rates declined linearly year over year. It was assumed costs would remain constant in the future and this conservative assumption may underestimate the total fracture costs in later years. </li>
                           <li>It was assumed in estimating the treatment effectiveness that all fractures could be reduced at rates shown in meta-analyses by Freemantle et al. 2013; however, treatment might not reduce fractures to all potential sites at the same rate as measured in clinical trials.</li>
                            </ul>
                           "))
                    )),
                fluidRow(  
                  boxPlus(
                    width = 12,
                    title = "Limitations", 
                    closable = FALSE, 
                    status = "warning", 
                    solidHeader = TRUE, 
                    collapsible = TRUE,
                    p(HTML(" <ul>
                            <li>Treatments included in the analysis are based on those approved at the time of market share analysis, and do not reflect therapies that became commercially available after January 1, 2019. If new treatment alternatives are introduced that are cost effective, the fracture reduction and cost savings may be an underestimate of the true benefit as later years are estimated.</li>
                           <li>Treatment efficacy on fracture risk reduction for non-hip related outcomes was based on a non-weighted average of the point estimates of the non-hip sites from Freemantle et al. 2013. This rate was applied to all other sites (including toes and body parts that might not have the same fracture risk reduction from therapy).</li>
                           <li>An increase in fracture findings was only incorporated by increasing the rate of DXA, but other methods have been shown to increase identification. To the extent fracture findings can occur without the need for DXA, the cost savings could be an underestimated.</li>
                           <li>The risk of fractures was estimated using the simplified charts from FRAX, and although they have been well validated, they are not perfect predictors.</li>
                            </ul>
                           "))
                    )),
                
                tags$img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Amgen.svg/1280px-Amgen.svg.png", width = "100px", height = "30px", style="display: block; margin-left: auto; margin-right: auto;")
                    ),
        
        tabItem(tabName = "Disclosures",
                fluidRow(
                  width = NULL, background = "black"),
                h3("Bending the Curve: The Estimated Long-Term Value of Improving Patient Identification and Treatment in Post-Menopausal Osteoporosis", align = "center"),
                fluidRow(
                  boxPlus(
                    id = "financial_disc",
                    width = 12,
                    title = "Financial Disclosures", 
                    closable = FALSE, 
                    status = "warning", 
                    solidHeader = TRUE, 
                    collapsible = TRUE,
                    p(HTML("The following studies used in the economic model involved an Amgen employee or were sponsored by Amgen. These disclosures are specific to what is listed within the publication or source document used to development the economic model.<br/>
                           <ul>
                            <li>Data on file, Amgen; [US Bend the Curve PMO Microsimulation; 2019] <br/>
                             <ul>
                            <li>This analysis was funded by Amgen, Inc. <br/>
                           </ul>
                             </ul> 
                           <ul>
                            <li>Freemantle N, Cooper C, Diez-Perez A, Gitlin M, Radcliffe H, Shepherd S, et al. Results of indirect and mixed treatment comparison of fracture efficacy for osteoporosis treatments: a meta-analysis. Osteoporos Int. 2013;24(1):209–17. <br/>
                           <ul>
                            <li>This study was funded by Amgen Inc.</li>
                           <li>Freemantle N has received research grants from Amgen, Inc. and has served as a consultant for Amgen, Inc. Cooper C has received consulting and lecture fees from Amgen, Inc.,  Diez-Perez A has received honoraria or consulted for Amgen, Inc., and received research grants from Amgen, Inc.  Roux C has received research grants, and/or honoraria from Amgen</li>
                           <li>Gitlin M, Radcliffe H, Shepherd S were employees and shareholders in Amgen, Inc. at the time of the study.</li>
                           </ul>
                             </ul> 
                           <ul>
                            <li>Durden E, Pinto L, Lopez-Gonzalez L, Juneau P, Barron R. Two-year persistence and compliance with osteoporosis therapies among postmenopausal women in a commercially insured population in the United States. Arch Osteoporos. 2017; 12:22.</li>
                           <ul>
                            <li>This study was funded by Amgen Inc.</li>
                            <li>Durden E, Lopez-Gonzalez L, Juneau P are employees of Truven Health Analytics, who were paid by Amgen Inc. for conducting this study.</li>
                            <li>Pinto L and Barron R were employees and shareholders in Amgen, Inc. at the time of the study. Jessica Ma, PhD (Amgen Inc.) provided medical writing support.</li>
                           "))
                    )),
                tags$style(HTML("
                                     #financial_disc {
                                     padding-left: 1%;
                                     font-size:   12pt;
                                     line-height: 12pt;
          }
  ")),
                fluidRow(
                  boxPlus(
                    id = "study_desc",
                    width = 12,
                    title = "Study Descriptions", 
                    closable = FALSE, 
                    status = "warning", 
                    solidHeader = TRUE, 
                    collapsible = TRUE,
                    p(HTML("The following studies were used in the economic model as model inputs or as source evidence used to inform model inputs. Other evidence was used to support model assumptions and such evidence are included as references but have not been summarized below.
                           <ul>
                            <li>Data on file, Amgen; [US Bend the Curve PMO Microsimulation; 2019] </li>
                           <ul> 
                            <li>This analysis uses an economic simulation model developed to assess the clinical and economic burden of osteoporotic fractures in the US with and without policy changes. The model takes a population perspective focused on women ≥ 65 years old. Multiple sources were utilized to develop the simulation and analysis including epidemiology, fracture risk factors, rate of fracture based on risk factors, DXA scanning and treatment rates, as well as direct and indirect fracture costs.</li>
                            </ul>
                             </ul>  
                           <ul>
                            <li>Centre for Metabolic Bone Diseases, University of Sheffield, UK. FRAX® Fracture Risk Assessment Tool. Available from: https://www.sheffield.ac.uk/FRAX/  </li>
                           <ul>
                            <li>The University of Sheffield launched the FRAX® tool in 2008. The FRAX® algorithms give the 10-year probability of fracture. The output is a 10-year probability of hip fracture and the 10-year probability of a major osteoporotic fracture (clinical spine, forearm, hip or shoulder fracture).The FRAX® model was developed from studying population-based cohorts from Europe, North America, Asia and Australia. The simplified tables that are publicly accessible are structured by geography and race. The US specific tables include Caucasian, Asian, black and Hispanic. All data tables are structured by fracture output and provided in 5-year age increments.</li>
                           </ul>
                              </ul>
                           <ul>
                            <li>Centers for Disease Control and Prevention. NHANES - National Health and Nutrition Examination Survey. Available from: https://www.cdc.gov/nchs/nhanes/ </li>
                           <ul>
                            <li>The National Health and Nutrition Examination Survey (NHANES) is a program designed to assess the health and nutritional status of adults and children in the United States. The NHANES data is based on interviews and physical examinations and includes demographic, socioeconomic, dietary, and health-related questions. The examination component consists of medical, dental, and physiological measurements, as well as laboratory tests administered by highly trained medical personnel. NHANES data were structured into datasets of 2 year with the 2013-2014 providing the most recent dataset available with all needed variables. In 2013-2014, 14,332 persons were selected from 30 different survey locations and 10,175 completed the interview and 9,813 were examined. </li>
                            </ul>
                              </ul>
                           <ul>
                            <li>Weaver J, Sajjan S, Lewiecki EM, Harris ST, Marvos P. Prevalence and Cost of Subsequent Fractures Among U.S. Patients with an Incident Fracture. J Manag Care Spec Pharm. 2017;23(4):461–71.</li>
                           <ul>
                            <li>A retrospective analysis of two large US administrative claims databases was conducted between 2007 and 2014. The analysis is based on the Humana Medicare Advantage and the Optum commercially insured populations. Medicare patients were included if they were ≥ 65 years old and commercial patients were included if they were ≥ 50 years old. Patients were included if they experienced a fracture with at least 1 year of insurance eligibility before and after the fracture date. Patients were stratified into two cohorts, including those who incurred 1 fracture and those who experience > 1 fracture (after a 3 month gap from index fracture) during the 12-month follow-up post the initial index fracture. Analysis of fracture costs was specific to direct medical (inpatient, outpatient, emergency department, long-term care, other services) and pharmacy costs. A total of 45,603 patients were included in the Medicare group. All costs were presented after 1:1 patient-level propensity score match of the two cohorts (n= 4,549 per cohort) based on facture site, year of fracture, gender. Region, employment status, race and Charlson comorbidity index.  All costs were reported in 2014 US dollars and all costs are reported as incremental costs. </li>
                             </ul>
                                </ul>
                           <ul>
                            <li>Colby, SL., Ortman, JM. Projections of the Size and Composition of the U.S. Population: 2014 to 2060. US Census Bur Wash DC. 2014 Mar; Current Population Reports, P25-1143:13. </li>
                           <ul>
                            <li>The census projections are for the US including all 50 states and the District of Columbia.  The projection estimates were produced using cohort-component methods ((births, deaths, and net international migration) based on the 2010 Census. These methods are based on methods similar to time series analysis.  Historical trends in mortality, fertility rates and net international migration were to is advance the population projection each year by using.  The 2014 National Projections, including summary tables, downloadable files, and methodology and assumptions, can be found at <www.census.gov /population/projections/data/national/2014.html>. </li>
                             </ul>
                              </ul>
                           <ul>
                            <li>Pike, CT, Birnbaum HG, Schiller M, Swallow E, Burge RT, Edgell ET. Prevalence and costs of osteoporotic patients with subsequent non-vertebral fractures in the US.  Osteoporos Int. 2011;22:2611–2621. </li>
                           <ul>
                            <li>A retrospective analysis of administrative claims from 1999 through 2006 of  privately-insured (ages 18–64 years) and Medicare (ages 65+ years) patients with ≥1 subsequent osteoporosis-related non-vertebral fracture within a year the initial non-vertebral fractures were included. Patients were matched to controls who experiences an initial non-vertebral fracture but no subsequent fractures. Subsequent fractures were included if occurring >3 months after the incident fracture or >6 months were required for fractures occurring at the site as the initial fracture. The analysis focused on the prevalence of subsequent fractures and the costs between the two cohorts. A total 48,742 Medicare patients were included in the analysis with at least one non-vertebral fracture.</li>
                           </ul>
                              </ul>
                           <ul>
                            <li>Freemantle N, Cooper C, Diez-Perez A, Gitlin M, Radcliffe H, Shepherd S, et al. Results of indirect and mixed treatment comparison of fracture efficacy for osteoporosis treatments: a meta-analysis. Osteoporos Int. 2013;24(1):209–17.</li>
                           <ul>
                            <li>A secondary analysis of the literature with the aim of estimating the relative efficacy of osteoporosis treatment among postmenopausal women. The analysis used multiple techniques including meta-analysis, adjusted indirect comparison, and mixed treatment comparison [MTC]) to provide comparisons of the relative efficacy of postmenopausal osteoporosis therapies in the absence of comprehensive head-to-head trials. Evidence was identified from MEDLINE; EMBASE; Cochrane Central Register of Controlled Trials (CENTRAL) via Wiley Interscience; and Cumulative Index to Nursing and Allied Health Literature (CINAHL) between April 28, 2009 and November 4, 2009. The analyses included data from trials with fracture endpoints, including morphometric vertebral, clinical vertebral, non-vertebral, hip and wrist. A total of 33 studies were included in the final analysis and included postmenopausal osteoporosis treatments used in model currently approved in US as of November 4, 2009. </li>
                            </ul>
                              </ul>
                           <ul>
                            <li>United States Department of Labor, Bureau of Labor Statistics. Medicare Care CPI (Consumer Price Index) Data Tables. 2019. Available from: https://data.bls.gov/pdq/SurveyOutputServlet</li>
                           <ul>
                            <li>A public database of CPI over time. The database provides national averages for medical care in the US for all urban consumers. These estimates are not seasonally adjusted. The medical care index is one of the eight major groups in the Consumer Price Index (CPI) and is divided into two main components: medical care services and medical care commodities, each containing several item categories. Medical care services, the larger component in terms of weight in the CPI, is organized into three categories: professional services, hospital and related services, and health insurance. Medical care commodities, the other major component, includes medicinal drugs and medical equipment and supplies.</li>
                            </ul>
                              </ul>
                           <ul>
                            <li>Durden E, Pinto L, Lopez-Gonzalez L, Juneau P, Barron R. Two-year persistence and compliance with osteoporosis therapies among postmenopausal women in a commercially insured population in the United States. Arch Osteoporos. 2017;12:22.</li>
                           <ul>
                            <li>A retrospective claims-based analysis was conducted in the Medicare and Commercial MarketScan databases to examine the 12 and 24-month persistence and compliance to osteoporosis therapies. Patients enrolled were women ≥ 50 years old newly initiating osteoporosis therapy between January 1, 2012 to and December 31, 2012 and had continuous insurance eligibility for 14 months before and 24 months after their initial prescription. Persistence was defined as the absence of a > 60 day gap in continuous therapy. Therapies included in the analysis were injectable and oral prescription drug therapies approved for postmenopausal osteoporosis as of December 31, 2012 of varying dose frequencies. A total of 43,543 patients across Medicare and Commercial population were included and evaluated for persistence at 12 and 24 months of follow-up. Adherence rates were applied to therapies as a group. Individual drug adherence rates may vary depending on therapy.</li>
                            </ul>
                              </ul>
                           <ul>
                            <li>Leader Jr. D, Williams SA, Curtis JR, Gut R. Osteoporosis-Related Fracture Events in the U.S (M19). AMCP Nexus; 2017 Oct 16; Dallas, TX, USA. S78.</li>
                           <ul>
                            <li>A retrospective analysis of 2015 Medicare claims data using the Medicare Standard Analytical File (SAF). In addition, commercial insurance estimates were built using the following sources of data: Medicare SAF Inpatient Claims, Health Care Cost Utilization Project, CMS Hospital, U.S. Census Bureau demographics by ZIP code, and commercial claims from several states. Fractures were estimated for  hip, clinical vertebral, wrist, and other skeletal sites associated with fractures that commonly occur in patients with osteoporosis. More than 2 million fractured in 2015, with women accounting for 70% of the fractures.</li>
                            </ul>
                              </ul>
                           <ul>
                            <li>King AB, Saag KG, Burge RT, Pisu M, Goel N. Fracture Reduction Affects Medicare Economics (FRAME): Impact of increased osteoporosis diagnosis and treatment. Osteoporos Int. 2005;16(12):1545–57.</li>
                           <ul>
                            <li>An economic model used to estimate the impact of increased diagnosis and treatment. The model was a Markov structure and was used to predict fracture incidence and costs among postmenopausal women ≥ 65 years old over a 3 year period (2001-2003). The primary outputs of the model were focused on estimating the budget impact of testing an additional 1 million women.  As part of the model, secondary evidence was used as inputs with a key input focused on the percentage of patients treated following a bone mineral density test. The rate of treatment was based on data from Medicare supplemental and retiree plans, specifically the MarketScan claims database.</li>
                            </ul>
                              </ul>
                           <ul>
                            <li>IBM Micromedex RED BOOK 2019.</li>
                           <ul>
                            <li>A subscription-based database summarizing various drug acquisition list prices by National Drug Code (NDC). The database is updated regularly providing different types of list prices including commonly cited prices such as wholesale acquisition cost (WAC) and average wholesale price (AWP).  This economic model evaluates treatment costs based on listed WAC price as of March 2019. Note that WAC price reflects the price offered to wholesalers and does not take into account discounts, rebates, or other price concessions that may ultimately affect the net price. In addition, WAC price is subject to change at any time.</li>
                            </ul>
                              </ul>
                           <ul>
                            <li>Center for Medicare and Medicaid Services (CMS) Physician Fee Schedule Current Procedural Terminology (CPT) payment rates. Access on March 29, 2019. https://www.cms.gov/apps/physician-fee-schedule/license-agreement.aspx</li>
                           <ul>
                            <li>A public database providing payment rate information from a Medicare perspective for physician services by CPT code. Payment rates are provided by state and nationally as well as breakdowns by facility and non-facility rates, including modifiers.</li>
                            </ul>
                              </ul>
                           <ul>
                            <li>Pike C, Birnbaum HG, Schiller M, Sharma H, Burge R, Edgell ET. Direct and indirect costs of non-vertebral fracture patients with osteoporosis in the US. PharmacoEconomics. 2010;28(5):395–409.</li>
                           <ul>
                            <li>A retrospective analysis of administrative claims from 1999 through 2006 of  privately-insured (ages 18–64 years) and Medicare (ages 65+ years) patients with ≥1 osteoporosis-related non-vertebral fracture were matched to osteoporosis controls without a fracture based on age, sex, employment status, and geographic location. The analysis focused on the healthcare resource use and cost stratified by perspective. For primary-insured patients only, indirect costs were estimated based on work loss. All costs were reported in 2006 US dollars and all costs are reported as incremental costs.  A total 48,742 Medicare patients and 4,764 privately-insured patients were included in the analysis with at least one non-vertebral fracture. Indirect costs were estimated among a subset of the population (n = 1,148) using disability data including employer payments for short- and long-term disability and days related to absenteeism separate from the disability days using employee’s daily wages.</li>
                            </ul>
                              </ul>
                           <ul>
                            <li>Vanness DJ, Tosteson ANA. Estimating the Opportunity Costs of Osteoporosis in the United States: Top Geriatr Rehabil. 2005;21(1):4–16.</li>
                           <ul>
                            <li>An observational study to estimate the economic consequences of osteoporosis, specifically opportunity cost in the United States. Opportunity costs count not only medical costs of treating osteoporotic fractures but also costs of screening and prevention, informal caregiving and osteoporosis-related research and development. Using evidence from other literature, specifically a study in Canada found that on average ~3 hours of additional informal caregiving time per week per patient were incurred. Using a human capital approach for estimating resource use, the average wage in 2002 was $9.16 per hour of caregiving time. All costs estimates are presented in 2004 US Dollars.</li>
                            </ul>
                              </ul>
                           <ul>
                            <li>Imaz I, Zegarra P, González-Enríquez J, Rubio B, Alcazar R, Amate JM. Poor bisphosphonate adherence for treatment of osteoporosis increases fracture risk: systematic review and meta-analysis. Osteoporos Int. 2010 Nov;21(11):1943–51.</li>
                           <ul>
                            <li>A secondary analysis of the literature with the aim of estimating the persistence and compliance with bisphosphonates for the treatment of osteoporotic patients, and to estimate the influence of compliance on fracture risk. A systematic and  meta-analysis of was conducted on a total of 15 articles for 704,134 patients.</li>
                            </ul>
                              </ul>
                           <ul>
                            <li>Lewiecki EM, Adler R, Curtis J, Gagel R, Saag K, Singer A, et al. Hip Fractures and Declining DXA Testing: At a Breaking Point? J Bone Miner Res. 2016 Sep;31(S1):S1–411.</li>
                           <ul>
                            <li>A retrospective analysis by Leader et al 2017 of 2015 Medicare claims data using the Medicare Standard Analytical File (SAF). In addition, commercial insurance estimates were built using the following sources of data: Medicare SAF Inpatient Claims, Health Care Cost Utilization Project, CMS Hospital, U.S. Census Bureau demographics by ZIP code, and commercial claims from several states. Fractures were estimated for  hip, clinical vertebral, wrist, and other skeletal sites associated with fractures that commonly occur in patients with osteoporosis. More than 2 million fractured in 2015, with women accounting for 70% of the fractures.</li>
                            </ul>
                              </ul>
                           ")) 
                    ),
                  tags$style(HTML("
                                     #study_desc {
                                     padding-left: 1%;
                                     font-size:   12pt;
                                     line-height: 12pt;
          }
    "))
                    ),
                
                tags$img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Amgen.svg/1280px-Amgen.svg.png", width = "100px", height = "30px", style="display: block; margin-left: auto; margin-right: auto;")
                
                    ),
        # tabItem(tabName = "Terms",
        #         fluidRow(
        #           width = NULL, background = "black"),
        #         h3("Bending the Curve: The Estimated Long-Term Value of Improving Patient Identification and Treatment in Post-Menopausal Osteoporosis", align = "center"),
        #         fluidRow(
        #           boxPlus(
        #             width = 12,
        #             title = "Abbreviations",
        #             closable = FALSE, status = "warning", solidHeader = TRUE, collapsible = TRUE,
        #             fluidRow(
        #               column(width = 4, tags$b("ABBREVIATED FORM")),
        #               column(width = 8, tags$b("FULL FORM"))),
        #             fluidRow(column(width = 4, tags$ul(
        #               lapply(1:nrow(abbreviations), function(x) {
        #                 return(tags$li(abbreviations$abbrev[x]))
        #               })
        #             )),
        #             column(width = 8, tags$ul(
        #               lapply(1:nrow(abbreviations), function(x) {
        #                 return(tags$li(abbreviations$full[x]))
        #               })
        #             )))
        #           )
        #         ),
        #         
        #         tags$img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Amgen.svg/1280px-Amgen.svg.png", width = "100px", height = "30px", style="display: block; margin-left: auto; margin-right: auto;")            ),
        
        tabItem(tabName = "References",
                fluidRow(
                  width = NULL, background = "black"),
                h3("Bending the Curve: The Estimated Long-Term Value of Improving Patient Identification and Treatment in Post-Menopausal Osteoporosis", align = "center"),
                fluidRow(
                  boxPlus(
                    width = 12,
                    title = "References",
                    closable = FALSE, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    fluidRow(column(width = 12, tags$ul(
                      lapply(1:nrow(references), function(x) {
                        return(tags$li(references$refs[x]))
                      })
                    ))
                    )
                  )
                ),
                tags$img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Amgen.svg/1280px-Amgen.svg.png", width = "100px", height = "30px", style="display: block; margin-left: auto; margin-right: auto;")
        )
        
                    )
                  )
                ))