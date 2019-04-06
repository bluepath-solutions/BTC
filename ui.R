abbreviations <- data.frame(abbrev = c("BMD", "CPI", "DXA", "FRAX", "NHANES", "RANKL", "SERMs"),
                            full = c("bone mineral density", "Consumer Price Index", "dual-energy X-ray absorptiometry", "Fracture Risk Assessment Tool", "National Health and Nutrition Examination Survey",  
                                     "receptor activator of nuclear factor kappa-B ligand", "selective estrogen receptor modulators"))
references <- data.frame(refs = c("Brauer CA, Coca-Perraillon M, Cutler DM, Rosen AB. Incidence and Mortality of Hip Fractures in the United States. JAMA. 2009;302(14):1573-1579. doi:10.1001/jama.2009.1462", "Lewiecki EM, Wright NC, Curtis JR, et al. Hip fracture trends in the United States, 2002 to 2015. Osteoporos Int. 2018;29(3):717-722. doi:10.1007/s00198-017-4345-0", "Blume SW, Curtis JR. Medical costs of osteoporosis in the elderly Medicare population. Osteoporos Int. 2011;22(6):1835-1844. doi:10.1007/s00198-010-1419-7", "King AB, Fiorentino DM. Medicare Payment Cuts For Osteoporosis Testing Reduced Use Despite Tests’ Benefit In Reducing Fractures. Health Aff (Millwood). 2011;30(12):2362-2370. doi:10.1377/hlthaff.2011.0233", "Lewiecki EM, Adler R, Curtis J, et al. Hip Fractures and Declining DXA Testing: At a Breaking Point? J Bone Miner Res. 2016;31(S1):S1-S411. doi:10.1002/jbmr.3107", "Office of the Surgeon General (US). Bone Health and Osteoporosis: A Report of the Surgeon General. Rockville (MD): Office of the Surgeon General (US); 2004. http://www.ncbi.nlm.nih.gov/books/NBK45513/. Accessed July 12, 2018.", "U.S. Preventive Services Task Force. Screening for osteoporosis: U.S. preventive services task force recommendation statement. Ann Intern Med. 2011;154(5):356-364. doi:10.7326/0003-4819-154-5-201103010-00307", "Gillespie CW, Morin PE. Trends and Disparities in Osteoporosis Screening Among Women in the United States, 2008-2014. Am J Med. 2017;130(3):306-316. doi:10.1016/j.amjmed.2016.10.018", "Freemantle N, Cooper C, Diez-Perez A, et al. Results of indirect and mixed treatment comparison of fracture efficacy for osteoporosis treatments: a meta-analysis. Osteoporos Int. 2013;24(1):209-217. doi:10.1007/s00198-012-2068-9", "Yusuf AA, Matlon TJ, Grauer A, Barron R, Chandler D, Peng Y. Utilization of osteoporosis medication after a fragility fracture among elderly Medicare beneficiaries. Arch Osteoporos. 2016;11(1):31. doi:10.1007/s11657-016-0285-0", "Iba K, Takada J, Hatakeyama N, et al. Underutilization of antiosteoporotic drugs by orthopedic surgeons for prevention of a secondary osteoporotic fracture. J Orthop Sci. 2006;11(5):446-449. doi:10.1007/s00776-006-1050-9", "Andrade SE, Majumdar SR, Chan KA, et al. Low frequency of treatment of osteoporosis among postmenopausal women following a fracture. Arch Intern Med. 2003;163(17):2052-2057. doi:10.1001/archinte.163.17.2052", "Boudreau DM, Yu O, Balasubramanian A, et al. A Survey of Women’s Awareness of and Reasons for Lack of Postfracture Osteoporotic Care. J Am Geriatr Soc. 2017;65(8):1829-1835. doi:10.1111/jgs.14921", "Burge R, Dawson-Hughes B, Solomon DH, Wong JB, King A, Tosteson A. Incidence and Economic Burden of Osteoporosis-Related Fractures in the United States, 2005–2025. J Bone Miner Res. 2006;22(3):465–475.", "Goeree R, Blackhouse G, Adachi J. Cost-effectiveness of alternative treatments for women with osteoporosis in Canada. Curr Med Res Opin. 2006;22(7):1425-1436. doi:10.1185/030079906X115568", "Hiligsmann M, Bruyère O, Ethgen O, Gathon H-J, Reginster J-Y. Lifetime absolute risk of hip and other osteoporotic fracture in Belgian women. Bone. 2008;43(6):991-994. doi:10.1016/j.bone.2008.08.119", "Konnopka A, Jerusel N, König H-H. The health and economic consequences of osteopenia- and osteoporosis-attributable hip fractures in Germany: estimation for 2002 and projection until 2050. Osteoporos Int. 2009;20(7):1117-1129. doi:10.1007/s00198-008-0781-1", "Si L, Winzenberg TM, Jiang Q, Chen M, Palmer AJ. Projection of osteoporosis-related fractures and costs in China: 2010–2050. Osteoporos Int. 2015;26(7):1929-1937. doi:10.1007/s00198-015-3093-2", "Centre for Metabolic Bone Diseases, University of Sheffield, UK. FRAX® Fracture Risk Assessment Tool. https://www.sheffield.ac.uk/FRAX/. Accessed November 14, 2017.", "Centers for Disease Control and Prevention. NHANES - National Health and Nutrition Examination Survey. https://www.cdc.gov/nchs/nhanes/. Accessed May 3, 2017.", "Weaver J, Sajjan S, Lewiecki EM, Harris ST, Marvos P. Prevalence and Cost of Subsequent Fractures Among U.S. Patients with an Incident Fracture. J Manag Care Spec Pharm. 2017;23(4):461-471. doi:10.18553/jmcp.2017.23.4.461", "Colby, Sandra L., Ortman, Jennifer M. Projections of the Size and Composition of the U.S. Population: 2014 to 2060. US Census Bur Wash DC. 2014;Current Population Reports, P25-1143:13.", "United States Census Bureau. https://www.census.gov/.", "Imaz I, Zegarra P, González-Enríquez J, Rubio B, Alcazar R, Amate JM. Poor bisphosphonate adherence for treatment of osteoporosis increases fracture risk: systematic review and meta-analysis. Osteoporos Int. 2010;21(11):1943-1951. doi:10.1007/s00198-009-1134-4", "Durden E, Pinto L, Lopez-Gonzalez L, Juneau P, Barron R. Two-year persistence and compliance with osteoporosis therapies among postmenopausal women in a commercially insured population in the United States. Arch Osteoporos. 2017;12(1). doi:10.1007/s11657-017-0316-5", "King AB, Saag KG, Burge RT, Pisu M, Goel N. Fracture Reduction Affects Medicare Economics (FRAME): Impact of increased osteoporosis diagnosis and treatment. Osteoporos Int. 2005;16(12):1545-1557. doi:10.1007/s00198-005-1869-5", "Wolters Kluwer. Price Rx. https://pricerx.medispan.com/. Published 2018. Accessed May 14, 2018.", "2018 Physicians’ Fee & Coding Guide. InHealth; 2018.", "Pike C, Birnbaum HG, Schiller M, Sharma H, Burge R, Edgell ET. Direct and indirect costs of non-vertebral fracture patients with osteoporosis in the US. PharmacoEconomics. 2010;28(5):395-409. doi:10.2165/11531040-000000000-00000", "Vanness DJ, Tosteson ANA. Estimating the Opportunity Costs of Osteoporosis in the United States: Top Geriatr Rehabil. 2005;21(1):4-16. doi:10.1097/00013614-200501000-00003", "United States Department of Labor, Bureau of Labor Statistics. CPI (Consumer Price Index) Inflation Calculator. http://www.bls.gov/data/inflation_calculator.htm. Published 2018.", "Leslie WD, Majumdar SR, Morin SN, et al. FRAX for fracture prediction shorter and longer than 10 years: the Manitoba BMD registry. Osteoporos Int. 2017;28(9):2557-2564. doi:10.1007/s00198-017-4091-3", "Stout NK, Goldie SJ. Keeping the noise down: common random numbers for disease simulation modeling. Health Care Manag Sci. 2008;11(4):399-406. doi:10.1007/s10729-008-9067-6", "Leader Jr. D, Williams SA, Curtis JR, Gut R. Osteoporosis-Related Fracture Events in the U.S. Presented at the: AMCP Nexus; October 16, 2017; Dallas, TX, USA.", "Zhang J, Delzell E, Zhao H, et al. Central DXA utilization shifts from office-based to hospital-based settings among medicare beneficiaries in the wake of reimbursement changes. J Bone Miner Res. 2012;27(4):858-864. doi:10.1002/jbmr.1534", "Laliberté M-C, Perreault S, Dragomir A, et al. Impact of a primary care physician workshop on osteoporosis medical practices. Osteoporos Int. 2010;21(9):1471-1485. doi:10.1007/s00198-009-1116-6", "Majumdar SR, Johnson JA, McAlister FA, et al. Multifaceted intervention to improve diagnosis and treatment of osteoporosis in patients with recent wrist fracture: a randomized controlled trial. CMAJ. 2008;178(5):569-575. doi:10.1503/cmaj.070981", "Cranney A, Wells GA, Yetisir E, et al. Ibandronate for the prevention of nonvertebral fractures: a pooled analysis of individual patient data. Osteoporos Int. 2009;20(2):291-297. doi:10.1007/s00198-008-0653-8", "Miki RA, Oetgen ME, Kirk J, Insogna KL, Lindskog DM. Orthopaedic management improves the rate of early osteoporosis treatment after hip fracture. A randomized clinical trial. J Bone Joint Surg Am. 2008;90(11):2346-2353. doi:10.2106/JBJS.G.01246", "Majumdar SR, Beaupre LA, Harley CH, et al. Use of a case manager to improve osteoporosis treatment after hip fracture: results of a randomized controlled trial. Arch Intern Med. 2007;167(19):2110-2115. doi:10.1001/archinte.167.19.2110", "Solomon DH, Patrick AR, Schousboe J, Losina E. The Potential Economic Benefits of Improved Postfracture Care: A Cost-Effectiveness Analysis of a Fracture Liaison Service in the US Health-Care System. J Bone Miner Res. 2014;29(7):1667-1674. doi:10.1002/jbmr.2180", "Greenspan SL, Bilezikian JP, Watts NB, et al. A Clinician Performance Initiative to Improve Quality of Care for Patients with Osteoporosis. J Womens Health. 2013;22(10):853-861. doi:10.1089/jwh.2013.4388", "Goldshtein I, Gerber Y, Ish-Shalom S, Leshno M. Fracture Risk Assessment With FRAX Using Real-World Data in a Population-Based Cohort From Israel. Am J Epidemiol. 2018;187(1):94-102. doi:10.1093/aje/kwx128", "Bonaccorsi G, Messina C, Cervellati C, et al. Fracture risk assessment in postmenopausal women with diabetes: comparison between DeFRA and FRAX tools. Gynecol Endocrinol. 2018;34(5):404-408. doi:10.1080/09513590.2017.1407308", "Schousboe JT, Riekkinen O, Karjalainen J. Prediction of hip osteoporosis by DXA using a novel pulse-echo ultrasound device. Osteoporos Int. 2017;28(1):85-93. doi:10.1007/s00198-016-3722-4", "Cohen JT, Neumann PJ, Weinstein MC. Does Preventive Care Save Money? Health Economics and the Presidential Candidates. N Engl J Med. 2008;358(7):661-663. doi:10.1056/NEJMp0708558", "Winn AN, Ekwueme DU, Guy GP, Neumann PJ. Cost-Utility Analysis of Cancer Prevention, Treatment, and Control. Am J Prev Med. 2016;50(2):241-248. doi:10.1016/j.amepre.2015.08.009"
))
#########################################################################################################################################
fluidPage(
useShinyjs(),

dashboardPagePlus(
  skin = "blue",
  header = dashboardHeaderPlus(
    titleWidth=270,
    #tags$li(class = "dropdown", actionBttn(inputId="Next", label=icon("arrow-right"),color="primary",style="minimal", size="xs")),
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "info-circle",
    title = tagList(span(class = "logo-lg", "Bending the Curve"),
                    img(src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRb4Eku7HH9tk3KfqEtF5BXU5obNhUmWRT9rS_z8_U_U_0pWf-m")),
      dropdownBlock(
          id = "defaultDD",
          title = "Restore Defaults",
          badgeStatus = NULL,
          actionBttn(inputId="restoreall", label="Reset All Default Inputs",color="primary",style="minimal", size="xs"),
          actionBttn(inputId="restorepop", label="Reset Population Inputs",color="primary",style="minimal", size="xs"),
          actionBttn(inputId="restorefxrcosts", label="Reset Fracture Cost Inputs",color="primary",style="minimal", size="xs")
          )
  ),
  sidebar = 
    dashboardSidebar(
      width = 270,
      sidebarMenu(id = "tabs",
        menuItem("Home", tabName = "Home", icon = icon("home")),
        menuItem("Overview", icon = icon("sitemap"), tabName = "Overview"),
        menuItem("Inputs", icon = icon("file-prescription"), tabName = "Inputs"),
        menuItem("Fracture Costs", icon = icon("file-invoice-dollar"), tabName = "Fracture"),
        menuItem("Results", icon = icon("th-list"), tabName = "Scenarios"),
        menuItem("Conclusions", icon = icon("sign-out-alt"), tabName = "Results"),
        menuItem("Assumptions & Limitations", icon = icon("exclamation-triangle"), tabName = "Assumptions"),
        menuItem("_______________________________", tabName = "Break"),
        menuItem("Disclosures & Study Descriptions", icon = icon("file-alt"), tabName = "Disclosures"),
        menuItem("Abbreviations & Terminology", icon = icon("book-open"), tabName = "Terms"),
        menuItem("References", icon = icon("asterisk"), tabName = "References")
      )
    ),
  rightsidebar = rightSidebar(
    background = "dark",
    rightSidebarTabContent(
      id = 1,
      icon = "sitemap",
      title = "Model Overview",
      active = TRUE,
      rightSidebarMenu(
        rightSidebarMenuItem(
          icon = menuIcon(
            name = "lightbulb",
            color = "red"
          ),
          info = menuInfo(
            title = "Model Information",
            description = tags$p("The model analyzes US women age 65 years and older. Each patient is randomly assigned a unique set of demographics and characteristics based on a probabilsitic distribution. The resulting average across the patients reflects the population evarage inputs customized on the left. 
            Based on the profile of each patient, a 10-year fracture risk is applied and adjusted to reflect an annual risk. This risk is used to estimate the probability of fracture. 
            Total fractures are aggreated by type and monetized using direct and indirect costs. For each calendar year, hypothetical cohorts of a specified number of women are simulated within each model scenario.
            ",style = "font-size: 90%;")
          ))))
    ),
    
  body <- dashboardBody(
    
    hidden(actionBttn(inputId="Previous", label=icon("arrow-left"),color="primary",style="float", size="xs")),  
    hidden(actionBttn(inputId="Next", label=icon("arrow-right"),color="primary",style="float", size="xs")),  
    
    #hidden(actionBttn(inputId ="Previous", label = icon("arrow-left"))),
    
    setShadow("box"),
    
    tabItems(
      tabItem(tabName = "Home",
              h2("BENDING THE CURVE", align = "center"),
              fluidRow(
                widgetUserBox(
                  title = "",
                  subtitle = "",
                  type = NULL,
                  width = 12,
                  height = 250,
                  src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRb4Eku7HH9tk3KfqEtF5BXU5obNhUmWRT9rS_z8_U_U_0pWf-m",
                  color = "gray",
                  boxToolSize = "lg",
                  background = TRUE,
                  backgroundUrl = "https://dlg7f0e93aole.cloudfront.net/wp-content/uploads/Osteoporosis.jpg",
                  collapsible = FALSE,
                  closable = FALSE,
                  HTML(paste(br(),"The Estimated Long-Term Value of Improving Patient Identification and Treatment in Post-Menopausal Osteoporosis")), align = "center", style = "font-size: 175%;"
                  #footer = "Disclaimer: This resource contains healthcare economic information and is intended  only for formulary committees or other similar entities with drug selection responsibilities, pursuant to Section 114 of the FDA Modernization Act.", align = "center"
                )),
              headerPanel(""),
              fluidRow(boxPlus(
                title = "Disclaimer", 
                closable = FALSE, 
                status = "info",
                width = 12,
                solidHeader = FALSE, 
                collapsible = TRUE,
                p("This resource contains healthcare economic information and is intended  only for formulary committees or other similar entities with drug selection responsibilities, pursuant to Section 114 of the FDA Modernization Act.")
              )),
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
                h3("Bending the Curve: The Estimated Long-Term Value of Improving Patient Identification and Treatment in Post-Menopausal Osteoporosis", align = "center"),
                fluidRow(
                  box(title = "Model Overview", width = 12, status = "primary",
                      footer = fluidRow(
                        column(
                          width = 12,
                          descriptionBlock(    
                      tags$img(src="Model_Flow.PNG", width = "800px", height = "150px", style="display: block; margin-left: auto; margin-right: auto;")),
                      text = "Model Flow")
                      ))),
                fluidRow(
                  box(
                    title = "Objective", width = 6,  status = "primary",
                    "To estimate the future clinical and economic burden of osteoporotic fractures in the US with and without policy changes."),
                  box(
                    title = "Structure", width = 6,  status = "primary",
                    "An R-based invidual-level microsimulation model, informed by NHANES estimates of risk factors and utilizing the FRAX risk prediction tool (based on publicly available risk tables)")),
                fluidRow(
                  box(
                    title = "Time Horizon", width = 6,  status = "primary",
                    HTML("•	Cross-sectional calendar year <br/>
                    •	The model evaluates the impact by calendar year from 2019 through 2040")),
                  box(
                    title = "Population", width = 6, status = "primary",
                    HTML("US women aged 65 years and older, including those with and without a history of fractures<br/>
                          &nbsp"))),
                fluidRow(
                  box(
                    title = "Clinical Inputs", width = 6, status = "primary",
                    HTML("•	Population demographics and charactersitics <br/>
                         • Fracture risk for untreated and treated population by therapy")),
                  box(
                    title = "Economic Inputs", width = 6,  status = "primary",
                    HTML("•	Identification and treatment cost by therapy <br/>
                         • Fracture costs (direct and indirect)"))),
                fluidRow(
                  box(
                    title = "Model Outputs", width = 12, status = "primary",
                    column(width = 6, HTML("•	Annual costs (total and by category) <br/>
                          &nbsp;&nbsp;&nbsp;&nbsp  o	Cumulative costs")),
                    column(width = 6, HTML("•	Annual fractures rates (total and by site)  <br/>
                          &nbsp;&nbsp;&nbsp;&nbsp o	Cumulative fractures"))
))),
          tags$img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Amgen.svg/1280px-Amgen.svg.png", width = "100px", height = "30px", style="display: block; margin-left: auto; margin-right: auto;")
              
      ),
      
      tabItem(tabName = "Inputs",
              fluidRow(
                width = NULL, background = "black"
              ),
              h3("Bending the Curve: The Estimated Long-Term Value of Improving Patient Identification and Treatment in Post-Menopausal Osteoporosis", align = "center"),
              fluidRow(
                boxPlus(title = "Population Size", width = 12, closable = FALSE, collapsible = TRUE,
                        enable_dropdown = TRUE, dropdown_icon = "question-circle",
                        dropdown_menu = dropdownItemList(
                          dropdownDivider(),
                          dropdownItem(name = HTML("Enter the desired population size to be analyzed. Among this population, risk factors are entered, <br/>
                                                                which are used to estimate the number of women with a fracture based on the FRAX index. Risk <br/>
                                                                factor prevalence was estimated from analyzing NHANES, a nationally-representative population-based <br/> 
                                                                survey conducted every 2 years. Fracture risks for individual women in the model was estimated using <br/>
                                                                FRAX simplified tables based on the baseline population demographics and charactersitics. Each patient<br/>
                                                                is randomly assigned a unique set of demographics and characteristics based on a probabilsitic <br/>
                                                                distribution. The resulting average across the patients reflects the population evarage inputs customized <br/>
                                                                on the left. Based on the profile of each patient, a 10-year fracture risk is applied and adjusted to <br/>
                                                                reflect an annual risk. This risk is used to estimate the probability of fracture. Total fractures are<br/>
                                                                aggreated by type and monetized using direct and indirect costs."))),
                fluidRow(
                  column(width = 4, numericInput(inputId = "pop_input", label = "Eligible Population", value = 1000000, min = 0, max = 30000000))),
                    # bsTooltip("pop_input", "Enter eligible population. Default value of 1 million is based on ....","right", options = list(container = "body"))),
                box(id="riskdesc", tags$p("Risk Factors", tags$sup(style="font-size: 50px")), align = "left", color = "blue", width = 16, height = 4),
                  bsTooltip("riskdesc", "Risk factor prevalence was estimated from analyzing NHANES. Fracture risks for individual women in the model is estimated using FRAX simplified tables based on the baseline population demographics and charactersitics.",
                          "left", options = list(container = "body")),
                tags$br(),
                  fluidRow(  
                  column(width = 4, numericInput(inputId = "BMD_mean", label = "Mean Bone Mineral Density", value = 0.67, min = 0, max = 1)),
                    bsTooltip("BMD_mean", "NHANES data (2013-2014) were used to estimate mean BMD and the standard deviation.",
                            "right", options = list(container = "body")),
                  column(width = 4, numericInput(inputId = "BMD_SD", label = "Bone Mineral Density Standard Deviation", value = 0.12, min = 0, max = 1)),
                  bsTooltip("BMD_SD", "NHANES data (2013-2014) were used to estimate mean BMD and the standard deviation.",
                            "right", options = list(container = "body"))),
                fluidRow(
                  column(width = 4, numericInput(inputId = "RA_inp", label = "Rheumatoid Arthritis (%)", value = 8.6, min = 0, max = 100)),
                  bsTooltip("RA_inp", "Centers for Disease Control and Prevention. NHANES - National Health and Nutrition Examination Survey. https://www.cdc.gov/nchs/nhanes/. Accessed May 3, 2017.",
                            "right", options = list(container = "body")),
                  column(width = 4, numericInput(inputId = "fxr_inp", label = "Previous Fracture (%)", value = 14.2, min = 0, max = 100)),
                  bsTooltip("fxr_inp", "Centers for Disease Control and Prevention. NHANES - National Health and Nutrition Examination Survey. https://www.cdc.gov/nchs/nhanes/. Accessed May 3, 2017.",
                            "right", options = list(container = "body")),
                  column(width = 4, numericInput(inputId = "parfxr_inp", label = "Parent History of Hip Fracture (%)", value = 12.0, min = 0, max = 100)),
                  bsTooltip("parfxr_inp", "Centers for Disease Control and Prevention. NHANES - National Health and Nutrition Examination Survey. https://www.cdc.gov/nchs/nhanes/. Accessed May 3, 2017.",
                            "left", options = list(container = "body"))),
                fluidRow(
                  column(width = 4, numericInput(inputId = "smoker", label = "Smoker (%)", value = 8.1, min = 0, max = 100)),
                  bsTooltip("smoker", "Centers for Disease Control and Prevention. NHANES - National Health and Nutrition Examination Survey. https://www.cdc.gov/nchs/nhanes/. Accessed May 3, 2017.",
                            "right", options = list(container = "body")),
                  column(width = 4, numericInput(inputId = "alco", label = "Excessive Alcohol Use (%)", value = 2.9, min = 0, max = 100)),
                  bsTooltip("alco", "Centers for Disease Control and Prevention. NHANES - National Health and Nutrition Examination Survey. https://www.cdc.gov/nchs/nhanes/. Accessed May 3, 2017.",
                            "right", options = list(container = "body")),
                  column(width = 4, numericInput(inputId = "gluco_tx", label = "Long-Term Glucocorticoid Therapy (%)", value = 8.7, min = 0, max = 100)),
                  bsTooltip("gluco_tx", "Centers for Disease Control and Prevention. NHANES - National Health and Nutrition Examination Survey. https://www.cdc.gov/nchs/nhanes/. Accessed May 3, 2017.",
                            "left", options = list(container = "body"))),
                h5("Race/Ethnicity Distribution (%)"),
                fluidRow(
                  column(width = 3, numericInput(inputId = "RE_cauc", label = "Caucasian", value = 68.3, min = 0, max = 100)),
                    bsTooltip("RE_cauc", "Centers for Disease Control and Prevention. NHANES - National Health and Nutrition Examination Survey. https://www.cdc.gov/nchs/nhanes/. Accessed May 3, 2017.",
                            "right", options = list(container = "body")),
                  column(width = 3, numericInput(inputId = "RE_hisp", label = "Hispanic", value = 15.4, min = 0, max = 100)),
                    bsTooltip("RE_hisp", "Centers for Disease Control and Prevention. NHANES - National Health and Nutrition Examination Survey. https://www.cdc.gov/nchs/nhanes/. Accessed May 3, 2017.",
                            "right", options = list(container = "body")),
                  column(width = 3, numericInput(inputId = "RE_asian", label = "Asian", value = 4.5, min = 0, max = 100)),
                    bsTooltip("RE_asian", "Centers for Disease Control and Prevention. NHANES - National Health and Nutrition Examination Survey. https://www.cdc.gov/nchs/nhanes/. Accessed May 3, 2017.",
                            "left", options = list(container = "body")),
                  column(width = 3, numericInput(inputId = "RE_black", label = "Black", value = 11.8, min = 0, max = 100)),
                  bsTooltip("RE_black", "Centers for Disease Control and Prevention. NHANES - National Health and Nutrition Examination Survey. https://www.cdc.gov/nchs/nhanes/. Accessed May 3, 2017.",
                            "left", options = list(container = "body")))
                )),
              fluidRow(
                boxPlus(title = "Clinical Inputs", width = 12, closable = FALSE, collapsible = TRUE, collapsed = TRUE,
                        enable_dropdown = TRUE, dropdown_icon = "question-circle",
                        dropdown_menu = dropdownItemList(
                          dropdownItem(name = "A pre-set market basket of treatments is analyzed with the market mix, monthly cost, and efficacy shown here.")),
                        
                        fluidRow(
                          column(width = 6, h5(" ")),
                          column(width = 2, h5(" ")),
                          column(width = 4, h5(strong("Efficacy")), align = "center")),
                        fluidRow(
                          column(width = 4, h5(strong("Medication")), align = "left"),
                          column(width = 2, h5(strong("Treatment Mix (%)")), align = "center"), 
                          column(width = 2, h5(strong("Monthly Cost ($)")), align = "center"),
                          column(width = 2, h5(strong("Hip")), align = "center"),
                          column(width = 2, h5(strong("Other")), align = "center")),
                        
                        fluidRow(
                          column(width = 4, h5("Anti-Resportive Agents")),
                          column(width = 2, box(id="AR_MS", tags$p("91.5%", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4)),
                          bsTooltip("AR_MS", "Market share inputs are based on utilization trends as of 2018, DOF.",
                                    "right", options = list(container = "body")),
                          column(width = 2, box(id="AR_cost", tags$p("$195.62", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4)),
                          bsTooltip("AR_cost", "Monthly costs are based on a weighted averge of costs - weighted by market share - from Medicare Part D files as of 2018.",
                                    "right", options = list(container = "body")),
                          column(width = 2, box(id="AR_eff", tags$p("0.61", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4)),
                          bsTooltip("AR_eff", "Efficacy of anti-resportives is based on a weighted average efficacy - weighted by market share - taken from NMA, 2018...",
                                    "left", options = list(container = "body")),
                          column(width = 2, box(id="AR_effoth", tags$p("0.58", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4))),
                        bsTooltip("AR_effoth", "Efficacy of denosumab is taken from NMA, 2018...",
                                  "left", options = list(container = "body")),
                        fluidRow(
                          column(width = 4, h5("Selective Estrogen Receptor Modulators (SERMs)")),
                          column(width = 2, box(id="SERM_MS", tags$p("7.4%", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4)),
                          bsTooltip("SERM_MS", "Market share inputs are based on utilization trends as of 2018, DOF.",
                                    "right", options = list(container = "body")),
                          column(width = 2, box(id="SERM_cost", tags$p("$158.84", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4)),
                          bsTooltip("SERM_cost", "Monthly costs are based on a weighted averge of costs - weighted by market share - from Medicare Part D files as of 2018.",
                                    "right", options = list(container = "body")),
                          column(width = 2, box(id="SERM_eff", tags$p("0.59", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4)),
                          bsTooltip("SERM_eff", "Efficacy of SERMs is taken from NMA, 2018...",
                                    "left", options = list(container = "body")),
                          column(width = 2, box(id="SERM_effoth", tags$p("0.59", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4))),
                        bsTooltip("SERM_effoth", "Efficacy of SERMs is taken from NMA, 2018...",
                                  "left", options = list(container = "body")),
                        fluidRow(
                          column(width = 4, h5("Bone Builders / PTH")),
                          column(width = 2, box(id="PTH_MS", tags$p("1.1%", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4)),
                          bsTooltip("PTH_MS", "Market share inputs are based on utilization trends as of 2018, DOF.",
                                    "right", options = list(container = "body")),
                          column(width = 2, box(id="PTH_cost", tags$p("$2,997.90", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4)),
                          bsTooltip("PTH_cost", "Monthly costs are based on a weighted averge of costs - weighted by market share - from Medicare Part D files as of 2018",
                                    "right", options = list(container = "body")),
                          column(width = 2, box(id="PTH_eff", tags$p("0.25", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4)),
                          bsTooltip("PTH_eff", "Efficacy of bone builders / PTHs is taken from NMA, 2018...",
                                    "left", options = list(container = "body")),
                          column(width = 2, box(id="PTH_effoth", tags$p("0.34", tags$sup(style="font-size: 15px")), align = "center", color = "blue", width = 16, height = 4)),
                        bsTooltip("PTH_effoth", "Efficacy of PTHs is taken from NMA, 2018...",
                                  "left", options = list(container = "body")))),
                
                tags$img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Amgen.svg/1280px-Amgen.svg.png", width = "100px", height = "30px", style="display: block; margin-left: auto; margin-right: auto;")
                )
      ),
      tabItem(tabName = "Fracture",
              fluidRow(
                width = NULL, background = "black"),
              h3("Bending the Curve: The Estimated Long-Term Value of Improving Patient Identification and Treatment in Post-Menopausal Osteoporosis", align = "center"),
              fluidRow(
                boxPlus(id="FxrCostInp", title = "Fracture Costs", width = 12, closable = FALSE, collapsible = TRUE,
                    enable_dropdown = TRUE, dropdown_icon = "question-circle",
                    dropdown_menu = dropdownItemList(
                      dropdownItem(name = HTML("The entered costs represent annual payer amounts. Patients who incur a fracture also increase an <br/>
                                                          increased risk of mortality. A hazard ratio of 2.27 was applied to those incurring a fracture.")),
                      dropdownDivider(),
                      dropdownItem(name = HTML("All direct costs must be included in the analyses, but indirect costs can be excluded. If the <br/>
                                                          'Include Indirect Costs' box is unchecked, both components of indirect costs will be excluded." ))
                      
                      ),
                        fluidRow(
                          column(width = 4, h5(" ")),
                          column(width = 8, h5(strong("Costs ($)")), align = "center")),
                        fluidRow(
                          column(width = 4, h5(strong("Direct Cost Inputs")), align = "left"),
                          column(width = 4, h5(strong("One per Year")), align = "center"),
                          column(width = 4, h5(strong("> 1 Per Year")), align = "center")),
                        fluidRow(
                          column(width = 4, h5("Inpatient Stay", align = "left")),
                          column(width = 4, numericInput(inputId = "costinpt1", label = NULL, value = "9576.40")),
                          column(width = 4, numericInput(inputId = "costinpt2", label = NULL, value = "16476.79"))),
                        fluidRow(
                          column(width = 4, h5("Outpatient Visit", align = "left")),
                          column(width = 4, numericInput(inputId = "costoutpt1", label = NULL, value = "2842.79")),
                          column(width = 4, numericInput(inputId = "costoutpt2", label = NULL, value = "4353.50"))),
                        fluidRow(
                          column(width = 4, h5("Long-Term Care", align = "left")),
                          column(width = 4, numericInput(inputId = "costLTC1", label = NULL, value = "4065.13")),
                          column(width = 4, numericInput(inputId = "costLTC2", label = NULL, value = "8720.98"))),
                        fluidRow(
                          column(width = 4, h5("Emergency Department Visit", align = "left")),
                          column(width = 4, numericInput(inputId = "costED1", label = NULL, value = "870.48")),
                          column(width = 4, numericInput(inputId = "costED2", label = NULL, value = "1216.96"))),
                        fluidRow(
                          column(width = 4, h5("Other", align = "left")),
                          column(width = 4, numericInput(inputId = "costother1", label = NULL, value = "2501.70")),
                          column(width = 4, numericInput(inputId = "costother2", label = NULL, value = "4295.39"))),
                        fluidRow(
                          column(width = 4, h5("Pharmacy", align = "left")),
                          column(width = 4, numericInput(inputId = "costpharm1", label = NULL, value = "2173.52")),
                          column(width = 4, numericInput(inputId = "costpharm2", label = NULL, value = "2487.71"))),
                    fluidRow(
                      box(id="IndirectCosts", "", color = "blue", width = 12, height = 4),
                      column(width = 4, prettyCheckbox(inputId = "IndirectCosts", strong("Include Indirect Costs"), value = TRUE, shape = "square"))),
                      fluidRow(
                        column(width = 4, h5("Productivity Losses", align = "left")),
                        column(width = 4, numericInput(inputId = "costprod1", label = NULL, value = "2445.00")),
                        column(width = 4, numericInput(inputId = "costprod2", label = NULL, value = "2487.71"))),
                      fluidRow(
                        column(width = 4, h5("Informal Caregiver", align = "left")),
                        column(width = 4, numericInput(inputId = "costcare1", label = NULL, value = "1770.60")),
                        column(width = 4, numericInput(inputId = "costcare2", label = NULL, value = "2487.71")))),
                bsTooltip("FxrCostInp", "Default direct costs following a fracture were based on a claims analysis, differed by category (i.e., inpatient, outpatient, emergency department, long-term care, pharmacy costs following a fracture, and other), and differed for individuals experiencing a single fracture within a year vs. those with a subsequent fracture. Outpatient services included claims for radiology, primary care, outpatient hospital visits, orthopedic specialist visits, and rehabilitation services. Long-term care services were defined as at least 1 longterm care stay in a rehabilitation or skilled nursing facility. Other costs include XYZ. All costs are updated to USD 2019 using the Medical Care index from the Bureau of Labor Statistics. ",
                          "left", options = list(container = "body"))),
                fluidRow(boxPlus(width=12,
                  plotlyOutput("costp"),
                  #tableOutput("coststbl"),
                  fill=FALSE)),
              
              tags$img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Amgen.svg/1280px-Amgen.svg.png", width = "100px", height = "30px", style="display: block; margin-left: auto; margin-right: auto;")
      ),
      tabItem(tabName = "Scenarios",
              fluidRow(
                width = NULL, background = "black"),
              h3("Bending the Curve: The Estimated Long-Term Value of Improving Patient Identification and Treatment in Post-Menopausal Osteoporosis", align = "center"),
              fluidRow(
                boxPlus(title = "Treatment, Identification Rates & Time Horizon", width = 12, closable = FALSE, collapsible = TRUE,
                        enable_dropdown = TRUE, dropdown_icon = "question-circle",
                        dropdown_menu = dropdownItemList(
                          dropdownItem(name = HTML("Define the base case and scenarios to evaluate over the time horizon. You may adjust the <br/>
                                                          scenarios by entering in a new identification and treatment rate to the left.")),
                          dropdownDivider(),
                          dropdownItem(name = HTML("The microsimulation is estimating the effects of identification and treatment across <br/>
                                                a cohort of the size entered in the inputs sheet. Please allow time for the simulation to run."))),
                        fluidRow(
                          column(width = 4, h5(" ")),
                          column(width = 4, h5(strong("Identification (%)")), align = "center"),
                          column(width = 4, h5(strong("Treatment (%)")), align = "center")),
                        fluidRow(
                          column(width = 4, align="center", h5("Base Case")),
                          column(width = 4, numericInput(inputId = "basecaseID", label = NULL, value = "11.3")),
                          column(width = 4, numericInput(inputId = "basecaseTx", label = NULL, value = "10.4"))),
                        
                        fluidRow(
                          column(width = 4, align="center", h5("New Scenario")),
                          column(width = 4, numericInput(inputId = "scenario1ID", label = NULL, value = "31.3")),
                          column(width = 4, numericInput(inputId = "scenario1Tx", label = NULL, value = "17.8"))),
                      fluidRow(
                        column(width = 4, align="center", h5("Time Horizon")),
                        column(width = 7,
                        sliderInput(inputId = "endYear", 
                                    label = NULL,
                                    min = as.Date("2022", '%Y'),
                                    max = as.Date("2040", '%Y'),
                                    value = as.Date("2040", '%Y'),
                                    step = 365,
                                    timeFormat = "%Y"), align = "center"),
                        column(width = 1)
                        ))),
              boxPlus(width = 12, closable = FALSE, collapsible = TRUE,
                      fluidRow(
                        column(width = 6, infoBoxOutput("FraxBox_R"), tags$style("#FraxBox_R {width:100%}")),
                        column(width = 6, infoBoxOutput("CostBox_R"), tags$style("#CostBox_R {width:100%}") ))),
              fluidRow(
                boxPlus(withSpinner(plotlyOutput("fxrplot")), width = 6),
                boxPlus(withSpinner(plotlyOutput("costplot")), width = 6)),
              
              
              tags$img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Amgen.svg/1280px-Amgen.svg.png", width = "100px", height = "30px", style="display: block; margin-left: auto; margin-right: auto;")
    ),
    tabItem(tabName = "Results",
            fluidRow(
              width = NULL, background = "black"),
            h3("Bending the Curve: The Estimated Long-Term Value of Improving Patient Identification and Treatment in Post-Menopausal Osteoporosis", align = "center"),
            fluidRow(
              boxPlus(title = "Total Estimated Fractures", 
                      closable = FALSE, status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 8,
                      p(textOutput('totalfxr_content'))
              ),
                infoBoxOutput("FraxBox")), 
            fluidRow(
              boxPlus(title = "Total Estimated Costs", 
                    closable = FALSE, status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 8,
                    p(textOutput('totalcost_content'))
            ),
            infoBoxOutput("CostBox"))
            ,
        fluidRow(
          boxPlus(title = "Thank you for visiting.", 
                  closable = FALSE, status = "info", solidHeader = FALSE, collapsible = TRUE, width = 12,
                  p("Thank you for visiting the Bend the Curve Microsimulation Site. The results of the model based on the current inputs suggest.... Using the validated FRAX tables and insights from NHANES to estimate the future burden among Medicare-eligible women, we found that total fractures and fracture-related costs will increase substantially under the status quo of under-diagnosis and under-treatment of osteoporosis")
          )),
        
        tags$img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Amgen.svg/1280px-Amgen.svg.png", width = "100px", height = "30px", style="display: block; margin-left: auto; margin-right: auto;")
        ),
    
    tabItem(tabName = "Assumptions",
            fluidRow(
              width = NULL, background = "black"),
            h3("Bending the Curve: The Estimated Long-Term Value of Improving Patient Identification and Treatment in Post-Menopausal Osteoporosis", align = "center"),
            fluidRow(
              boxPlus(
                width = 6,
                title = "Assumptions", 
                closable = FALSE, 
                status = "warning", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                p("We assumed costs would remain constant in the future, which has not been the case historically. To the extent that healthcare costs continue rising, the growth in projected spending would be an underestimate. Our assumption that those at highest risk would be treated first might not reflect reality, although it should be the goal in clinical practice. In estimating the treatment effectiveness, we assumed that all fractures could be prevented at rates shown in meta-analyses; however, treatment might not prevent fractures to all potential sites at the same rate as measured in clinical trials. The risk of fractures was estimated using the simplified charts from FRAX, and although they have been well validated, they are not perfect predictors.")
              ),
              boxPlus(
                width = 6,
                title = "Limitations", 
                closable = FALSE, 
                status = "warning", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                p("In any model-based analysis, especially when making projections as far out as 2040, uncertainty exists and simplifying assumptions must be made. However, we erred on the conservative side, underestimating the results to provide a lower bound on the potential benefits of increased case finding and treatment. We did not attempt to quantify the clinical benefits in terms of quality-adjusted life years, and this could be an area of future research. Additionally, we only considered currently available treatments. To the extent that new treatment alternatives are introduced that are more efficacious, the fracture reduction and cost savings in scenarios with increased utilization would be an underestimate of the true benefit.")
              )
            ),
           
            tags$img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Amgen.svg/1280px-Amgen.svg.png", width = "100px", height = "30px", style="display: block; margin-left: auto; margin-right: auto;")
            ),

    tabItem(tabName = "Disclosures",
            fluidRow(
              width = NULL, background = "black"),
            h3("Bending the Curve: The Estimated Long-Term Value of Improving Patient Identification and Treatment in Post-Menopausal Osteoporosis", align = "center"),
            fluidRow(
              boxPlus(
                width = 12,
                title = "Financial Disclosures", 
                closable = FALSE, 
                status = "warning", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                p(HTML("The following studies used in the economic model involved an Amgen employee or were sponsored by Amgen. These disclosures are specific  
                  to those listed within the publication or source document used in the development of the economic model. <br/>
                         •	Data on file, Amgen; [US Bend the Curve PMO Microsimulation; 2019] <br/>
                             &nbsp;&nbsp;&nbsp;&nbsp; o	This analysis was funded by Amgen, Inc. <br/>
                          •	Freemantle N, Cooper C, Diez-Perez A, Gitlin M, Radcliffe H, Shepherd S, et al. Results of indirect and mixed treatment comparison of fracture efficacy for osteoporosis treatments: a meta-analysis. Osteoporos Int. 2013;24(1):209–17. <br/>
                              &nbsp;&nbsp;&nbsp;&nbsp; o	This study was funded by Amgen Inc. <br/>
                              &nbsp;&nbsp;&nbsp;&nbsp; o	Freemantle N has received research grants from Amgen, Inc. and has served as a consultant for Amgen, Inc. Cooper C has received consulting and lecture fees from Amgen, Inc.,  Diez-Perez A has received honoraria or consulted for Amgen, Inc., and received research grants from Amgen, Inc.  Roux C has received research grants, and/or honoraria from Amgen, <br/>
                              &nbsp;&nbsp;&nbsp;&nbsp; o	Gitlin M, Radcliffe H, Shepherd S were employees and shareholders in Amgen, Inc. at the time of the study. <br/>
                          •	Durden E, Pinto L, Lopez-Gonzalez L, Juneau P, Barron R. Two-year persistence and compliance with osteoporosis therapies among postmenopausal women in a commercially insured population in the United States. Arch Osteoporos. 2017; 12:22.
o	This study was funded by Amgen Inc.<br/>
                            &nbsp;&nbsp;&nbsp;&nbsp; o	Durden E, Lopez-Gonzalez L, Juneau P are employees of Truven Health Analytics, who were paid by Amgen Inc. for conducting this study.<br/>
                            &nbsp;&nbsp;&nbsp;&nbsp; o	Pinto L and Barron R were employees and shareholders in Amgen, Inc. at the time of the study. Jessica Ma, PhD (Amgen Inc.) provided medical writing support."
                       
                ))
              )),
            fluidRow(
              boxPlus(
                width = 12,
                title = "Study Descriptions", 
                closable = FALSE, 
                status = "warning", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                p(HTML("The following studies were used in the economic model as model inputs or as source evidence used to inform model inputs. Other evidence was used to support model assumptions and such evidence are included as references but have not been summarized below.<br/>
                •	Data on file, Amgen; [US Bend the Curve PMO Microsimulation; 2019] <br/>
                 &nbsp;&nbsp;o	This analysis used an economic simulation model developed to assess the clinical and economic burden of osteoporotic fractures in the US with and without policy changes. The model takes a population perspective focused on women ≥ 65 years old. Multiple sources were utilized to develop the simulation and analysis including epidemiology, fracture risk factors, rate of fracture based on risk factors, DXA scanning and treatment rates, as well as direct and indirect fracture costs. <br/>
                •	Centre for Metabolic Bone Diseases, University of Sheffield, UK. FRAX® Fracture Risk Assessment Tool. Available from: https://www.sheffield.ac.uk/FRAX/ <br/>
                o	The University of Sheffield launched the FRAX tool in 2008. The FRAX® algorithms give the 10-year probability of fracture. The output is a 10-year probability of hip fracture and the 10-year probability of a major osteoporotic fracture (clinical spine, forearm, hip or shoulder fracture).The FRAX® model was developed from studying population-based cohorts from Europe, North America, Asia and Australia. The simplified tables that are publicly accessible are structured by geography and race. The US specific tables include Caucasian, Asian, black and Hispanic. All data tables are structured by fracture output and provided in 5-year age increments. <br/>
                       
                       •	Centers for Disease Control and Prevention. NHANES - National Health and Nutrition Examination Survey. Available from: https://www.cdc.gov/nchs/nhanes/<br/>
                       o	The National Health and Nutrition Examination Survey (NHANES) is a program designed to assess the health and nutritional status of adults and children in the United States. The NHANES data is based on interviews and physical examinations and includes demographic, socioeconomic, dietary, and health-related questions. The examination component consists of medical, dental, and physiological measurements, as well as laboratory tests administered by highly trained medical personnel. NHANES data were structured into datasets of 2 year with the 2013-2014 providing the most recent dataset available with all needed variables. In 2013-2014, 14,332 persons were selected from 30 different survey locations and 10,175 completed the interview and 9,813 were examined.<br/>
                       
                       •	Weaver J, Sajjan S, Lewiecki EM, Harris ST, Marvos P. Prevalence and Cost of Subsequent Fractures Among U.S. Patients with an Incident Fracture. J Manag Care Spec Pharm. 2017;23(4):461–71. <br/>
                       o	A retrospective analysis of two large US administrative claims databases was conducted between 2007 and 2014. The analysis is based on the Humana Medicare Advantage and the Optum commercially insured populations. Medicare patients were included if they were ≥ 65 years old and commercial patients were included if they were ≥ 50 years old. Patients were included if they experienced a fracture with at least 1 year of insurance eligibility before and after the fracture date. Patients were stratified into two cohorts, including those who incurred 1 fracture and those who experience > 1 fracture (after a 3 month gap from index fracture) during the 12-month follow-up post the initial index fracture. Analysis of fracture costs was specific to direct medical (inpatient, outpatient, emergency department, long-term care, other services) and pharmacy costs. A total of 45,603 patients were included in the Medicare group. All costs were presented after 1:1 patient-level propensity score match of the two cohorts (n= 4,549 per cohort) based on facture site, year of fracture, gender. Region, employment status, race and Charlson comorbidity index.  All costs were reported in 2014 US dollars and all costs are reported as incremental costs.  <br/>
                       
                       •	Colby, SL., Ortman, JM. Projections of the Size and Composition of the U.S. Population: 2014 to 2060. US Census Bur Wash DC. 2014 Mar; Current Population Reports, P25-1143:13. <br/>
                       o	The census projections are for the US including all 50 states and the District of Columbia.  The projection estimates were produced using cohort-component methods ((births, deaths, and net international migration) based on the 2010 Census. These methods are based on methods similar to time series analysis.  Historical trends in mortality, fertility rates and net international migration were to is advance the population projection each year by using. <br/>
                       o	The 2014 National Projections, including summary tables, downloadable files, and methodology and assumptions, can be found at <www.census.gov /population/projections/data/national/2014.html>. <br/>
                       
                       •	Pike, CT, Birnbaum HG, Schiller M, Swallow E, Burge RT, Edgell ET. Prevalence and costs of osteoporotic patients with subsequent non-vertebral fractures in the US.  Osteoporos Int. 2011;22:2611–2621.<br/>
                       o	A retrospective analysis of administrative claims from 1999 through 2006 of  privately-insured (ages 18–64 years) and Medicare (ages 65+ years) patients with ≥1 subsequent osteoporosis-related non-vertebral fracture within a year the initial non-vertebral fractures were included. Patients were matched to controls who experiences an initial non-vertebral fracture but no subsequent fractures. Subsequent fractures were included if occurring >3 months after the incident fracture or >6 months were required for fractures occurring at the site as the initial fracture. The analysis focused on the prevalence of subsequent fractures and the costs between the two cohorts. A total 48,742 Medicare patients were included in the analysis with at least one non-vertebral fracture.<br/>
                       
                       •	Freemantle N, Cooper C, Diez-Perez A, Gitlin M, Radcliffe H, Shepherd S, et al. Results of indirect and mixed treatment comparison of fracture efficacy for osteoporosis treatments: a meta-analysis. Osteoporos Int. 2013;24(1):209–17. <br/>
                       o	A secondary analysis of the literature with the aim of estimating the relative efficacy of osteoporosis treatment among postmenopausal women. The analysis used multiple techniques including meta-analysis, adjusted indirect comparison, and mixed treatment comparison [MTC]) to provide comparisons of the relative efficacy of postmenopausal osteoporosis therapies in the absence of comprehensive head-to-head trials. Evidence was identified from MEDLINE; EMBASE; Cochrane Central Register of Controlled Trials (CENTRAL) via Wiley Interscience; and Cumulative Index to Nursing and Allied Health Literature (CINAHL) between April 28, 2009 and November 4, 2009. The analyses included data from trials with fracture endpoints, including morphometric vertebral, clinical vertebral, non-vertebral, hip and wrist. A total of 33 studies were included in the final analysis and includes denosumab, strontium, raloxifene, teriparatide, zoledronic acid, alendronate, risedronate, etidronate, and ibandronate. <br/>
                       
                       •	United States Department of Labor, Bureau of Labor Statistics. Medicare Care CPI (Consumer Price Index) Data Tables. 2019. Available from: https://data.bls.gov/pdq/SurveyOutputServlet<br/> 
                       o	A public database of CPI over time. The database provides national averages for medical care in the US for all urban consumers. These estimates are not seasonally adjusted. The medical care index is one of the eight major groups in the Consumer Price Index (CPI) and is divided into two main components: medical care services and medical care commodities, each containing several item categories. Medical care services, the larger component in terms of weight in the CPI, is organized into three categories: professional services, hospital and related services, and health insurance. Medical care commodities, the other major component, includes medicinal drugs and medical equipment and supplies.<br/>
                       
                       •	Durden E, Pinto L, Lopez-Gonzalez L, Juneau P, Barron R. Two-year persistence and compliance with osteoporosis therapies among postmenopausal women in a commercially insured population in the United States. Arch Osteoporos. 2017;12:22.<br/>
                       o	A retrospective claims-based analysis was conducted in the Medicare and Commercial MarketScan databases to examine the 12 and 24-month persistence and compliance to osteoporosis therapies.  Patients enrolled were women ≥ 50 years old newly initiating osteoporosis therapy between January 1, 2012 to and December 31, 2012 and had continuous insurance eligibility for 14 months before and 24 months after their initial prescription. Persistence was defined as the absence of a > 60 day gap in continuous therapy. Therapies included in the analysis were injectable (denosumab, ibandronate,  teriparatide, zoledronic acid annual IV) and oral (alendronate, alendronate, ibandronate, raloxifene, risedronate) of varying dose frequencies. A total of 43,543 patients across Medicare and Commercial population were included and evaluated for persistence at 12 and 24 months of follow-up. <br/>
                       
                       •	Leader Jr. D, Williams SA, Curtis JR, Gut R. Osteoporosis-Related Fracture Events in the U.S (M19). AMCP Nexus; 2017 Oct 16; Dallas, TX, USA. S78.<br/>
                       o	A retrospective analysis of 2015 Medicare claims data using the Medicare Standard Analytical File (SAF). In addition, commercial insurance estimates were built using the following sources of data: Medicare SAF Inpatient Claims, Health Care Cost Utilization Project, CMS Hospital, U.S. Census Bureau demographics by ZIP code, and commercial claims from several states. Fractures were estimated for  hip, clinical vertebral, wrist, and other skeletal sites associated with fractures that commonly occur in patients with osteoporosis. More than 2 million fractured in 2015, with women accounting for 70% of the fractures. <br/>
                       
                       •	King AB, Saag KG, Burge RT, Pisu M, Goel N. Fracture Reduction Affects Medicare Economics (FRAME): Impact of increased osteoporosis diagnosis and treatment. Osteoporos Int. 2005;16(12):1545–57.<br/>
                       o	An economic model used to estimate the impact of increased diagnosis and treatment. The model was a Markov structure and was used to predict fracture incidence and costs among postmenopausal women ≥ 65 years old over a 3 year period (2001-2003). The primary outputs of the model were focused on estimating the budget impact of testing an additional 1 million women.  As part of the model, secondary evidence was used as inputs with a key input focused on the percentage of patients treated following a bone mineral density test. The rate of treatment was based on data from Medicare supplemental and retiree plans, specifically the MarketScan claims database. <br/>
                       
                       •	IBM Micromedex RED BOOK 2019.<br/>
                       o	A subscription-based database summarizing various drug acquisition list prices by National Drug Code (NDC). The database is updated regularly providing different types of list prices including commonly cited prices such as wholesale acquisition cost (WAC) and average wholesale price (AWP). <br/>
                       
                       •	Center for Medicare and Medicaid Services (CMS) Physician Fee Schedule Current Procedural Terminology (CPT) payment rates. Access on March 29, 2019. https://www.cms.gov/apps/physician-fee-schedule/license-agreement.aspx <br/>
                       o	A public database providing payment rate information from a Medicare perspective for physician services by CPT code. Payment rates are provided by state and nationally as well as breakdowns by facility and non-facility rates, including modifiers.<br/>
                       
                       •	Pike C, Birnbaum HG, Schiller M, Sharma H, Burge R, Edgell ET. Direct and indirect costs of non-vertebral fracture patients with osteoporosis in the US. PharmacoEconomics. 2010;28(5):395–409.<br/>
                       o	A retrospective analysis of administrative claims from 1999 through 2006 of  privately-insured (ages 18–64 years) and Medicare (ages 65+ years) patients with ≥1 osteoporosis-related non-vertebral fracture were matched to osteoporosis controls without a fracture based on age, sex, employment status, and geographic location. The analysis focused on the healthcare resource use and cost stratified by perspective. For primary-insured patients only, indirect costs were estimated based on work loss. All costs were reported in 2006 US dollars and all costs are reported as incremental costs.  A total 48,742 Medicare patients and 4,764 privately-insured patients were included in the analysis with at least one non-vertebral fracture. Indirect costs were estimated among a subset of the population (n = 1,148) using disability data including employer payments for short- and long-term disability and days related to absenteeism separate from the disability days using employee’s daily wages. <br/>
                       
                       •	Vanness DJ, Tosteson ANA. Estimating the Opportunity Costs of Osteoporosis in the United States: Top Geriatr Rehabil. 2005;21(1):4–16. <br/>
                       o	An observational study to estimate the economic consequences of osteoporosis, specifically opportunity cost in the United States. Opportunity costs count not only medical costs of treating osteoporotic fractures but also costs of screening and prevention, informal caregiving and osteoporosis-related research and development. Using evidence from other literature, specifically a study in Canada found that on average ~3 hours of additional informal caregiving time per week per patient were incurred. Using a human capital approach for estimating resource use, the average wage in 2002 was $9.16 per hour of caregiving time. All costs estimates are presented in 2004 US Dollars.  <br/>
                      "))
              )
            ),
            
            tags$img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Amgen.svg/1280px-Amgen.svg.png", width = "100px", height = "30px", style="display: block; margin-left: auto; margin-right: auto;")
            
      ),
    tabItem(tabName = "Terms",
            fluidRow(
              width = NULL, background = "black"),
            h3("Bending the Curve: The Estimated Long-Term Value of Improving Patient Identification and Treatment in Post-Menopausal Osteoporosis", align = "center"),
            fluidRow(
              boxPlus(
                width = 12,
                title = "Abbreviations",
                closable = FALSE, status = "warning", solidHeader = TRUE, collapsible = TRUE,
                fluidRow(
                column(width = 4, tags$b("ABBREVIATED FORM")),
                column(width = 8, tags$b("FULL FORM"))),
                fluidRow(column(width = 4, tags$ul(
                  lapply(1:nrow(abbreviations), function(x) {
                    return(tags$li(abbreviations$abbrev[x]))
                                      })
              )),
              column(width = 8, tags$ul(
                lapply(1:nrow(abbreviations), function(x) {
                  return(tags$li(abbreviations$full[x]))
                })
              )))
              )
              ),
            
            tags$img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Amgen.svg/1280px-Amgen.svg.png", width = "100px", height = "30px", style="display: block; margin-left: auto; margin-right: auto;")            ),
    tabItem(tabName = "References",
            fluidRow(
              width = NULL, background = "black"),
            h3("Bending the Curve: The Estimated Long-Term Value of Improving Patient Identification and Treatment in Post-Menopausal Osteoporosis", align = "center"),
            fluidRow(
              boxPlus(
                width = 12,
                title = "References",
                closable = FALSE, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
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