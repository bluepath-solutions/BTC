source("header.R")
source("sidebar.R")
source("body.R")
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
    header = getHeader(),
    sidebar = getSidebar(),
    body = getBody()
                ))