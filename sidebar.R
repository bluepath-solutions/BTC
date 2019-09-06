source("global.R")
getSidebar <- function(){
  
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
  )  
  
}