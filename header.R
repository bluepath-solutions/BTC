source("global.R")
getHeader <- function(){
  dashboardHeaderPlus(
    uiOutput("logoutButton"),
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
  )
}

