## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----basic-demo-code, eval=FALSE----------------------------------------------
#  library(shiny)
#  library(shinydashboard)
#  library(shinydashboardPlus)
#  shinyApp(
#     ui = dashboardPage(
#       options = list(sidebarExpandOnHover = TRUE),
#       header = dashboardHeader(),
#       sidebar = dashboardSidebar(minified = TRUE, collapsed = TRUE),
#       body = dashboardBody(
#        lapply(1:20, box, width = 12, title = "box")
#       ),
#       controlbar = dashboardControlbar(),
#       title = "DashboardPage"
#     ),
#     server = function(input, output) { }
#   )

