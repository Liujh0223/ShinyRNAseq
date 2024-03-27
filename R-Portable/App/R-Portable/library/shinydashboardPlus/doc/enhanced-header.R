## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----leftUi-code, eval=FALSE--------------------------------------------------
#  library(shiny)
#  library(shinyWidgets)
#  library(shinydashboard)
#  library(shinydashboardPlus)
#   shinyApp(
#     ui = dashboardPage(
#       header = dashboardHeader(
#         leftUi = tagList(
#           dropdownButton(
#             label = "Controls",
#             icon = icon("sliders"),
#             status = "primary",
#             circle = FALSE,
#             sliderInput(
#               inputId = "n",
#               label = "Number of observations",
#               min = 10, max = 100, value = 30
#             ),
#             prettyToggle(
#               inputId = "na",
#               label_on = "NAs kept",
#               label_off = "NAs removed",
#               icon_on = icon("check"),
#               icon_off = icon("remove")
#             )
#           ),
#           dropdownMenu(
#             type = "messages",
#             badgeStatus = "success",
#             messageItem(from = "Support Team", message = "This is the content of a message.", time = "5 mins"),
#             messageItem(from = "Support Team", message = "This is the content of another message.", time = "2 hours"),
#             messageItem(from = "New User", message = "Can I get some help?", time = "Today")
#           )
#         )
#       ),
#       sidebar = dashboardSidebar(),
#       body = dashboardBody(
#         setShadow(class = "dropdown-menu")
#       ),
#       title = "DashboardPage"
#     ),
#     server = function(input, output) { }
#   )

## ----dropdownBlock-code, eval=FALSE-------------------------------------------
#  shinyApp(
#    ui = dashboardPage(
#      header = dashboardHeader(
#        leftUi = tagList(
#          dropdownBlock(
#            id = "mydropdown",
#            title = "Dropdown 1",
#            icon = "sliders",
#            sliderInput(
#              inputId = "n",
#              label = "Number of observations",
#              min = 10, max = 100, value = 30
#            ),
#            prettyToggle(
#              inputId = "na",
#              label_on = "NAs kept",
#              label_off = "NAs removed",
#              icon_on = icon("check"),
#              icon_off = icon("remove")
#            )
#          ),
#          dropdownBlock(
#            id = "mydropdown2",
#            title = "Dropdown 2",
#            icon = "sliders",
#            prettySwitch(
#              inputId = "switch4",
#              label = "Fill switch with status:",
#              fill = TRUE,
#              status = "primary"
#            ),
#            prettyCheckboxGroup(
#              inputId = "checkgroup2",
#              label = "Click me!",
#              thick = TRUE,
#              choices = c("Click me !", "Me !", "Or me !"),
#              animation = "pulse",
#              status = "info"
#            )
#          )
#        )
#      ),
#      sidebar = dashboardSidebar(),
#      body = dashboardBody(
#        setShadow(class = "dropdown-menu")
#      ),
#      title = "DashboardPage"
#    ),
#    server = function(input, output) { }
#  )

## ----dashboardUser-code, eval=FALSE-------------------------------------------
#  shinyApp(
#    ui = dashboardPage(
#      header = dashboardHeader(userOutput("user")),
#      sidebar = dashboardSidebar(),
#      body = dashboardBody(),
#      title = "User dropdown"
#    ),
#    server = function(input, output) {
#     output$user <- renderUser({
#      dashboardUser(
#         name = "Divad Nojnarg",
#         image = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg",
#         title = "shinydashboardPlus",
#         subtitle = "Author",
#         footer = p("The footer", class = "text-center"),
#         fluidRow(
#          dashboardUserItem(
#           width = 6,
#           socialButton(
#            href = "https://dropbox.com",
#            icon = icon("dropbox")
#           )
#          ),
#          dashboardUserItem(
#           width = 6,
#           socialButton(
#            href = "https://github.com",
#            icon = icon("github")
#           )
#          )
#         )
#        )
#     })
#    }
#   )

