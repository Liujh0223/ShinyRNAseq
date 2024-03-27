## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----skinSelector-code, eval=FALSE--------------------------------------------
#  library(shiny)
#  library(shinydashboard)
#  library(shinydashboardPlus)
#  shinyApp(
#     ui = dashboardPage(
#       header = dashboardHeader(),
#       sidebar = dashboardSidebar(),
#       body = dashboardBody(),
#       controlbar = dashboardControlbar(collapsed = FALSE, skinSelector()),
#       title = "Skin Selector"
#     ),
#     server = function(input, output) { }
#   )

## ----midnight-code, eval=FALSE------------------------------------------------
#  shinyApp(
#     ui = dashboardPage(
#       skin = "midnight",
#       header = dashboardHeader(),
#       sidebar = dashboardSidebar(),
#       body = dashboardBody(),
#       controlbar = dashboardControlbar(),
#       footer = dashboardFooter(),
#       title = "Midnight Skin"
#     ),
#     server = function(input, output) { }
#   )

## ----midnight-skin, echo=FALSE, fig.cap='Midnight skin overview', fig.align = 'center', out.width='100%'----
knitr::include_graphics("figures/skin-midnight-overview.png")

## ----md-code, eval=FALSE------------------------------------------------------
#  shinyApp(
#     ui = dashboardPage(
#       md = TRUE,
#       skin = "blue",
#       header = dashboardHeader(),
#       sidebar = dashboardSidebar(),
#       body = dashboardBody(),
#       controlbar = dashboardControlbar(),
#       footer = dashboardFooter(),
#       title = "Midnight Skin"
#     ),
#     server = function(input, output) { }
#   )

## ----fresh-code, eval=FALSE---------------------------------------------------
#  library(fresh)
#  # create the theme with a cyberpunk color palette
#  theme <- create_theme(
#    adminlte_color(
#      green = "#3fff2d",
#      blue = "#2635ff",
#      red = "	#ff2b2b",
#      yellow = "#feff6e",
#      fuchsia = "#ff5bf8",
#      navy = "#374c92",
#      purple = "#615cbf",
#      maroon = "#b659c9",
#      light_blue = "#5691cc"
#    ),
#    adminlte_sidebar(
#      dark_bg = "#D8DEE9",
#      dark_hover_bg = "#81A1C1",
#      dark_color = "#2E3440"
#    ),
#    adminlte_global(
#      content_bg = "#aaaaaa"
#    )
#  )
#  
#  # create tribble for box global config
#  box_config <- tibble::tribble(
#    ~background, ~labelStatus,
#    "red", "warning",
#    "purple", "success",
#    "green", "primary",
#    "yellow", "danger",
#    "fuchsia", "info"
#  )
#  
#  # box factory function
#  box_factory <- function(background, labelStatus) {
#    box(
#      title = "Cyberpunk Box",
#      collapsible = TRUE,
#      background = background,
#      height = "200px",
#      label = boxLabel(1, labelStatus)
#    )
#  }
#  
#  # pmap magic
#  boxes <- purrr::pmap(box_config, box_factory)
#  
#  shinyApp(
#     ui = dashboardPage(
#       freshTheme = theme,
#       skin = "blue",
#       options = list(sidebarExpandOnHover = TRUE),
#       header = dashboardHeader(
#         dropdownMenu(
#           type = "messages",
#           badgeStatus = "success",
#           messageItem(
#             from = "Support Team",
#             message = "This is the content of a message.",
#             time = "5 mins"
#           ),
#           messageItem(
#             from = "Support Team",
#             message = "This is the content of another message.",
#             time = "2 hours"
#           )
#         )
#       ),
#       sidebar = dashboardSidebar(
#         sidebarMenu(
#           menuItem("Item 1", badgeLabel = icon("heart"), badgeColor = "light-blue"),
#           menuItem("Item 2", badgeLabel = icon("poo"), badgeColor = "maroon")
#         )
#       ),
#       body = dashboardBody(boxes),
#       controlbar = dashboardControlbar(),
#       title = "Fresh theming"
#     ),
#     server = function(input, output) { }
#   )

