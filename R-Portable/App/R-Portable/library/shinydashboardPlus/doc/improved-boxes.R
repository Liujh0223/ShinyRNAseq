## ----boxAPI-code, eval=FALSE--------------------------------------------------
#  library(shiny)
#  library(shinydashboard)
#  library(shinydashboardPlus)
#  ui <- dashboardPage(
#    title = "Box API",
#    dashboardHeader(),
#    dashboardSidebar(),
#    dashboardBody(
#      tags$style("body { background-color: ghostwhite}"),
#      fluidRow(
#        actionButton("toggle_box", "Toggle Box"),
#        actionButton("remove_box", "Remove Box", class = "bg-danger"),
#        actionButton("restore_box", "Restore Box", class = "bg-success"),
#        actionButton("update_box", "Update Box", class = "bg-primary")
#      ),
#      br(),
#      box(
#        title = textOutput("box_state"),
#        "Box body",
#        id = "mybox",
#        collapsible = TRUE,
#        closable = TRUE,
#        plotOutput("plot")
#      )
#    )
#  )
#  
#  server <- function(input, output, session) {
#    output$plot <- renderPlot({
#      req(!input$mybox$collapsed)
#      plot(rnorm(200))
#    })
#  
#    output$box_state <- renderText({
#      state <- if (input$mybox$collapsed) "collapsed" else "uncollapsed"
#      paste("My box is", state)
#    })
#  
#    observeEvent(input$toggle_box, {
#      updateBox("mybox", action = "toggle")
#    })
#  
#    observeEvent(input$remove_box, {
#      updateBox("mybox", action = "remove")
#    })
#  
#    observeEvent(input$restore_box, {
#      updateBox("mybox", action = "restore")
#    })
#  
#    observeEvent(input$update_box, {
#      updateBox(
#        "mybox",
#        action = "update",
#        options = list(
#          title = h2("New title", dashboardLabel(1, status = "primary")),
#          status = "danger",
#          solidHeader = TRUE,
#          width = 4
#        )
#      )
#    })
#  
#    observeEvent(input$mybox$visible, {
#      collapsed <- if (input$mybox$collapsed) "collapsed" else "uncollapsed"
#      visible <- if (input$mybox$visible) "visible" else "hidden"
#      message <- paste("My box is", collapsed, "and", visible)
#      showNotification(message, type = "warning", duration = 1)
#    })
#  }
#  
#  shinyApp(ui, server)

## ----boxTools, echo=FALSE, fig.cap='Box Tools. DFrom left to right: boxLabel, boxDropdown, collapsible and closable buttons, boxSidebar trigger.', fig.align = 'center', out.width='50%'----
knitr::include_graphics("figures/boxTools.png")

## ----boxSidebar-code, eval=FALSE----------------------------------------------
#  shinyApp(
#    ui = dashboardPage(
#      header = dashboardHeader(),
#      body = dashboardBody(
#        box(
#          title = "Update box sidebar",
#          closable = TRUE,
#          width = 12,
#          height = "500px",
#          solidHeader = FALSE,
#          collapsible = TRUE,
#          actionButton("update", "Toggle card sidebar"),
#          sidebar = boxSidebar(
#            id = "mycardsidebar",
#            width = 25,
#            sliderInput(
#              "obs",
#              "Number of observations:",
#              min = 0,
#              max = 1000,
#              value = 500
#            )
#          ),
#          plotOutput("distPlot")
#        )
#      ),
#      sidebar = dashboardSidebar()
#    ),
#    server = function(input, output, session) {
#      observe(print(input$mycardsidebar))
#  
#      output$distPlot <- renderPlot({
#        hist(rnorm(input$obs))
#      })
#  
#      observeEvent(input$update, {
#        updateBoxSidebar("mycardsidebar")
#      })
#  
#    }
#  )

## ----boxDropdown-code, eval=FALSE---------------------------------------------
#  shinyApp(
#    ui = dashboardPage(
#      dashboardHeader(),
#      dashboardSidebar(),
#      dashboardBody(
#        box(
#          title = "Closable Box with dropdown",
#          closable = TRUE,
#          width = 12,
#          status = "warning",
#          solidHeader = FALSE,
#          collapsible = TRUE,
#          dropdownMenu = boxDropdown(
#            boxDropdownItem("Click me", id = "dropdownItem", icon = icon("heart")),
#            boxDropdownItem("item 2", href = "https://www.google.com/"),
#            dropdownDivider(),
#            boxDropdownItem("item 3", icon = icon("th"))
#          ),
#          "My box"
#        )
#      )
#    ),
#    server = function(input, output) {
#      observeEvent(input$dropdownItem, {
#        showNotification("Hello", duration = 1, type = "message")
#      })
#    }
#  )

## ----user-description, eval=FALSE---------------------------------------------
#  userDescription(
#    title = "Nadia Carmichael",
#    subtitle = "lead Developer",
#    type = 2,
#    image = "https://adminlte.io/themes/AdminLTE/dist/img/user7-128x128.jpg",
#  )

## ----userBox, echo=FALSE, fig.cap='Some userBox components', fig.align = 'center', out.width='100%'----
knitr::include_graphics("figures/userBox.png")

## ----userBox-code, eval=FALSE-------------------------------------------------
#  shinyApp(
#    ui = dashboardPage(
#      dashboardHeader(),
#      dashboardSidebar(),
#      dashboardBody(
#        userBox(
#          title = userDescription(
#            title = "Nadia Carmichael",
#            subtitle = "lead Developer",
#            type = 2,
#            image = "https://adminlte.io/themes/AdminLTE/dist/img/user7-128x128.jpg",
#          ),
#          status = "warning",
#          navPills(
#            id = "pillItem",
#            navPillsItem(
#              left = "Item 1",
#              color = "green",
#              right = 10
#            ),
#            navPillsItem(
#              left = "Item 2",
#              color = "red",
#              icon = icon("angle-down"),
#              right = "10%"
#            )
#          ),
#          footer = "The footer here!"
#        )
#      ),
#      title = "userBox"
#    ),
#    server = function(input, output) {
#  
#      observeEvent(input$pillItem, {
#        if (input$pillItem == 2) {
#          showModal(
#            modalDialog("A modal")
#          )
#        }
#      })
#  
#      observeEvent(input$pillItem, {
#        showNotification(
#          sprintf("You clicked on pill N° %s", input$pillItem),
#          type = "warning",
#          duration = 1
#        )
#      })
#    }
#  )

## ----update-userBox-code, eval=FALSE------------------------------------------
#  shinyApp(
#    ui = dashboardPage(
#      dashboardHeader(),
#      dashboardSidebar(),
#      dashboardBody(
#        actionButton("update_box", "Update"),
#        userBox(
#          id = "userbox",
#          title = userDescription(
#            title = "Nadia Carmichael",
#            subtitle = "lead Developer",
#            type = 2,
#            image = "https://adminlte.io/themes/AdminLTE/dist/img/user7-128x128.jpg",
#          ),
#          status = "primary",
#          gradient = TRUE,
#          background = "light-blue",
#          boxToolSize = "xl",
#          "Some text here!",
#          footer = "The footer here!"
#        )
#      ),
#      title = "userBox"
#    ),
#    server = function(input, output) {
#      observeEvent(input$update_box, {
#        updateBox(
#          "userbox",
#          action = "update",
#          options = list(
#            title = userDescription(
#              title = "Jean Box",
#              subtitle = "Developer",
#              type = 1,
#              image = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
#            ),
#            status = "red",
#            background = NULL,
#            width = 4
#          )
#        )
#      })
#    }
#  )

## ----user-block, eval=FALSE---------------------------------------------------
#  userBlock(
#    image = "https://adminlte.io/themes/AdminLTE/dist/img/user4-128x128.jpg",
#    title = "Social Box",
#    subtitle = "example-01.05.2018"
#  )

## ---- eval=FALSE--------------------------------------------------------------
#  shinyApp(
#    ui = dashboardPage(
#      dashboardHeader(),
#      dashboardSidebar(),
#      dashboardBody(
#        socialBox(
#          id = "socialbox",
#          title = userBlock(
#            image = "https://adminlte.io/themes/AdminLTE/dist/img/user4-128x128.jpg",
#            title = "Social Box",
#            subtitle = "example-01.05.2018"
#          ),
#          actionButton("update_box", "Refresh"),
#          "Some text here!",
#          br(),
#          tabsetPanel(
#            tabPanel(
#              "News",
#              attachmentBlock(
#                image = "https://www.sammobile.com/wp-content/uploads/2017/11/Camel.png",
#                title = "Test",
#                href = "http://google.com",
#                "This is the content"
#              )
#            ),
#            tabPanel(
#              "Messages",
#              userMessages(
#                width = 12,
#                status = "danger",
#                userMessage(
#                  author = "Alexander Pierce",
#                  date = "20 Jan 2:00 pm",
#                  image = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
#                  type = "received",
#                  "Is this template really for free? That's unbelievable!"
#                ),
#                userMessage(
#                  author = "Sarah Bullock",
#                  date = "23 Jan 2:05 pm",
#                  image = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
#                  type = "sent",
#                  "You better believe it!"
#                )
#              )
#            )
#          ),
#          lapply(X = 1:10, FUN = function(i) {
#            boxComment(
#              image = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
#              title = paste("Comment", i),
#              date = "01.05.2018",
#              paste0("The ", i, "-th comment")
#            )
#          }),
#          footer = "The footer here!"
#        )
#      ),
#      title = "Social Box"
#    ),
#    server = function(input, output) {
#      observeEvent(input$update_box, {
#        updateBox(
#          "socialbox",
#          action = "update",
#          options = list(
#            title = userBlock(
#              image = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
#              title = "Social Box updated",
#              subtitle = "today"
#            )
#          )
#        )
#      })
#    }
#  )

## ---- eval=FALSE--------------------------------------------------------------
#  shinyApp(
#    ui = dashboardPage(
#      dashboardHeader(),
#      dashboardSidebar(),
#      dashboardBody(
#        fluidRow(
#          column(
#            width = 6,
#            uiOutput("active_side"),
#            actionButton("toggle", "Toggle flip box"),
#            flipBox(
#              id = "myflipbox",
#              trigger = "hover",
#              width = 12,
#              front = div(
#                class = "text-center",
#                h1("Flip on hover"),
#                img(
#                  src = "https://image.flaticon.com/icons/svg/149/149076.svg",
#                  height = "300px",
#                  width = "100%"
#                )
#              ),
#              back = div(
#                class = "text-center",
#                height = "300px",
#                width = "100%",
#                h1("Flip on hover"),
#                p("More information....")
#              )
#            )
#          ),
#          column(
#            width = 6,
#            uiOutput("active_side_2"),
#            flipBox(
#              id = "myflipbox2",
#              width = 12,
#              front = div(
#                class = "text-center",
#                h1("Flip on click"),
#                img(
#                  src = "https://image.flaticon.com/icons/svg/149/149076.svg",
#                  height = "300px",
#                  width = "100%"
#                )
#              ),
#              back = div(
#                class = "text-center",
#                height = "300px",
#                width = "100%",
#                h1("Flip on click"),
#                p("More information....")
#              )
#            )
#          )
#        )
#      )
#    ),
#  
#    server = function(input, output, session) {
#      output$active_side <- renderUI({
#        side <- if (input$myflipbox) "front" else "back"
#        dashboardBadge(side, color = "blue")
#      })
#  
#      output$active_side_2<- renderUI({
#        side <- if (input$myflipbox2) "front" else "back"
#        dashboardBadge(side, color = "blue")
#      })
#  
#      observeEvent(input$toggle, {
#        updateFlipBox("myflipbox")
#      })
#    }
#  )

