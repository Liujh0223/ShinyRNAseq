observeEvent(input$ui_datacheck,{
  updateTabsetPanel(session = session, inputId = "navbar", selected = "datacheck")
})
observeEvent(input$ui_differenceanalysis,{
  updateTabsetPanel(session = session, inputId = "navbar", selected = "differenceanalysis")
})
observeEvent(input$ui_enrichmentanallysis,{
  updateTabsetPanel(session = session, inputId = "navbar", selected = "enrichmentanalysis")
})
observeEvent(input$ui_tools,{
  updateTabsetPanel(session = session, inputId = "navbar", selected = "tools")
})
observeEvent(input$ui_timeseriesanalysis,{
  updateTabsetPanel(session = session, inputId = "navbar", selected = "timeseriesanalysis")
})
observeEvent(input$ui_wgcna,{
  updateTabsetPanel(session = session, inputId = "navbar", selected = "wgcna")
})
