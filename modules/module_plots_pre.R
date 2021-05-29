################################################################################
# module calendar_pre
#
# Author: Liam Trotzuk
# Created: 2020-10-20
################################################################################

# Module UI ---------------------------------------------------------------

plots_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("module_ui"))
}

# Module server logic -----------------------------------------------------

plots_pre <- function(input,output,session) {
  
  gcs_load("initial_PLOT_precip.rda",bucket="adj_outputs")
  gcs_load("initial_PLOT_tempmin.rda",bucket="adj_outputs")
  
  REACT_plot_precip <- reactive(renderPlotly(initial_PLOT_precip))
  REACT_plot_temp <- reactive(renderPlotly(initial_PLOT_tempmin))
  
  output$module_ui <- renderUI({
    
    htmlTemplate(
      filename = "www/modules/plots/index.html",
      precip_plot_for_index_HTML = REACT_plot_precip(),
      temp_plot_for_index_HTML = REACT_plot_temp()
    )
  })
  
}