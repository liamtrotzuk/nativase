################################################################################
# entry point of the Shiny app
#
# Author: Liam Trotzuk
# Created: 2020-10-10 
################################################################################

readRenviron(".Renviron")

### PACKAGES ###
# import libraries
library(shiny)
library(shinybusy)
library(shinyjs)
library(plotly)
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(soilDB)
library(rlang)
library(rvest)
library(xml2)
library(maps)
library(tigris)
library(rmapshaper)
library(leaflet)
library(sf)
library(htmlwidgets)
library(sparkline)
library(future)
library(glue)
library(highcharter)
library(lubridate)
library(purrr)
library(visNetwork)
library(shinyWidgets)
library(googleCloudStorageR)

### MODULES ###
source("modules/module_calendar_pre.R")
source("modules/module_calendar_main.R")
source("modules/module_map_pre.R")
source("modules/module_map_main.R")
source("modules/module_plots_pre.R")
source("modules/module_plots_main.R")

### JAVASCRIPT ###
shiny::addResourcePath("shinyjs",system.file("srcjs", package = "shinyjs"))
