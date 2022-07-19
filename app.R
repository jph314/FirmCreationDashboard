#---
# Shiny App for UK COVID-19 Firm Creation
# Link to project's website:
# Yannis Galanakis; <i.galanakis@kent.ac.uk>
# Jonathan Hobbs; <jph37@kent.ac.uk>
# May 2022
#---

# Libraries
# library(tidyverse)
library(fst)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(DT)
library(shiny)
library(fresh)
library(dashboardthemes)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(plotly)
library(sf)
library(markdown)

# Theme
source("www/UKFirmCreationTheme.R")

# Functions
source("UKFirmCreation_functions3.R")

# Data
source("UKFirmCreation_getData2.R")

# UI
source("UKFirmCreation_ui.R")

# Server
source("UKFirmCreation_server2.R")

# Run the application ----
shinyApp(ui = ui, server = server)
