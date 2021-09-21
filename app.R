#---
# Shiny App for UK COVID-19 Firm Creation
# Link to project's website:
# Yannis Galanakis; <i.galanakis@kent.ac.uk>
# Jonathan Hobbs; <jph37@kent.ac.uk>
# February 2021
#---

# Libraries
# library(tidyverse)
library(lubridate)
library(vroom)
library(dplyr)
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

# Functions
source("functions.R")

# Data
source("getData.R")

# Theme
source("www/UKFirmCreationTheme.R")

# UI
source("ui.R")

# Server
source("server.R")

# Run the application ----
shinyApp(ui = ui, server = server)
