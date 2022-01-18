# Loading packages and data
library(plyr)
library(tidyverse)
library(shiny)
library(reshape)
library(quantreg)
library(DT)
library(kableExtra)
library(gridExtra)
library(grid)
library(bayestestR) # get perfect nv
library(rriskDistributions) # getting nv from quantiles
library(dplyr)
library(personograph) # needs to be loaded for dependencies
source("personograph_package.R")

countries <- c("Germany", "United Kingdom", "United States")

flags <- c(
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/de.svg",
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/gb.svg",
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/us.svg")

source("ui.R")
source("server.R")

load("data/quantiles_shiny_pooled_pf.RData") # models object for personograph
load("data/quantiles_shiny_pooled_ue.RData") # models object for personograph
load("data/plotdat_pf_sex.RData") # models object (model 5) for plotting and table
load("data/plotdat_ue_sex.RData") # models object (model 5) for plotting and table

# Run shiny app
runApp(shinyApp(ui, server))

