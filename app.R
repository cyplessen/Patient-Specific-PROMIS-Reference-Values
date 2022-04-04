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

source("ui.R")
source("server.R")

load("data/plotdata_pf.RData") 
load("data/plotdata_ue.RData") 
load("data/plotdata_pi.RData") 

# Run shiny app
runApp(shinyApp(ui, server))

