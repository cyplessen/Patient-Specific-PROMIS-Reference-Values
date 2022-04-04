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


print(dir())
print(dir("data"))
print(ls())

countries <- c("Germany", "United Kingdom", "United States")

flags <- c(
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/de.svg",
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/gb.svg",
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/us.svg")


ui <- fluidPage(
  pageWithSidebar(
    
    # Application title
    headerPanel("Test app"),
    
    sidebarPanel(
      tags$h3("Country"),
      radioButtons("country", 
                   label = "Select country", 
                   choiceNames = mapply(countries, 
                                        flags, 
                                        FUN = function(country, flagUrl) {
                                          tagList(
                                            tags$img(src=flagUrl, 
                                                     width=20, 
                                                     height=15),
                                            country
                                          )
                                        }, SIMPLIFY = FALSE, USE.NAMES = FALSE),
                   choiceValues = c("country0", "country1", "country2"),
                   selected = "country1"),
      tags$h3("Age"),
      sliderInput("age",
                  label = "Select age",
                  min = 50,
                  max = 100, 
                  value = 65),
      
      tags$h3("Sex"),
      selectInput('sex', 
                  'Select sex:', 
                  c("Male" = 0, 
                    "Female" = 1)),
      
      tags$h3("PROMIS Physical Functioning"),
      numericInput("tscore_pf", 
                   "Physical Functioning T-Score",
                   value = 50, min = 1, max = 100),
      
      tags$h3("PROMIS Upper Extremities"),
      numericInput("tscore_ue", 
                   "PROMIS Upper Extremities T-Score",
                   value = 50, min = 1, max = 100),
      
      tags$h3("PROMIS Pain Interference"),
      numericInput("tscore_pi", 
                   "PROMIS Pain Interference T-Score",
                   value = 50, min = 1, max = 100)),
    
    mainPanel(
      # Keep this for trouble shooting on selecting input
      # fluidRow(
      #   column(width = 6,
      #          textOutput("country_choice")), 
      #   column(width = 6, 
      #          textOutput("age_choice"))),
      # textOutput("sex_choice"),
      # textOutput("pf_choice"),
      # textOutput("ue_choice"),
      
      tabsetPanel(
        tabPanel("Plots",
                 tags$h3("PROMIS Physical Functioning"),
                 fluidRow(
                   column(width = 10, plotOutput("plot_pf"))),
                 
                 tags$h3("PROMIS Upper Extremities"),
                 fluidRow(
                   column(width = 10, plotOutput("plot_ue"))),
                 
                 tags$h3("PROMIS Pain Interference"),
                 fluidRow(
                   column(width = 10, plotOutput("plot_pi"))),
                 
        ),
        tabPanel( "Tables",
                  tags$h3("PROMIS Physical Functioning"),
                  
                  fluidRow(
                    column(width = 10, tableOutput("table_pf"))),
                  tags$h3("PROMIS Upper Extremities"),
                  
                  fluidRow(
                    column(width = 10, tableOutput("table_ue"))),
                  
                  tags$h3("PROMIS Pain Interference"),
                  
                  fluidRow(
                    column(width = 10, tableOutput("table_pi")))
        )
      )
    )
  )
)
