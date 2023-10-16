# Loading packages and data
library(dplyr)
library(tidyr)
library(shiny)
#devtools::install_github("joelkuiper/personograph")
library(personograph) # needs to be loaded for dependencies
source("personograph_package.R")

#NCmisc::list.functions.in.file("ui.R")
#NCmisc::list.functions.in.file("server.R")
#
#print(dir())
#print(dir("data"))
#print(ls())

countries <- c("Germany", "United Kingdom", "United States")

flags <- c(
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/de.svg",
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/gb.svg",
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/us.svg")


ui <- fluidPage(
  pageWithSidebar(
    
    # Application title
    #headerPanel("PROMIS Physical Function, Upper Extremity and Pain Interference Reference Scores for Adults (50+)"),
    headerPanel(""),
    
    sidebarPanel(
      tags$h3("1. Country"),
      radioButtons("country", 
                   label = "Select the patient`s country of residence:", 
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
      #tags$br(),
      tags$h3("2. Age"),
      sliderInput("age",
                  label = "Select the patient`s age (between 50 and 100 years):",
                  min = 50,
                  max = 100, 
                  value = 65),
     # tags$br(),
      tags$h3("3. Sex"),
      selectInput('sex', 
                  'Select the patient`s sex:', 
                  c("Male" = 0, 
                    "Female" = 1)),
      #tags$br(),
      tags$h3("4. PROMIS Measures"),
      helpText("Note:",
               "Input the patient`s", em("T-score"), "for the relevant PROMIS measure."),
     # tags$br(),
      numericInput("tscore_pf", 
                   label = shiny::div(HTML("PROMIS Physical Functioning <em>T</em>-Score")),
                   value = 50, min = 1, max = 100),
      
      numericInput("tscore_ue", 
                   label = shiny::div(HTML("PROMIS Upper Extremities <em>T</em>-Score")),
                   value = 50, min = 1, max = 100),
      
      numericInput("tscore_pi", 
                   label = shiny::div(HTML("PROMIS Pain Interference <em>T</em>-Score")),
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
        tabPanel("About",
                 tags$h3("About this Shiny App"),
                 fluidRow(
                   column(width = 10,
                          tags$br(),
                          "This web application can be used to obtain patient-specific reference values for",
                          "PROMIS Physical Functioning, ",
                          "Upper Extremity, and Pain Interference item banks in general populations from the USA, UK, and Germany.",
                          "By entering information about your patient you can obtain 1) plots to discuss their Physical Functioning, Upper Extremities, and Pain interference test results and",
                          "2) obtain patient-specific reference values. These values can help identifying if the obtained test results are higher, similar, or lower compared to patient specific reference group",
                          "i.e. with the same age, sex, and from the same country.",
                          tags$br(),
                          tags$br(),
                          "It is based on the publication",
                          tags$em( "Age-, sex-, and country-specific reference values of PROMIS Physical Functioning, Upper Extremity, and Pain Interference item banks were established in general populations from the USA, UK, and Germany"),
                          
                          "by Plessen et al. (2022, submitted).")),
                 
                 tags$h3("How To Use This App"),
                 fluidRow(
                   column(width = 10, 
                          "On the left/above, you can see 4 different sections, in which you can enter information on your patient.",
                          tags$br(),
                          "To obtain patient-specific reference values, you need to enter:", 
                          tags$br(),
                          tags$b("1. Country,"), "information on the country of your patient, currently only Germany, UK, and US are implemented.",
                          tags$br(),
                          tags$b("2. Age,"), "use the slider to select an age from 50 to 100 years.",
                          tags$br(),
                          tags$b("3. Sex,"), "indicate the sex of the patient.",
                          tags$br(),
                          tags$b("4. PROMIS Measures,"), "Optional/to obtain plots, you can input the T-Scores for PROMIS Physical Functioning, Upper Extremities, and Pain Interference.")),
                 
                 tags$h3("Plots"),
                 fluidRow(
                   column(width = 10, "In the Tab", tags$b("Plots"), "you can see so called", tags$em("people plots"), "that indicates how many people would achieve a higher, a similar, or a lower score than your patient",
                          "These plots are based on the T-Scores you entered for PROMIS Physical Functioning, Upper Extremities, or Pain Interference and the information you entered about your patient.")),
                 
                 tags$h3("Tables"),
                 fluidRow(
                   column(width = 10, "In the Tab", tags$b("Tables"), "you can see the reference tables for PROMIS Physical Functioning, Upper Extremities, or Pain Interference based on the information you entered about your patient."))
        ),
        tabPanel("Plots",
                 helpText("Note:",
                          "These plots show the patient´s position compared to a similar population based on age, sex, and country."),
                 tags$h3("PROMIS Physical Functioning (PF)"),
                 fluidRow(
                   column(width = 10, plotOutput("plot_pf"))),
                 
                 tags$h3("PROMIS Upper Extremities (UE)"),
                 fluidRow(
                   column(width = 10, plotOutput("plot_ue"))),
                 
                 tags$h3("PROMIS Pain Interference (PI)"),
                 fluidRow(
                   column(width = 10, plotOutput("plot_pi")))
                 
        ),
        tabPanel( "Tables",
                  tags$br(),
                  helpText("Note:",
                           "These tables show the quantiles from 1% to 99%, ranging from lowest to highest possible score for each domain, based on patients with similar age, sex, and country of residence."),
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


