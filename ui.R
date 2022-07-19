# Loading packages and data

library(dplyr)
library(shiny)
library(shinycssloaders)
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
    headerPanel("PROMIS Physical Function, Upper Extremity and Pain Interference Reference Scores for Adults (50+)"),
    
    sidebarPanel(
      tags$h3("1. Country"),
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
      helpText("Note:",
               "This is an example",
               "help text",
               "and or explanation"),
      tags$br(),
      tags$h3("2. Age"),
      sliderInput("age",
                  label = "Select age",
                  min = 50,
                  max = 100, 
                  value = 65),
      tags$br(),
      tags$h3("3. Sex"),
      selectInput('sex', 
                  'Select sex:', 
                  c("Male" = 0, 
                    "Female" = 1)),
      tags$br(),
      tags$h3("4. PROMIS Measures"),
      tags$h4("PROMIS Physical Functioning"),
      numericInput("tscore_pf", 
                   "T-Score",
                   value = 50, min = 1, max = 100),
      
      tags$h4("PROMIS Upper Extremities"),
      numericInput("tscore_ue", 
                   "T-Score",
                   value = 50, min = 1, max = 100),
      
      tags$h4("PROMIS Pain Interference"),
      numericInput("tscore_pi", 
                   " T-Score",
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
                          "In the first panel, ", tags$b("Country,"), "you can enter information on the country of your patient, currently only Germany, UK, and US are implemented.",
                          tags$br(),
                          "In the second panel below, ", tags$b("Age,"), "you can use the slider to select an age from 50 to 100 years.",
                          tags$br(),
                          "In the third panel below, ", tags$b("Sex,"), "you can indicate the sex of the patient.",
                          tags$br(),
                          "In the fourth panel below, ", tags$b("PROMIS Measures,"), ", you can input the T-Scores for PROMIS Physical Functioning, Upper Extremities, and Pain Interference.")),
                 
                 tags$h3("Plots"),
                 fluidRow(
                   column(width = 10, "In the Tab", tags$b("Plots"), "you can see so called people plots that indicates how many people would achieve a higher, a similar, or a lower score than your patient",
                          "These plots are based on the T-Scores you entered for PROMIS Physical Functioning, Upper Extremities, or Pain Interference and the information you entered about your patient.")),
                 
                 tags$h3("Tables"),
                 fluidRow(
                   column(width = 10, "In the Tab", tags$b("Tables"), "you can see the reference tables for PROMIS Physical Functioning, Upper Extremities, or Pain Interference based on the information you entered about your patient."))
        ),
        tabPanel("Plots",
                 helpText("Note:",
                          "This is an example",
                          "help text",
                          "and or explanation"),
                 tags$h3("PROMIS Physical Functioning (PF)"),
                 fluidRow(
                   column(width = 10, plotOutput("plot_pf"))),
                 
                 tags$h3("PROMIS Upper Extremities (UE)"),
                 fluidRow(
                   column(width = 10, plotOutput("plot_ue"))),
                 
                 tags$h3("PROMIS Pain Interference (PI)"),
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
  ) %>% withSpinner(color="#0dc5c1")
)


