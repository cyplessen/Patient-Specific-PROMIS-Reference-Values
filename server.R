#library(tidyverse)
#library(shiny)
#library(reshape)
#library(quantreg)
#library(plyr)
#library(DT)
#library(bayestestR) # get perfect nv
#library(rriskDistributions) # getting nv from quantiles
#source("personograph_package.R")
#load("quantiles_shiny_pooled_pf.RData") # models object

# error: non-numeric argument to binary operator
server <- function(input, output, session) {
  
  output$country_choice <- renderText({
    paste0("You selected ", input$country)
  })
  
  output$age_choice <- renderText({ 
    paste0("You selected ", is.numeric(input$age))
  })
  
  output$sex_choice <- renderText({ 
    paste0("You selected ", is.numeric(input$sex))
    
  })
  
  output$pf_choice <- renderText({ 
    paste0("You selected ", input$tscore_pf)
  })
  
  output$ue_choice <- renderText({ 
    paste0("You selected ", input$tscore_ue)
  })
  
  output$plot_pf <- renderPlot({
    
    plot_personograph(input_age = input$age, 
                      input_sex = input$sex, 
                      input_country = input$country, 
                      input_tscore = input$tscore_pf, 
                      domain_data = quantiles_shiny_pooled_pf)
    
  })
  
  
  output$plot_pf <- renderPlot({
    
    plot_personograph_felix(input_age = input$age, 
                      input_sex = input$sex, 
                      input_country = input$country, 
                      input_tscore = input$tscore_pf, 
                      domain_data = plotdat_pf_sex)
    
  })
  
  output$plot_ue <- renderPlot({
    
    plot_personograph_felix(input_age = input$age, 
                            input_sex = input$sex, 
                            input_country = input$country, 
                            input_tscore = input$tscore_ue, 
                            domain_data = plotdat_ue_sex)
    
  })
  
  
  output$table_pf_sex <- renderTable({
    plotdat_pf_sex %>% 
      dplyr::filter(sex == input$sex & age == input$age & country == ifelse(input$country == "country0", 0, 
                                                  ifelse(input$country == "country1", 1, 
                                                         ifelse(input$country == "country2", 2)))) %>% 
      mutate(value = paste0(round(fit, 2), " [", round(lower, 2), "; ", round(higher, 2), "]")) %>%
      dplyr::select(tau, value) %>% 
      pivot_wider(names_from = tau, values_from = value)  
  })
  
  output$table_ue_sex <- renderTable({
    plotdat_ue_sex %>% 
      dplyr::filter(sex == input$sex & age == input$age & country == ifelse(input$country == "country0", 0, 
                                                                     ifelse(input$country == "country1", 1, 
                                                                            ifelse(input$country == "country2", 2)))) %>% 
      mutate(value = paste0(round(fit, 2), " [", round(lower, 2), "; ", round(higher, 2), "]")) %>%
      dplyr::select(tau, value) %>% 
      pivot_wider(names_from = tau, values_from = value)
  })
  
}