


load("data/plotdata_pf.RData") 
load("data/plotdata_ue.RData") 
load("data/plotdata_pi.RData") 

server <- function(input, output, session) {

  
 # Keep this for trouble shooting on identifying input
 # output$country_choice <- renderText({
 #   paste0("You selected ", input$country)
 # })
 # 
 # output$age_choice <- renderText({ 
 #   paste0("You selected ", is.numeric(input$age))
 # })
 # 
 # output$sex_choice <- renderText({ 
 #   paste0("You selected ", is.numeric(input$sex))
 #   
 # })
 # 
 # output$pf_choice <- renderText({ 
 #   paste0("You selected ", input$tscore_pf)
 # })
 # 
 # output$ue_choice <- renderText({ 
 #   paste0("You selected ", input$tscore_ue)
 # })
  
  # Plots
  output$plot_pf <- renderPlot({ 
    plot_personograph_with_interpolation_pf(input_age = input$age, 
                                         input_sex = input$sex, 
                                         input_country = input$country, 
                                         input_tscore = input$tscore_pf, 
                                         domain_data = plotdata_pf) # former: plotdat_pf_sex
  })
  
  
  output$plot_ue <- renderPlot({ 
    plot_personograph_with_interpolation_ue(input_age = input$age, 
                                         input_sex = input$sex, 
                                         input_country = input$country, 
                                         input_tscore = input$tscore_ue, 
                                         domain_data = plotdata_ue)
  })
  
  output$plot_pi <- renderPlot({ 
    plot_personograph_with_interpolation_pi(input_age = input$age, 
                                         input_sex = input$sex, 
                                         input_country = input$country, 
                                         input_tscore = input$tscore_pi, 
                                         domain_data = plotdata_pi)
  })
  
  # Tables
  output$table_pf <- renderTable({
    result = plotdata_pf %>% 
      dplyr::filter(sex == input$sex & age == input$age & country == ifelse(input$country == "country0", 0, 
                                                  ifelse(input$country == "country1", 1, 
                                                         ifelse(input$country == "country2", 2)))) %>% 
      mutate(value = paste0(round(fit, 2), " [", round(lower, 2), "; ", round(higher, 2), "]")) %>%
      dplyr::select(tau, value) %>% 
      pivot_wider(names_from = tau, values_from = value)  
    colnames(result) = paste0(sprintf("%.0f", as.numeric(colnames(result))*100), "%")
    result
      
  })
  
  output$table_ue <- renderTable({
    result = plotdata_ue %>% 
      dplyr::filter(sex == input$sex & age == input$age & country == ifelse(input$country == "country0", 0, 
                                                                     ifelse(input$country == "country1", 1, 
                                                                            ifelse(input$country == "country2", 2)))) %>% 
      mutate(value = paste0(round(fit, 2), " [", round(lower, 2), "; ", round(higher, 2), "]")) %>%
      dplyr::select(tau, value) %>% 
      pivot_wider(names_from = tau, values_from = value)
    colnames(result) = paste0(sprintf("%.0f", as.numeric(colnames(result))*100), "%")
    result
  })
  
  output$table_pi <- renderTable({
    result = plotdata_pi %>% 
      dplyr::filter(sex == input$sex & age == input$age & country == ifelse(input$country == "country0", 0, 
                                                                            ifelse(input$country == "country1", 1, 
                                                                                   ifelse(input$country == "country2", 2)))) %>% 
      mutate(value = paste0(round(fit, 2), " [", round(lower, 2), "; ", round(higher, 2), "]")) %>%
      dplyr::select(tau, value) %>% 
      pivot_wider(names_from = tau, values_from = value)
    colnames(result) = paste0(sprintf("%.0f", as.numeric(colnames(result))*100), "%")
    result
  })
  
}