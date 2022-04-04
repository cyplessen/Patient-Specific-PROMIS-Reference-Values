
# trying to exclude pf, ue, pi values for the table, but currently not working. Will add this for the publicaiton

ui <- fluidPage(
  pageWithSidebar(
    
    # Application title
    headerPanel("Test app"),
    
    sidebarPanel(
      # show only when plot tab is selected
      conditionalPanel(condition="input.tabselected==1",
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
      
      conditionalPanel(condition="input.tabselected==2",
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
                                    value = 50, min = 1, max = 100))
    ),
    
    mainPanel(
      # Keep this for trouble shooting on selecting input
      fluidRow(
        column(width = 6,
               textOutput("country_choice")), 
        column(width = 6, 
               textOutput("age_choice"))),
      textOutput("sex_choice"),
      textOutput("pf_choice"),
      textOutput("ue_choice"),
      
      tabsetPanel(
        tabPanel("Plots",value=1,
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
        tabPanel( "Tables", value = 2,
                  tags$h3("PROMIS Physical Functioning"),
                  
                  fluidRow(
                    column(width = 10, tableOutput("table_pf"))),
                  tags$h3("PROMIS Upper Extremities"),
                  
                  fluidRow(
                    column(width = 10, tableOutput("table_ue"))),
                  
                  tags$h3("PROMIS Pain Interference"),
                  
                  fluidRow(
                    column(width = 10, tableOutput("table_pi")))
        ),
        id = "tabselected"
      )
    )
  )
)
