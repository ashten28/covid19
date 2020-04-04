# ui

fluidPage(useShinyjs(), 
          theme = shinytheme("yeti"),
          
          fluidRow(
            column(8, 
                   headerPanel(
                     title = "Covid19", 
                     windowTitle = "Covid19"),
                   style = "text-align:left; display:inline; margin-left:0"
            )
          ),
          
          br(),
          br(),
          
          fluidRow(
            column(6, 
                   wellPanel(
                     fluidRow(
                       column(11, 
                              h3("Covid19", style = "text-decoration:underline")
                       )
                       
                     ),
                     br(),
                     fluidRow(
                       column(2, 
                              pickerInput(
                                # width = "100%",
                                inputId = "location",
                                label = "Location:",
                                choices = c("Canada", "More coming soon"),
                                selected = "Canada",
                                multiple = F,
                                choicesOpt = list(
                                  disabled = c("Canada", "More coming soon") %in% c("More coming soon")
                                )
                              )
                       )
                       
                       
                     )
                     
                     
                     
                   )
            )
            
            
          )
          
          
)