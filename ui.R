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
            
            # ,column(1, class ="dropdown", loadingLogo('./img/cv19pic2.png', './img/loading.gif', height = 80, width = 80), style="float:right; margin-top:20px")
            
            
            ),
          
          br(),
          # br(),
          
          fluidRow(
            column(12, 
                   wellPanel(
                     # h3("Confirmed cases", style = "text-decoration:underline"),
                     h3("Confirmed, deaths and recovered cases - incremental", style = ""),
                     br(),
                     fluidRow(
                       column(2, 
                              pickerInput(
                                # width = "100%",
                                inputId = "country",
                                label = "Country:",
                                # choices = c("Canada", "More coming soon"),
                                choices = countries_list,
                                selected = "Canada",
                                multiple = F
                                # choicesOpt = list(
                                #   disabled = c("Canada", "More coming soon") %in% c("More coming soon")
                                # )
                              )
                       ),
                       
                       column(6,
                              plotOutput("inc_summary_plot", height = "700px")
                              
                              )
                       
                     ),
                     # br()
                     
                   )
            )
            
            
          ), 
          
          br(),
          
          fluidRow(
            column(12, 
                   wellPanel(
                     # h3("Confirmed cases", style = "text-decoration:underline"),
                     h3("Confirmed, deaths and recovered cases - cumulative", style = ""),
                     br(),
                     fluidRow(
                       column(2, 
                              pickerInput(
                                # width = "100%",
                                inputId = "country2",
                                label = "Country:",
                                # choices = c("Canada", "More coming soon"),
                                choices = countries_list,
                                selected = "Canada",
                                multiple = F
                                # choicesOpt = list(
                                #   disabled = c("Canada", "More coming soon") %in% c("More coming soon")
                                # )
                              )
                       ),
                       
                       column(6,
                              plotOutput("cum_summary_plot", height = "700px")
                              
                       )
                       
                     ),
                     br()
                     
                   )
            )
            
            
          )
          
          
)