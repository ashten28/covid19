# Authors : Aldous Silas, Ashten Anthony 
# Email   : aldous.silas@yahoo.com, ashten28@gmail.com

# rsconnect::deployApp(getwd())

options(shiny.launch.browser = TRUE)

# r packages used
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(ggplot2)
library(ggrepel)
library(scales)
library(patchwork)
library(reactable)
library(sparkline)
library(showtext)
library(curl)

# font_add_google("Schoolbell", "bell")
# showtext::showtext_auto()

# theme pallete
# theme_pallete <- 
#   c("#dfe5ef", "#c3cfe1", "#9db1cf", "#6f8db9", "#4c6c9c", "#364d6e")
# 
# scales::show_col(theme_pallete)

font_add_google("Montserrat", "Montserrat")
showtext_auto()

# ====================================== #
# ------      data from github   -------#
# ====================================== #

# covid19_data <- 
#   load("www/data/COVID-19.RData")
# covid19_data2 <- 
#   read.csv(text = RCurl::getURL("https://raw.githubusercontent.com/ashten28/covid19_analysis/master/01_data/covid19_daily_cases_cleaned.csv")) %>%
#   as_tibble() %>% 
#   mutate(
#     report_date = as.Date(as.character(report_date))
#   )

covid19_data <- 
  read.csv("www/data/covid19_daily_cases_cleaned.csv") %>%
  as_tibble() %>% 
  mutate(
    report_date = as.Date(as.character(report_date))
  )
  
countries_list <-
  covid19_data %>% 
  distinct(country_region) %>% 
  pull() %>% 
  as.character() %>% 
  sort()

covid19_data_latest <-
  covid19_data %>% 
  group_by(country_region, report_date) %>%
  summarize(
    inc_confirmed = sum(inc_confirmed),
    inc_deaths    = sum(inc_deaths),
    inc_recovered = sum(inc_recovered),
    cum_confirmed = sum(cum_confirmed),
    cum_deaths    = sum(cum_deaths),
    cum_recovered = sum(cum_recovered)
  ) %>% 
  ungroup() %>% 
  mutate(day = row_number()) 

total_confirmed <- sum(covid19_data_latest$cum_confirmed)
total_deaths    <- sum(covid19_data_latest$cum_deaths)
total_recovered <- sum(covid19_data_latest$cum_recovered)

# + theme(text=element_text(size=16,  family="Comic Sans MS"))


# ====================================== #
# ------       google sheets     ------- #
# ====================================== #

# # deaunthicate google account to prevent prompt login - only works for public sheets
# drive_deauth()
# sheets_deauth()
# 
# # url to covid19 public google sheet
# gsheets_url <- "https://docs.google.com/spreadsheets/d/1P3PbK_c2V_k2H2xv4FnbpErmE5AjOQV0MC4_BsguFR0/edit?usp=sharing"
# 
# # read covid19 
# daily_confirmed <- 
#   read_sheet(as_id(gsheets_url), sheet = "daily_confirmed")
# 
# glimpse(daily_daily_confirmed)

# ====================================== #
# ------        loading logo     ------- #
# ====================================== #

loadingLogo <- function(src, loadingsrc, height = NULL, width = NULL, alt = NULL) {
  tagList(
    tags$head(
      tags$script(
        "setInterval(function(){
        if ($('html').attr('class')=='shiny-busy') {
        $('div.busy').show();
        $('div.notbusy').hide();
        } else {
        $('div.busy').hide();
        $('div.notbusy').show();
        }
        },100)")
    ),
    tags$div(
      div(class = "busy", style = "text-align:right;",
          img(src=loadingsrc, height = 44, width = 44, alt = alt)),
      div(class = 'notbusy',
          img(src = src, height = height, width = width, alt = alt))
    )
  )
} 

# ====================================== #
# ------        to detlete     ------- #
# ====================================== #

# covid19_data_selected_smooth <- covid19_data_selected

# covid19_data_selected_smooth$inc_confirmed_smooth = predict(x = covid19_data_selected$day, fit_confirmed)$y
# covid19_data_selected_smooth$inc_deaths_smooth    = predict(x = covid19_data_selected$day, fit_deaths)$y
# covid19_data_selected_smooth$inc_recovered_smooth = predict(x = covid19_data_selected$day, fit_recovered)$y
