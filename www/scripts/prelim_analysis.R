# Authors : Aldous Silas, Ashten Anthony 
# Email   : aldous.silas@yahoo.com, ashten28@gmail.com

# options(shiny.launch.browser = TRUE)

# r packages used
rpkgs <- 
  c("dplyr", "stringr", "shiny", "scales", "shinydashboard", "shinythemes", "shinyWidgets", "shinyjs", 
    "googledrive", "googlesheets4")

# install r packages that have not been installed
rpkgs_to_install <- 
  rpkgs[!(rpkgs %in% installed.packages()[,"Package"])]

if(length(rpkgs_to_install)) { install.packages(rpkgs_to_install) }

# load r packages
for (pkg in rpkgs){
  suppressMessages(library(pkg, character.only = T))
  
}

# theme pallete
theme_pallete <- 
  c("#dfe5ef", "#c3cfe1", "#9db1cf", "#6f8db9", "#4c6c9c", "#364d6e")

scales::show_col(theme_pallete)

# deaunthicate google account to prevent prompt login - only works for public sheets
drive_deauth()
sheets_deauth()

# url to covid19 public google sheet
gsheets_url <- "https://docs.google.com/spreadsheets/d/1P3PbK_c2V_k2H2xv4FnbpErmE5AjOQV0MC4_BsguFR0/edit?usp=sharing"

# read covid19 
daily_num_cases <- 
  read_sheet(as_id(gsheets_url), sheet = "daily_num_cases")

glimpse(daily_num_cases)

