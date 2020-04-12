# Authors : Aldous Silas, Ashten Anthony 
# Email   : aldous.silas@yahoo.com, ashten28@gmail.com

# options(shiny.launch.browser = TRUE)

# r packages used
rpkgs <- 
  c("dplyr", "stringr", "shiny", "scales", "shinydashboard", "shinythemes", "shinyWidgets", "shinyjs", 
    "googledrive", "googlesheets4", "ISLR", "splines")

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

# ====================================== #
# ------      data from github   -------#
# ====================================== #

covid19_data <- 
  read.csv(text = RCurl::getURL("https://raw.githubusercontent.com/ashten28/covid19_analysis/master/01_data/covid19_daily_cases_cleaned.csv")) %>% 
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
  filter(report_date == max(report_date))

total_confirmed <- sum(covid19_data_latest$cum_confirmed)
total_deaths    <- sum(covid19_data_latest$cum_deaths)
total_recovered <- sum(covid19_data_latest$cum_recovered)

#Choosing a country and a region for prediction

check_out <- covid19_data %>% filter(country_region == "Canada",province_state == "Ontario")
check_out$first_date <- min(check_out$report_date)
check_out$day_dff <- as.Date(as.character(check_out$report_date), format="%Y-%m-%d")-
                          as.Date(as.character(check_out$first_date), format="%Y-%m-%d")

#The number of cases "7" is very suspesious. Changed it to 400 based on the data I have.
check_out$inc_confirmed[38] <- 400
#cubic spline
fit<-lm(inc_confirmed ~ bs(day_dff,knots = c(43,50,65)),data = check_out )
summary(fit)

#plot
#Plotting the  Line to the scatterplot   
day_dff_limits<-range(check_out$day_dff)
#Generating Test Data
day_dff_grid<-seq(from=day_dff_limits[1], to = day_dff_limits[2])

plot(check_out$day_dff,check_out$inc_confirmed,col="grey",xlab="incremental Days",ylab="Confirmed Cases")
points(day_dff_grid,predict(fit,newdata = list(day_dff=day_dff_grid)),col="darkgreen",lwd=2,type="l")
#adding cutpoints
abline(v=c(43,50,65),lty=2,col="darkgreen")



#fitting smoothing splines using smooth.spline(X,Y,df=...)
fit1<-smooth.spline(check_out$day_dff,check_out$inc_confirmed,df=16) #16 degrees of freedom

#Plotting both cubic and Smoothing Splines 
plot(check_out$day_dff,check_out$inc_confirmed,col="grey",xlab="incremental Days",ylab="Confirmed Cases")
points(day_dff_grid,predict(fit,newdata = list(day_dff=day_dff_grid)),col="darkgreen",lwd=2,type="l")

#adding cutpoints
abline(v=c(43,50,65),lty=2,col="darkgreen")
lines(fit1,col="red",lwd=2)
legend("topright",c("Smoothing Spline with 16 df","Cubic Spline"),col=c("red","darkgreen"),lwd=2)


                                     
