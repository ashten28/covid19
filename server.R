# server

server <- function(input, output, session){
  
  # summary data 
  country_selected <- reactive({
    
    # get data for selected country
    covid19_data_selected <-
      covid19_data %>% 
      filter(country_region == input$country) %>% 
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
    
    # get data for selected country for latest date
    covid19_data_selected_latest <-
      covid19_data_selected %>% 
      filter(report_date == max(report_date))
    
    # get latest as at figures for confirmed, deaths and recovered for country selected
    total_confirmed <- sum(covid19_data_selected_latest$cum_confirmed)
    total_deaths    <- sum(covid19_data_selected_latest$cum_deaths)
    total_recovered <- sum(covid19_data_selected_latest$cum_recovered)
    
    # return results
    return(
      list(
        data = covid19_data_selected,
        data_latest = covid19_data_selected_latest, 
        total_confirmed = total_confirmed,
        total_deaths    = total_deaths,
        total_recovered = total_recovered
      )
      
    )
    
  })
  
  output$inc_summary_plot <- renderPlot({
    
    # covid19_data_selected <-
    #   covid19_data %>% 
    #   filter(country_region == input$country) %>% 
    #   group_by(country_region, report_date) %>%
    #   summarize(
    #     inc_confirmed = sum(inc_confirmed),
    #     inc_deaths = sum(inc_deaths),
    #     inc_recovered = sum(inc_recovered)
    #   ) %>% 
    #   ungroup() %>% 
    #   mutate(day = row_number()) 
  
    # fit spline - confirmed
    fit_confirmed <- 
      smooth.spline(x = country_selected()$data$day, y = country_selected()$data$inc_confirmed)
    
    # fit spline - deaths
    fit_deaths <- 
      smooth.spline(x = country_selected()$data$day, y = country_selected()$data$inc_deaths)
    
    # fit spline - recovered
    fit_recovered <- 
      smooth.spline(x = country_selected()$data$day, y = country_selected()$data$inc_recovered)
    
    # predict smooth spline numbers
    covid19_data_selected_smooth <- 
      country_selected()$data %>% 
      mutate(
        inc_confirmed_smooth = predict(x = country_selected()$data$day, fit_confirmed)$y,
        inc_deaths_smooth    = predict(x = country_selected()$data$day, fit_deaths)$y,
        inc_recovered_smooth = predict(x = country_selected()$data$day, fit_recovered)$y
      )
    
    # pivot columns to create long format
    covid19_data_selected_long <-
      covid19_data_selected_smooth %>% 
      select(day, inc_confirmed_smooth, inc_deaths_smooth, inc_recovered_smooth) %>% 
      pivot_longer(cols = c("inc_confirmed_smooth", "inc_deaths_smooth", "inc_recovered_smooth"), names_to = "type", values_to = "value") %>% 
      mutate(
        type = case_when(
          type == "inc_confirmed_smooth" ~ "Confirmed",
          type == "inc_deaths_smooth" ~ "Deaths",
          type == "inc_recovered_smooth" ~ "Recovered",
        )
      )
    
    # get latest data to highlight point
    covid19_data_selected_point <- 
      covid19_data_selected_long %>% 
      filter(day == max(day))
    
    # first plot - line plot
    p1 <-
      covid19_data_selected_long %>% 
      ggplot(aes(x = day, y = value, colour = type)) +
      facet_grid(type~., scales = "free_y") +
      geom_line(size = 1) + 
      geom_point(
        data = covid19_data_selected_point,
        mapping = aes(x = day, y = value),
        size = 3
        
      ) + 
      geom_text_repel(
        data = covid19_data_selected_point,
        mapping = aes(x = day + 2, y = value, label = type),
        nudge_x = 2,
        # nudge_y= -2,
        segment.colour = "transparent",
        size = 5,
        na.rm = TRUE
      ) +
      geom_hline(yintercept = 0, alpha = 0.2) +
      # scale_colour_manual(values = c("#003972", "#d11d53", "#006734"))+
      scale_colour_manual(values = c("#003972", "#d11d53", "#b3a400"))+
      scale_x_continuous(limits = c(0, 90), breaks = seq(0, 80, by = 10), expand = c(0,0)) +
      # scale_y_continuous(limits = c(0, max(value)), expand = c(0, 0)) +
      # ylim(0, range) +
      # scale_y_continuous(breaks = c(12,24), expand = c(0,0)) +
      labs(
        x = "\nNumber of days", 
        y = element_blank(),
        title = paste0(input$country),
        subtitle = paste0("Smoothed incremental confirmed, deaths and recovered cases\n"),
        caption = "Github: ashten28 | aldoussilas \n\nData: Johns Hopkins CSSE"
      ) + 
      theme_bw() +
      # ggtitle("Stop procrasting") +
      theme(
        text = element_text(family = "Montserrat", size=15),
        # text = element_text(size=20),
        # panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background  = element_blank(),
        # panel.background = element_rect(fill = "#e8f4f875"),
        # plot.background  = element_rect(fill = "#e8f4f875"),
        # panel.background  = element_rect(colour = "#e8f4f880"),
        strip.text.y = element_blank() , 
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        # axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        # axis.text.y = element_blank(),
        legend.position = "none",
        # legend = element_blank(),
        legend.title = element_blank(),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(hjust = 0)
        # axis.line.x = element_blank()
        # axis.line = element_line(colour = "black")
      )
    
    
    # plot second plot - text
    # create data
    text_data <- 
      tribble(
        ~type, ~text1, ~text2,
        "Confirmed", paste0("Total confirmed: ", format(country_selected()$total_confirmed, big.mark = ",", scientific = F)), paste0("with active of "   ,  round(((country_selected()$total_confirmed-country_selected()$total_deaths-country_selected()$total_recovered
                                                                                                                                                                    )/country_selected()$total_confirmed)*100, 1) , "%"),
        "Deaths"   , paste0("Total deaths: "   , format(country_selected()$total_deaths,    big.mark = ",", scientific = F)), paste0("with death rate of "   ,  round((country_selected()$total_deaths/country_selected()$total_confirmed)*100, 1) , "%"),
        "Recovered", paste0("Total recovered: ", format(country_selected()$total_recovered, big.mark = ",", scientific = F)), paste0("with recovery rate of ",  round((country_selected()$total_recovered/country_selected()$total_confirmed)*100, 1), "%"),
      )
    
    # second plot
    p2 <-
      ggplot(text_data) +
      geom_rect(aes(xmin = -1, ymin = -1, xmax = 1, ymax = 1), fill = "#e8f4f8", color = "white") +
      geom_text(aes(label = text1, x = -0.9, y =  0.2), hjust = 0, size = 5) +
      geom_text(aes(label = text2, x = -0.9, y = -0.1), hjust = 0, size = 4) +
      # coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1)) +
      facet_grid(type~.) +
      labs(x=NULL, y=NULL) +
      theme_void() +
      theme(
        text = element_text(family = "Montserrat"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        # axis.ticks = element_blank(),
        # axis.ticks.length = unit(0, "pt"),
        # panel.spacing.y = unit(-2, "lines"),
        # panel.spacing.x = unit(-2, "lines"),
        # panel.spacing = unit(0, "lines"),
        # panel.border = element_rect(colour = "grey")
        # panel.border = element_rect(colour = "black", fill=NA, size=2),
        # panel.background = element_rect(fill = "#e8f4f875"),
        # plot.background  = element_rect(fill = "#e8f4f875")
        # plot.margin=grid::unit(c(0,0,0,0), "mm")
      )
    
    # use patchwork to combine plots
    (p1 | p2) + plot_layout(widths = c(3, 1)) + theme(text = element_text(family = "Montserrat"))
    
  }, bg= "transparent")
  
  output$summary_table <- renderReactable({
    
    # make_color_pal <- function(colors, bias = 1) {
    #   get_color <- colorRamp(colors, bias = bias)
    #   function(x) rgb(get_color(x), maxColorValue = 255)
    # }
    # 
    # off_rating_color <- make_color_pal(c("#ff2700", "#f8fcf8", "#44ab43"), bias = 1.3)
    
    new_data <-
      covid19_data_latest %>% 
      filter(country_region %in% input$country2) %>% 
      group_by(country_region) %>% 
      summarize(
        cum_confirmed = list(cum_confirmed),
        cum_deaths    = list(cum_deaths),
        cum_recovered = list(cum_recovered),
        inc_confirmed = last(inc_confirmed),
        inc_deaths    = last(inc_deaths),
        inc_recovered = last(inc_recovered)
        
        )  %>% 
      ungroup() %>% 
      select(country_region, cum_confirmed, inc_confirmed, cum_deaths, inc_deaths, cum_recovered, inc_recovered) %>% 
      arrange(desc(inc_confirmed))
      # head(10)
    
    # table <-
      reactable(
        data = new_data, 
        columnGroups = list(
          colGroup(name = "Confirmed", columns = c("cum_confirmed", "inc_confirmed")),
          colGroup(name = "Deaths",    columns = c("cum_deaths", "inc_deaths")),
          colGroup(name = "Recovered", columns = c("cum_recovered", "inc_recovered"))
        ),
        defaultColDef = colDef(headerClass = "header col-header"),
        columns = list(
          # Confirmed = colDef(name = )
          # weight = colDef(cell = function(values) {
          #   sparkline(values, type = "bar", chartRangeMin = 0, chartRangeMax = max(chickwts$weight))
          # }),
          # boxplot = colDef(cell = function(value, index) {
          #   sparkline(new_data$cum_confirmed[[index]], type = "box")
          # }),
          country_region = 
            colDef(
              name = "Country"
              # , style = list(borderRight = "1px solid rgba(0, 0, 0, 0.7)")
              ),
          
          cum_confirmed = 
            colDef(
              name = "Trend", 
              align = "center",
              width = 200, 
              cell = function(value, index) {sparkline(new_data$cum_confirmed[[index]])}
              
            ),
          
          inc_confirmed =
            colDef(
              name = "Increase",
              align = "center",
              width = 120,  
              # style = list(borderRight = "1px solid rgba(0, 0, 0, 0.7)"),
              cell = function(value) {
                # if (value >= 3.3) {
                # classes <- "tag num-high"
                # } else if (value >= 3) {
                  classes <- "tag num-med"
                # } else {
                # classes <- "tag num-low"
                # }
                value <-
                  format(value, nsmall = 1)
                span(class = classes, value)
              }
            ),
          
          cum_deaths = 
            colDef(
              name = "Trend", 
              align = "center",
              width = 200, 
              cell = function(value, index) {sparkline(new_data$cum_deaths[[index]])}
            ),
          
          inc_deaths =
            colDef(
              name = "Increase",
              align = "center",
              width = 120,  
              # style = list(borderRight = "1px solid rgba(0, 0, 0, 0.7)"),
              cell = function(value) {
                # if (value >= 3.3) {
                #   classes <- "tag num-high"
                # } else if (value >= 3) {
                #   classes <- "tag num-med"
                # } else {
                  classes <- "tag num-low"
                # }
                value <-
                  format(value, nsmall = 1)
                span(class = classes, value)
              }
            ),
          
          cum_recovered = 
            colDef(
              name = "Trend",
              align = "center",
              width = 200, 
              cell = function(value, index) {sparkline(new_data$cum_recovered[[index]])}
            ),
          
          inc_recovered =
            colDef(
              name = "Increase",
              align = "center",
              width = 120,  
              # style = list(borderRight = "1px solid rgba(0, 0, 0, 0.7)"),
              cell = function(value) {
                # if (value >= 3.3) {
                  classes <- "tag num-high"
                # } else if (value >= 3) {
                #   classes <- "tag num-med"
                # } else {
                # classes <- "tag num-low"
                # }
                value <-
                  format(value, nsmall = 1)
                span(class = classes, value)
              }
            )
          
          
        ),
        bordered = TRUE,
        # searchable = TRUE,
        highlight = TRUE,
        paginationType = "simple",
        height = "700px"
      )
    
    
  })
  
}