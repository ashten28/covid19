# server

server <- function(input, output, session){
  
  output$inc_summary_plot <- renderPlot({
    
    covid19_data_selected <-
      covid19_data %>% 
      filter(country_region == input$country) %>% 
      group_by(country_region, report_date) %>%
      summarize(
        inc_confirmed = sum(inc_confirmed),
        inc_deaths = sum(inc_deaths),
        inc_recovered = sum(inc_recovered)
      ) %>% 
      ungroup() %>% 
      mutate(day = row_number()) 
    
    fit_confirmed <- 
      smooth.spline(x = covid19_data_selected$day, y = covid19_data_selected$inc_confirmed)
    
    fit_deaths <- 
      smooth.spline(x = covid19_data_selected$day, y = covid19_data_selected$inc_deaths)
    
    fit_recovered <- 
      smooth.spline(x = covid19_data_selected$day, y = covid19_data_selected$inc_recovered)
    
    covid19_data_selected_smooth <- 
      covid19_data_selected %>% 
      mutate(
        inc_confirmed_smooth = predict(x = covid19_data_selected$day, fit_confirmed)$y,
        inc_deaths_smooth    = predict(x = covid19_data_selected$day, fit_deaths)$y,
        inc_recovered_smooth = predict(x = covid19_data_selected$day, fit_recovered)$y
      )
    
    
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
    
    covid19_data_selected_point <- 
      covid19_data_selected_long %>% 
      filter(day == max(day))
    
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
    
  }, bg= "transparent")
  
  output$cum_summary_plot <- renderPlot({
    
    covid19_data_selected <-
      covid19_data %>% 
      filter(country_region == input$country) %>% 
      group_by(country_region, report_date) %>%
      summarize(
        cum_confirmed = sum(cum_confirmed),
        cum_deaths = sum(cum_deaths),
        cum_recovered = sum(cum_recovered)
      ) %>% 
      ungroup() %>% 
      mutate(day = row_number()) 
    
    fit_confirmed <- 
      smooth.spline(x = covid19_data_selected$day, y = covid19_data_selected$cum_confirmed)
    
    fit_deaths <- 
      smooth.spline(x = covid19_data_selected$day, y = covid19_data_selected$cum_deaths)
    
    fit_recovered <- 
      smooth.spline(x = covid19_data_selected$day, y = covid19_data_selected$cum_recovered)
    
    covid19_data_selected_smooth <- 
      covid19_data_selected %>% 
      mutate(
        cum_confirmed_smooth = predict(x = covid19_data_selected$day, fit_confirmed)$y,
        cum_deaths_smooth    = predict(x = covid19_data_selected$day, fit_deaths)$y,
        cum_recovered_smooth = predict(x = covid19_data_selected$day, fit_recovered)$y
      )
    
    covid19_data_selected_long <-
      covid19_data_selected_smooth %>% 
      select(day, cum_confirmed_smooth, cum_deaths_smooth, cum_recovered_smooth) %>% 
      pivot_longer(cols = c("cum_confirmed_smooth", "cum_deaths_smooth", "cum_recovered_smooth"), names_to = "type", values_to = "value") %>% 
      mutate(
        type = case_when(
          type == "cum_confirmed_smooth" ~ "Confirmed",
          type == "cum_deaths_smooth" ~ "Deaths",
          type == "cum_recovered_smooth" ~ "Recovered",
        )
      )
    
    covid19_data_selected_point <- 
      covid19_data_selected_long %>% 
      filter(day == max(day))
    
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
      scale_colour_manual(values = c("#003972", "#d11d53", "#ccbb00"))+
      scale_x_continuous(limits = c(0, 90), breaks = seq(0, 80, by = 10), expand = c(0,0)) +
      # scale_y_continuous(breaks = c(12,24), expand = c(0,0)) +
      labs(
        x = "\nNumber of days", 
        y = element_blank(),
        title = paste0(input$country),
        subtitle = paste0("Smoothed cumulative confirmed, deaths and recovered cases\n"),
        caption = "Github: ashten28 | aldoussilas \n\nData: Johns Hopkins CSSE"
      ) + 
      theme_bw() +
      # ggtitle("Stop procrasting") +
      theme(
        text = element_text(family = "Montserrat", size = 15),
        # text = element_text(size=20),
        # panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
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
    
  }, bg= "transparent")
  
  
}