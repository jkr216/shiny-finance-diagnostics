library(shinydashboard)
library(tidyverse)
library(tidyquant)
library(timetk)
library(tibbletime)
library(dbplyr)
library(shiny)
library(highcharter)
library(DT)
library(htmltools)

# Use purrr's split() and map() function to create the list
# needed to display the name of the airline but pass its
# # Carrier code as the value
# 
# str(airlines)
# 
# airline_list <- airlines %>%
#   collect()  %>%
#   split(.$name) %>%
#   map(~.$carrier)
# 
# # Use rlang's set_names() to easily create a valid "choices"
# # argument of the dropdown where the displayed text has to be
# # different than the value passed as the input selection
# 
# month_list <- as.list(1:12) %>%
#   set_names(month.name)
# 
# month_list$`All Year` <- 99

ui <- dashboardPage(
  dashboardHeader(title = "Portfolio Dashboard",
                  titleWidth = 220),
  dashboardSidebar(width = 220,

    fluidRow(
      column(5, offset = 1, style='padding:0px;', textInput("stock1", "Stock 1", "SPY")),
      column(4, offset = 0, style='padding:0px;', numericInput("w1", "Portf. %", 25, min = 1, max = 100))
    ),
    fluidRow(
      column(5, offset = 1, style='padding:0px;', textInput("stock2", "Stock 2", "EFA")),
      column(4, offset = 0, style='padding:0px;', numericInput("w2", "Portf. %", 25, min = 1, max = 100))
    ),
    fluidRow(
      column(5, offset = 1, style='padding:0px;', textInput("stock3", "Stock 3", "IJS")),
      column(4, offset = 0, style='padding:0px;', numericInput("w3", "Portf. %", 20, min = 1, max = 100))
    ),
    fluidRow(
      column(5, offset = 1, style='padding:0px;', textInput("stock4", "Stock 4", "EEM")),
      column(4, offset = 0, style='padding:0px;', numericInput("w4", "Portf. %", 20, min = 1, max = 100))
    ),
    fluidRow(
      column(5, offset = 1, style='padding:0px;', textInput("stock5", "Stock 5", "AGG")),
      column(4, offset = 0, style='padding:0px;', numericInput("w5", "Portf. %", 10, min = 1, max = 100))
    ),
    
    fluidRow(
      column(6, offset = 1, style='padding:0px;', dateInput("date", 
                                                            "Start Date", 
                                                            min = "2000-01-01", 
                                                            "2018-01-01", 
                                                            format = "yyyy-mm-dd"))
    ),
    
    fluidRow(
      column(5, offset = 1, style='padding:0px;', numericInput("sim_months", 
                                                               "Months", 
                                                               120, 
                                                               min = 6, 
                                                               max = 240, 
                                                               step = 6)),
      column(4, offset = 0, style='padding:0px;', numericInput("sims", 
                                                               "Sims", 
                                                               51, 
                                                               min = 31,
                                                               max = 101, 
                                                               step = 10))
    ),
    
   actionButton("go", "Submit")
    
    ),
  dashboardBody(      
    tabsetPanel(id = "tabs",
                tabPanel(
                  title = "Monte Carlo",
                  value = "page1",
                  fluidRow(
                    box(width = 12, highchartOutput("hc_monte_all", height = 250))
                  ),
                  fluidRow(
                    box(width = 12, highchartOutput("hc_min_max_med", height = 250))
                  )
                )
    )
  )
)

server <- function(input, output, session) { 
  
prices <- eventReactive(input$go, label = "prices", {
    
    symbols <- c(input$stock1, input$stock2, input$stock3, input$stock4, input$stock5)
    
    getSymbols(symbols, src = 'yahoo', from = input$date, 
               auto.assign = TRUE, warnings = FALSE) %>% 
      map(~Ad(get(.))) %>% 
      reduce(merge) %>%
      `colnames<-`(symbols)
  })
  
portfolio_returns_tq_rebalanced_monthly <- eventReactive(input$go, label = "portfolio returns", {
  
  validate(need(input$w1 + input$w2 + input$w3 + input$w4 + input$w5 == 100, 
                "The portfolio weights must sum to 100%!"))  
  
    prices <- prices()
    
    w <- c(input$w1/100, input$w2/100, input$w3/100, input$w4/100, input$w5/100)
    
    portfolio_returns_tq_rebalanced_monthly <- 
      prices %>% 
      to.monthly(indexAt = "last", OHLC = FALSE) %>% 
      tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
      gather(asset, returns, -date) %>% 
      group_by(asset) %>%  
      mutate(returns = (log(returns) - log(lag(returns)))) %>%
      tq_portfolio(assets_col  = asset, 
                   returns_col = returns,
                   weights     = w,
                   col_rename  = "returns",
                   rebalance_on = "months")
  })
  
mean_port_return <- eventReactive(input$go, label = "mean returns", {
  
  portfolio_returns_tq_rebalanced_monthly <- portfolio_returns_tq_rebalanced_monthly()
  
  mean(portfolio_returns_tq_rebalanced_monthly$returns)
})

stddev_port_return <- eventReactive(input$go, label = "sigma returns", {
  
  portfolio_returns_tq_rebalanced_monthly <- portfolio_returns_tq_rebalanced_monthly()
  
  sd(portfolio_returns_tq_rebalanced_monthly$returns)
})

simulation_accum_1 <- function(init_value, N, mean, stdev) {
  tibble(c(init_value, 1 + rnorm(N, mean, stdev))) %>% 
    `colnames<-`("returns") %>%
    mutate(growth = accumulate(returns, function(x, y) x * y)) %>% 
    select(growth)
}

sims <- eventReactive(input$go, {input$sims})

monte_carlo_sim <- eventReactive(input$go, label = "run simulations", { 
  
  sims <- sims()
  
  starts <-  
    rep(1, sims) %>%
    set_names(paste("sim", 1:sims, sep = ""))
  
  map_dfc(starts, simulation_accum_1,
          N = input$sim_months, mean = mean_port_return(), 
          stdev = stddev_port_return()) %>% 
    mutate(month = seq(1:nrow(.))) %>% 
    select(month, everything()) %>% 
    `colnames<-`(c("month", names(starts))) %>% 
    gather(sim, growth, -month) %>% 
    group_by(sim) %>% 
    mutate_all(funs(round(., 2)))
  
})
  
  
  # output$monthly <- renderText({
  #   if(input$month == "99")"Click on a month in the plot to see the daily counts"
  # })
  # 
  # output$total_flights <- renderValueBox({
  #   # The following code runs inside the database
  #   result <- db_flights %>%
  #     filter(carrier == input$airline)
  #   
  #   if(input$month != 99) result <- filter(result, month == input$month)
  #   
  #   result <- result %>%
  #     tally() %>%
  #     pull() %>% 
  #     as.integer()
  #   
  #   valueBox(value = prettyNum(result, big.mark = ","),
  #            subtitle = "Number of Flights")
  # })
  
  
  # output$per_day <- renderValueBox({
  #   
  #   # The following code runs inside the database
  #   result <- db_flights %>%
  #     filter(carrier == input$airline)
  #   
  #   if(input$month != 99) result <- filter(result, month == input$month)
  #   result <- result %>%
  #     group_by(day, month) %>%
  #     tally() %>%
  #     summarise(avg = mean(n)) %>%
  #     pull()
  #   
  #   valueBox(prettyNum(result, big.mark = ","),
  #            subtitle = "Average Flights",
  #            color = "blue")
  # })
  
  
  
  # output$percent_delayed <- renderValueBox({
  #   
  #   # The following code runs inside the database
  #   result <- db_flights %>%
  #     filter(carrier == input$airline)
  #   
  #   if(input$month != 99) result <- filter(result, month == input$month)
  #   result <- result %>%
  #     filter(!is.na(dep_delay)) %>%
  #     mutate(delayed = ifelse(dep_delay >= 15, 1, 0)) %>%
  #     summarise(delays = sum(delayed),
  #               total = n()) %>%
  #     mutate(percent = delays / total) %>%
  #     pull()
  #   
  #   valueBox(paste0(round(result * 100), "%"),
  #            subtitle = "Flights delayed",
  #            color = "teal")
  # })
  
  # Events in Highcharts can be tracked using a JavaScript. For data points in a plot, the 
  # event.point.category returns the value that is used for an additional filter, in this case
  # the month that was clicked on.  A paired observeEvent() command is activated when
  # this java script is executed
  #  js_click_line <- JS("function(event) {Shiny.onInputChange('line_clicked', [event.point.category]);}")
  
  output$hc_monte_all <- renderHighchart({
    validate(need(input$go, "Please choose your portfolio assets, weights, rfr, rolling window and start date and click submit."))
    
      hchart( monte_carlo_sim(), 
              type = 'line', 
              hcaes(y = growth,
                    x = month,
                    group = sim)) %>% 
        hc_title(text = paste(sims(), "Simulations", sep = " ")) %>%
        hc_xAxis(title = list(text = "months")) %>%
        hc_yAxis(title = list(text = "dollar growth"),
                 labels = list(format = "${value}")) %>%
        hc_add_theme(hc_theme_flat()) %>%
        hc_exporting(enabled = TRUE) %>% 
        hc_legend(enabled = FALSE)
    
  })
  
  output$hc_min_max_med <- renderHighchart({
    validate(need(input$go, "Please choose your portfolio assets, weights, rfr, rolling window and start date and click submit."))
    
    sim_summary <- 
      monte_carlo_sim() %>%
      summarise(final = last(growth)) %>% 
      summarise(
        max = max(final), 
        min = min(final),
        median = median(final))
    
    mc_max_med_min <- 
      monte_carlo_sim() %>%
      filter(
        last(growth) == sim_summary$max || 
          last(growth) == sim_summary$median ||
          last(growth) == sim_summary$min)
    
    hchart(mc_max_med_min, 
           type = 'line', 
           hcaes(y = growth,
                 x = month,
                 group = sim)) %>% 
      hc_title(text = "Min Max Median Simulations") %>%
      hc_xAxis(title = list(text = "months")) %>%
      hc_yAxis(title = list(text = "dollar growth"),
               labels = list(format = "${value}")) %>%
      hc_add_theme(hc_theme_flat()) %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_legend(enabled = FALSE)
  })
}
  # Tracks the JavaScript event created by `js_click_line`
  #observeEvent(input$line_clicked != "",
              # if(input$month == 99)
              #    updateSelectInput(session, "month", selected = input$line_clicked),
              #  ignoreInit = TRUE)
  
  #js_bar_clicked <- JS("function(event) {Shiny.onInputChange('bar_clicked', [event.point.category]);}")
#   
#   output$top_airports <- renderD3({
#     # The following code runs inside the database
#     result <- db_flights %>%
#       filter(carrier == input$airline) 
#     
#     if(input$month != 99) result <- filter(result, month == input$month) 
#     
#     result <- result %>%
#       group_by(dest_name) %>%
#       tally() %>%
#       arrange(desc(n)) %>%
#       collect() %>%
#       head(10)
#     
#     r2d3(
#       result,
#       "bar_plot.js"
#     )
#     
#     
#   })
#   
#   observeEvent(input$bar_clicked,
#                {
#                  airport <- input$bar_clicked[1]
#                  tab_title <- paste(input$airline, 
#                                     "-", airport , 
#                                     if(input$month != 99) paste("-" , month.name[as.integer(input$month)]))
#                  
#                  if(tab_title %in% tab_list == FALSE){
#                    details <- db_flights %>%
#                      filter(dest_name == airport,
#                             carrier == input$airline)
#                    
#                    if(input$month != 99) details <- filter(details, month == input$month) 
#                    
#                    details <- details %>%
#                      head(100) %>% 
#                      select(month,
#                             day,
#                             flight,
#                             tailnum,
#                             dep_time,
#                             arr_time,
#                             dest_name,
#                             distance) %>%
#                      collect() %>%
#                      mutate(month = month.name[as.integer(month)])
#                    
#                    
#                    appendTab(inputId = "tabs",
#                              tabPanel(
#                                tab_title,
#                                DT::renderDataTable(details)
#                              ))
#                    
#                    tab_list <<- c(tab_list, tab_title)
#                    
#                  }
#                  
#                  updateTabsetPanel(session, "tabs", selected = tab_title)
#                  
#                })
#   
#   observeEvent(input$remove,{
#     # Use purrr's walk command to cycle through each
#     # panel tabs and remove them
#     tab_list %>%
#       walk(~removeTab("tabs", .x))
#     tab_list <<- NULL
#   })
#   
#  }



shinyApp(ui, server)