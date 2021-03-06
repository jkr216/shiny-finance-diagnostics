library(shinydashboard)
library(dplyr)
library(dbplyr)
library(purrr)
library(shiny)
library(highcharter)
library(DT)
library(htmltools)
library(nycflights13)
library(r2d3)
library(stringr)

# Use purrr's split() and map() function to create the list
# needed to display the name of the airline but pass its
# Carrier code as the value

str(airlines)

airline_list <- airlines %>%
  collect()  %>%
  split(.$name) %>%
  map(~.$carrier)

# Use rlang's set_names() to easily create a valid "choices"
# argument of the dropdown where the displayed text has to be
# different than the value passed as the input selection

month_list <- as.list(1:12) %>%
  set_names(month.name)

month_list$`All Year` <- 99

ui <- dashboardPage(
  dashboardHeader(title = "Flights Dashboard",
                  titleWidth = 200),
  dashboardSidebar(
    selectInput(
      inputId   = "airline",
      label     = "Airline:", 
      choices   = airline_list, 
      selectize = FALSE),
    sidebarMenu(
      selectInput(
        inputId = "month",
        label =   "Month:", 
        choices =  month_list, 
        selected =  99,
        selectize = FALSE
      ),
      actionLink("remove", "Remove detail tabs")
    )
  ),
  dashboardBody(      
    tabsetPanel(id = "tabs",
                tabPanel(
                  title = "Main Dashboard",
                  value = "page1",
                  fluidRow(
                    valueBoxOutput("total_flights"),
                    valueBoxOutput("per_day"),
                    valueBoxOutput("percent_delayed")
                  ),
                  fluidRow(
                    
                    
                  ),
                  fluidRow(
                    column(width = 12,
                           p(textOutput("monthly")),
                           highchartOutput("group_totals"))
                  )
                )
    )
  )
)

server <- function(input, output, session) { 
  
  tab_list <- NULL
  
  # Preparing the data by pre-joining flights to other
  # tables and doing some name clean-up
  db_flights <- flights %>%
    left_join(airlines, by = "carrier") %>%
    rename(airline = name) %>%
    left_join(airports, by = c("origin" = "faa")) %>%
    rename(origin_name = name) %>%
    select(-lat, -lon, -alt, -tz, -dst) %>%
    left_join(airports, by = c("dest" = "faa")) %>%
    rename(dest_name = name) 
  
  output$monthly <- renderText({
    if(input$month == "99")"Click on a month in the plot to see the daily counts"
  })
  
  output$total_flights <- renderValueBox({
    # The following code runs inside the database
    result <- db_flights %>%
      filter(carrier == input$airline)
    
    if(input$month != 99) result <- filter(result, month == input$month)
    
    result <- result %>%
      tally() %>%
      pull() %>% 
      as.integer()
    
    valueBox(value = prettyNum(result, big.mark = ","),
             subtitle = "Number of Flights")
  })
  
  
  output$per_day <- renderValueBox({
    
    # The following code runs inside the database
    result <- db_flights %>%
      filter(carrier == input$airline)
    
    if(input$month != 99) result <- filter(result, month == input$month)
    result <- result %>%
      group_by(day, month) %>%
      tally() %>%
      summarise(avg = mean(n)) %>%
      pull()
    
    valueBox(prettyNum(result, big.mark = ","),
             subtitle = "Average Flights",
             color = "blue")
  })
  
  
  
  output$percent_delayed <- renderValueBox({
    
    # The following code runs inside the database
    result <- db_flights %>%
      filter(carrier == input$airline)
    
    if(input$month != 99) result <- filter(result, month == input$month)
    result <- result %>%
      filter(!is.na(dep_delay)) %>%
      mutate(delayed = ifelse(dep_delay >= 15, 1, 0)) %>%
      summarise(delays = sum(delayed),
                total = n()) %>%
      mutate(percent = delays / total) %>%
      pull()
    
    valueBox(paste0(round(result * 100), "%"),
             subtitle = "Flights delayed",
             color = "teal")
  })
  
  # Events in Highcharts can be tracked using a JavaScript. For data points in a plot, the 
  # event.point.category returns the value that is used for an additional filter, in this case
  # the month that was clicked on.  A paired observeEvent() command is activated when
  # this java script is executed
  js_click_line <- JS("function(event) {Shiny.onInputChange('line_clicked', [event.point.category]);}")
  
  output$group_totals <- renderHighchart({
    
    if(input$month != 99) {
      result <- db_flights %>%
        filter(month == input$month,
               carrier == input$airline) %>%
        group_by(day) %>%
        tally() %>%
        collect()
      group_name <- "Daily"
    } else {
      result <- db_flights %>%
        filter(carrier == input$airline) %>%
        group_by(month) %>%
        tally() %>%
        collect()    
      group_name <- "Monthly"
    } 
    
    highchart() %>%
      hc_add_series(
        data = result$n, 
        type = "line",
        name = paste(group_name, " total flights"),
        events = list(click = js_click_line)) 
    
    
  })
  
  # Tracks the JavaScript event created by `js_click_line`
  observeEvent(input$line_clicked != "",
               if(input$month == 99)
                 updateSelectInput(session, "month", selected = input$line_clicked),
               ignoreInit = TRUE)
  
  js_bar_clicked <- JS("function(event) {Shiny.onInputChange('bar_clicked', [event.point.category]);}")
  
  output$top_airports <- renderD3({
    # The following code runs inside the database
    result <- db_flights %>%
      filter(carrier == input$airline) 
    
    if(input$month != 99) result <- filter(result, month == input$month) 
    
    result <- result %>%
      group_by(dest_name) %>%
      tally() %>%
      arrange(desc(n)) %>%
      collect() %>%
      head(10)
    
    r2d3(
      result,
      "bar_plot.js"
    )
    
    
  })
  
  observeEvent(input$bar_clicked,
               {
                 airport <- input$bar_clicked[1]
                 tab_title <- paste(input$airline, 
                                    "-", airport , 
                                    if(input$month != 99) paste("-" , month.name[as.integer(input$month)]))
                 
                 if(tab_title %in% tab_list == FALSE){
                   details <- db_flights %>%
                     filter(dest_name == airport,
                            carrier == input$airline)
                   
                   if(input$month != 99) details <- filter(details, month == input$month) 
                   
                   details <- details %>%
                     head(100) %>% 
                     select(month,
                            day,
                            flight,
                            tailnum,
                            dep_time,
                            arr_time,
                            dest_name,
                            distance) %>%
                     collect() %>%
                     mutate(month = month.name[as.integer(month)])
                   
                   
                   appendTab(inputId = "tabs",
                             tabPanel(
                               tab_title,
                               DT::renderDataTable(details)
                             ))
                   
                   tab_list <<- c(tab_list, tab_title)
                   
                 }
                 
                 updateTabsetPanel(session, "tabs", selected = tab_title)
                 
               })
  
  observeEvent(input$remove,{
    # Use purrr's walk command to cycle through each
    # panel tabs and remove them
    tab_list %>%
      walk(~removeTab("tabs", .x))
    tab_list <<- NULL
  })
  
}



shinyApp(ui, server)