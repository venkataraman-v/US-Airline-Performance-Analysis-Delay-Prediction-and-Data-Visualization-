library(shinydashboard)
library(dplyr)
library(dbplyr)
library(purrr)
library(shiny)
library(highcharter)
library(DT)
library(htmltools)
library(data.table)
library(caret)
library(leaflet)
library(sf)
library(ggplot2)
library(shinycssloaders)
library(dplyr)
library(data.table)
library(datasets)
library(shinyWidgets)

source('DATA_PREPROCESSING.R')
source('LOGISTIC_REGRESSION_.R')

options(warn = -1)
ui <- shinyUI(fluidPage(tags$head(
    disable = TRUE,
    tags$style(HTML('.navbar {margin-bottom:0 !important;}
                    .container-fluid{padding:0 !important}
                    .navbar-header{padding-left:15px}
                    #map{position:relative !important}
                    #text{margin-left: 15px;margin-right: 15px;padding: 20px;font-size: 20px;background-color: #001f3f;color: #ffffff; margin-bottom: 15px;border-radius: 15px;} .content-wrapper{height: -webkit-fill-available}
                    #predDelay{margin-left: 15px;padding: 160px 20px 50px;font-size: 20px;background-color: #001f3f;color: #ffffff;margin-bottom: 15px;border-radius: 15px;    width: 97%;opacity: 1}
                    .tab-pane{font-family: auto !important;}
                    '))
),
navbarPage(
    "Flights Dashboard",
    tabPanel("Airline Performance Analysis",
             dashboardPage(skin = "blue",
                           dashboardHeader(disable = TRUE),
                           dashboardSidebar(
                               selectInput(
                                   inputId = "airline",
                                   label = "Airline:", 
                                   choices = unique(db_flights$AIRLINE_NAME), 
                                   selectize = FALSE),
                               sidebarMenu(
                                   selectInput(
                                       "month",
                                       "Month:", 
                                       list(
                                           "All Year" = 99,
                                           "January" = 1,
                                           "February" = 2,
                                           "March" = 3,
                                           "April" = 4,
                                           "May" = 5,
                                           "June" = 6,
                                           "July" = 7,
                                           "August" = 8,
                                           "September" = 9,
                                           "October" = 10,
                                           "November" = 11,
                                           "December" = 12
                                       ), 
                                       selected =  "All Year", 
                                       selectize = FALSE),
                                   actionLink("remove", "Close detail tabs")
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
                                                   column(width = 7,
                                                          p(textOutput("monthly")),
                                                          highchartOutput("group_totals")),
                                                   column(width = 5,
                                                          p("Click on an airport in the plot to see the details"),
                                                          highchartOutput("top_airports"))
                                               )
                                           )
                               )
                           )
             )),
    tabPanel("Travel Delay Analysis",
             dashboardPage(skin = "blue",
                           dashboardHeader(disable = TRUE),
                           dashboardSidebar(
                               selectInput("OriginAirport", "Origin Airport",
                                           choices = unique(db_flights$ORIGIN_CITY_NAME),
                                           selected = "Chicago, IL", multiple = FALSE,
                                           selectize = TRUE),
                               sliderInput(inputId = "mins",
                                           label = "Allowable delay in minutes",
                                           min = 0,
                                           max = 100,
                                           value = 15),
                               selectInput(
                                   inputId = "DestState",
                                   label = "Destination State:", 
                                   choices = unique(db_flights$DEST_STATE_NM), 
                                   selected = "New York",
                                   selectize = FALSE)
                           ),
                           dashboardBody(
                               fluidRow(
                                
                               textOutput("text"),
                               ),
                               fluidRow(
                               box(width=6,height="50%",withSpinner(leafletOutput("map", width = "100%")),p("Size of the dot represents the delay percentage to the destination")),
                               box(width=6,height="50%",title = "On Time Vs Delayed Flights",withSpinner(plotOutput("doughnutplot", width = "100%")))
                               ),
                               fluidRow(
                               box(width=12,height="50%",withSpinner(highchartOutput("bardelay", width = "100%")))
                                ),
                               fluidRow(
                                        box(width=12,height="50%",withSpinner(highchartOutput("linedelay", width = "100%")))
                               )
                           )
             )),
    
    tabPanel("Predictive Analytics",
             dashboardPage(skin = "blue",

                           dashboardHeader(disable = TRUE),
                           dashboardSidebar(
                               selectInput(
                                   inputId = "predict_airline",
                                   label = " Choose an Airline:", 
                                   choices = list(
                                       "Frontier Airlines Inc."="F9",
                                       "Alaska Airlines Inc."="AS",
                                       "JetBlue Airways"="B6",
                                       "Delta Air Lines Inc."="DA",
                                       "American Airlines Inc."="AA",
                                       "Hawaiian Airlines Inc."="HA",
                                       "Spirit Air Lines"="NK",
                                       "SkyWest Airlines Inc."="OO",
                                       "United Air Lines Inc."="UA",
                                       "Southwest Airlines Co."="WN",
                                       "Republican Airline"="YX"
                                   ),
                                   selectize = FALSE),
                               
                               
                               selectInput(
                                   "predict_month",
                                   "Choose a Month:", 
                                   list(
                                       "January" = 1,
                                       "February" = 2,
                                       "March" = 3,
                                       "April" = 4,
                                       "May" = 5,
                                       "June" = 6,
                                       "July" = 7,
                                       "August" = 8,
                                       "September" = 9,
                                       "October" = 10,
                                       "November" = 11,
                                       "December" = 12
                                   ), 
                                   selected =  "January", 
                                   selectize = FALSE)
                               
                           ),
                           
                           dashboardBody(
                              
                               box(width=4,sliderInput(inputId = "predict_hour",
                                                       label = "Choose an Hour:",
                                                       min = 5,
                                                       max = 23,
                                                       value = 5)),
                               box(width=4,sliderInput(inputId = "predict_distance",
                                                       label = "Select distance to DESTINATION (miles):",
                                                       min = 80,
                                                       max = 5000,
                                                       value = 677)),
                               box(width=4,sliderInput(inputId = "predict_airtime",
                                                       label = "Select your Travel Time (minutes):",
                                                       min = 18,
                                                       max = 412,
                                                       value = 25)),
                               textOutput("predDelay")
                               
                           )
             )),
    tabPanel("Project Info", fluid = TRUE, 
             dashboardPage(skin = "blue",
               dashboardHeader(disable = TRUE),
               dashboardSidebar(
                   sidebarMenu(
                       menuItem("About Project", tabName = "about"),
                       menuItem("App Guide",tabName = "appguide"),
                       menuItem("Links", tabName = "links")
                   )
               ),
               dashboardBody(
                   tabItems(
                       tabItem(tabName = "about",
                               h1(strong("About this project")),
                               p("The purpose of this project is to visually analyze the performance of top 4 domestic airlines in the United States and build a model to predict the number of minutes a flight can be delayed. The complete dataset consists of Airline, Airport, and Flight Itinerary was obtained from the Bureau of Transportation Statistics (BTS) repository for the year 2019. Datasets for a location's longitude and latitude coordinates were also taken from the same source. "),
                               p("The initial airline, airport, and flight dataset consist of 7.5 million (approx.) rows and 110 columns. This data was preprocessed by removing unwanted dimensions and null values followed by subsetting the top 4 Airlines (American Airline, Delta Airlines, Southwest Airlines, and Spirit Airlines). To reduce further complexity, we subset the Origin State and Destination State including New York, California, Washington, Texas, Alaska, Hawaii, Florida, Massachusetts, Illinois. Data cleaning was also done to remove missing values."),
                               p("Several data visualizations including line graph and bar graph are included to analyze the Airline's monthly flight movement, flights delayed, and average flights per day. Visualizations are also developed to analyze the flight delays between an Origin City and a Destination State. This was done by building a map, doughnut chart and a multi-bar chart that compares all flights with flights which are delayed."),
                               p("And further, implemented a Logistic regression algorithm to predict the delay time based on the distance between the airports, flight time, hour of the day, month and airline.")
                       ),
                       
                       tabItem(tabName = "appguide",
                               h1(strong("How to Use App")),
                               p("The Application consists of three main components."),
                               h3("Airline Performance Analysis Dashboard:"),
                               p("This part of the application depicts the information about the monthly flight operations of the particular Airline. Consisting of two interactive plots (bar graph and line graph), this section presents the information on the total number of flights operated, the average number of flights per day for the chosen Airline and month. The click event on the bar graph will open a list of the flights from the particular origin point. Similarly, the click event on the line graph shows a more detailed picture of the number of flights per month. This tab has two input fields, Airline name and month."),
                               h3("Travel Delay Analysis:"),
                               p("This section highlights the percentage of delayed flights from the chosen airport to the destination state. Information is presented in the form of four plots viz map visualization, doughnut chart, multi-barred bar chart and line chart. Origin and destination airports on the map are represented with the marker and the dots respectively. Size of the dot indicates the percentage of delay flights to that airport. The doughnut plot compares the total flights and delayed flights between chosen origin and destination airports. Similarly, bar and line charts visualize the total flights and delayed flights with regards to the Airlines and month respectively. This tab allows users to choose from and to airports and allowable delay time. The application also has an alerting mechanism to indicate that there are no flights between the given source and destinations."),
                               h3("Predictive Analytics:"),
                               p("This tab is to predict the delay times of the flights based on the historical data. The output for this tab is based on the model being trained using logistic regression  Functionality has been built to accept Airline operator name, Month, Air time, Distance between the ports and hour in the day, with the model predicting delay time of each flight, with Logistic Regression in the background.")
                        ),
                       tabItem(tabName = "links",
                               h1(strong("Data Source:")),
                               p(a("Bureau of Transportation Statistics: Reporting Airline Carrier On-Time Performance", 
                                   href = "https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236")),
                               p(a("Bureau of Transportation Statistics: Airport Location Details", 
                                   href = "https://www.transtats.bts.gov/Fields.asp?Table_ID=288")),
                               h1(strong("Project GitHub:")),
                               p(a("Project Implementation:  GitHub", 
                                   href = "https://github.com/Raghuramas94/Data-Visualization/tree/final"))
                       
                       )
               )
            )
            ))
)
))

server <- function(input, output, session) { 
    tab_list <- NULL
    
    output$monthly <- renderText({
        if(input$month == "99")"Click on a month in the plot to see the daily counts"
    })
    
    observeEvent(input$OriginAirport, {
        data1 <-db_flights[db_flights$ORIGIN_CITY_NAME %in% input$OriginAirport & db_flights$DEST_STATE_NM %in% input$DestState,]
        if(nrow(data1) == 0) 
        {
            showModal(modalDialog(
                title = paste("Oops! There are no flighs from ", input$OriginAirport, " to", input$DestState," state" ),
                "Please select other set of inputs",
                selectInput("OriginAirportpopup", "Origin Airport",
                            choices = unique(db_flights$ORIGIN_CITY_NAME),
                            selected = NULL, multiple = FALSE,
                            selectize = TRUE),
                selectInput("DestStatepopup", "Destination State",
                            choices = unique(db_flights$DEST_STATE_NM),
                            selected = NULL, multiple = FALSE,
                            selectize = TRUE),
                footer = tagList(
                    actionButton("popupok", "OK")
                )
            ))
        }
    })
    observeEvent(input$DestState, {
        data1 <-db_flights[db_flights$ORIGIN_CITY_NAME %in% input$OriginAirport & db_flights$DEST_STATE_NM %in% input$DestState,]
        if(nrow(data1) == 0) 
        {
            showModal(modalDialog(
                title = paste("Oops! There are no flighs from ", input$OriginAirport, " to", input$DestState," state" ),
                "Please select other set of inputs",
                selectInput("OriginAirportpopup", "Origin Airport",
                            choices = unique(db_flights$ORIGIN_CITY_NAME),
                            selected = NULL, multiple = FALSE,
                            selectize = TRUE),
                selectInput("DestStatepopup", "Destination State",
                            choices = unique(db_flights$DEST_STATE_NM),
                            selected = NULL, multiple = FALSE,
                            selectize = TRUE),
                footer = tagList(
                    actionButton("popupok", "OK")
                )
            ))
        }
    })
    observeEvent(input$popupok, {
        updateSelectInput(session,"OriginAirport", "Origin Airport",
                          choices = unique(db_flights$ORIGIN_CITY_NAME),
                          selected = as.character(input$OriginAirportpopup[1]))
        updateSelectInput(session,"DestState", "Destination State",
                          choices = unique(db_flights$DEST_STATE_NM),
                          selected = as.character(input$DestStatepopup[1]))
        
        removeModal()
    })
    
    
    output$total_flights <- renderValueBox({
        # The following code runs inside the database
        result <- db_flights %>%
            filter(AIRLINE_NAME == input$airline)
        
        if(input$month != 99) result <- filter(result, MONTH == input$month)
        
        result <- result %>%
            tally() %>%
            pull() %>% 
            as.integer()
        
        valueBox(value = prettyNum(result, big.mark = ","),
                 subtitle = "Number of Flights",
                 color = "navy")
    })
    #navy, teal, olive, lime, orange, fuchsia, purple, maroon
    output$per_day <- renderValueBox({
        
        # The following code runs inside the database
        result <- db_flights %>%
            filter(AIRLINE_NAME == input$airline)
        
        if(input$month != 99) result <- filter(result, MONTH == input$month)
        
        result <- result %>%
            group_by(DAY_OF_MONTH, MONTH) %>%
            tally() %>%
            summarise(avg = mean(n)) %>% 
            pull()
        
        valueBox(prettyNum(mean(result), big.mark = ","),
                 subtitle = "Average Flights Per Day",
                 color = "teal")
    })
    
    
    
    output$percent_delayed <- renderValueBox({
        
        # The following code runs inside the database
        result <- db_flights %>%
            filter(AIRLINE_NAME == input$airline)
        
        if(input$month != 99) result <- filter(result, MONTH == input$month)
        result <- result %>%
            filter(!is.na(DEP_DELAY)) %>%
            mutate(delayed = ifelse(DEP_DELAY >= 15, 1, 0)) %>%
            summarise(delays = sum(delayed),
                      total = n()) %>%
            mutate(percent = delays / total) %>%
            pull()
        
        valueBox(paste0(round(result * 100), "%"),
                 subtitle = "Flights delayed",
                 color = "fuchsia")
    })
    
    js_click_line <- JS("function(event) {Shiny.onInputChange('line_clicked', [event.point.category]);}")
    
    output$group_totals <- renderHighchart({
        
        if(input$month != 99) {
            result <- db_flights %>%
                filter(MONTH == input$month,
                       AIRLINE_NAME == input$airline) %>%
                group_by(DAY_OF_MONTH) %>%
                tally() %>%
                collect()
            group_name <- "Daily"
            categories <- result$DAY_OF_MONTH
            title <- paste("Daily distribution of flights operated by ",input$airline, " in ", month.abb[input$month] ,"2019")
        } else {
            result <- db_flights %>%
                filter(AIRLINE_NAME == input$airline) %>%
                group_by(MONTH) %>%
                tally() %>%
                collect()    
            group_name <- "Monthly"
            categories <- month.abb[result$MONTH]
            title <- paste("Monthly distribution of flights operated by ",input$airline, " in 2019")
        } 
        
        highchart() %>%
            hc_add_series(
                data = result$n, 
                type = "line",
                name = paste(group_name, " total flights"),
                events = list(click = js_click_line)
            ) %>%
            hc_xAxis(categories = categories)
        
        
    })
    
    observeEvent(input$line_clicked != "",
                 if(input$month == 99)
                     updateSelectInput(session, "month", selected = match(input$line_clicked,month.abb)),
                 ignoreInit = TRUE)
    
    js_bar_clicked <- JS("function(event) {Shiny.onInputChange('bar_clicked', [event.point.category]);}")
    
    output$top_airports <- renderHighchart({
        result <- db_flights %>%
            filter(AIRLINE_NAME == input$airline) 
        
        if(input$month != 99) result <- filter(result, MONTH == input$month) 
        
        result <- result %>%
            group_by(DEST_CITY_NAME) %>%
            tally() %>%
            arrange(desc(n)) %>%
            collect() %>%
            head(10)
        
        highchart() %>%
            hc_add_series(
                data = result$n, 
                type = "bar",
                name = paste("No. of Flights"),
                events = list(click = js_bar_clicked)) %>%
            hc_xAxis(
                categories = result$DEST_CITY_NAME,
                tickmarkPlacement="on")
        
    })
    
    observeEvent(input$bar_clicked,
                 {
                     airport <- input$bar_clicked[1]
                     tab_title <- paste(input$airline, 
                                        "-", airport , 
                                        if(input$month != 99) paste("-" , month.name[as.integer(input$month)]))
                     
                     if(tab_title %in% tab_list == FALSE){
                         details <- db_flights %>%
                             filter(DEST_CITY_NAME == airport,
                                    AIRLINE_NAME == input$airline)
                         
                         if(input$month != 99) details <- filter(details, MONTH == input$month) 
                         
                         details <- details %>% 
                             select(MONTH,
                                    DAY_OF_MONTH,
                                    AIRLINE_NAME,
                                    TAIL_NUM,
                                    DEP_DELAY,
                                    ARR_DELAY,
                                    DEST_CITY_NAME,
                                    AIR_TIME) %>%
                             collect() %>%
                             mutate(MONTH = month.name[as.integer(MONTH)])
                         
                         
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
        tab_list %>%
            walk(~removeTab("tabs", .x))
        tab_list <<- NULL
    })
    
    
    Selectedlocation <- reactive({
        data1 <-db_flights[db_flights$ORIGIN_CITY_NAME %in% input$OriginAirport & db_flights$DEST_STATE_NM %in% input$DestState,]})
    
    
    
    output$map <- renderLeaflet({
        data1<- Selectedlocation()
        deldet <- data1 %>% filter(!is.na(DEP_DELAY)) %>%
            mutate(delayed = ifelse(DEP_DELAY >= input$mins, 1, 0)) %>% group_by(DEST_CITY_NAME) %>%
            summarise(delays = sum(delayed),
                      total = n()) %>% mutate(percent = delays / total)
        data1 <- merge(data1, deldet, by="DEST_CITY_NAME", all=TRUE)
        data1<-data1[data1$DEP_DELAY < input$mins,]
        if(nrow(data1)!=0){
            m <- leaflet(data1) %>%
                
                addTiles() %>%  # Add default OpenStreetMap map tiles
                setView(lng = data1$LONGITUDE_ORIGIN[1], lat = data1$LATITUDE_ORIGIN[1], zoom = 3) %>%
                addMarkers(lng=data1$LONGITUDE_ORIGIN[1], lat=data1$LATITUDE_ORIGIN[1], popup=data1$ORIGIN_CITY_NAME[1]) %>%
                addCircleMarkers(
                    lng=~LONGITUDE_DEST, # Longitude coordinates
                    lat=~LATITUDE_DEST, # Latitude coordinates
                    radius=~percent*20, # Total count
                    stroke=FALSE, # Circle stroke
                    color = "Black",
                    fillOpacity=~total, # Circle Fill Opacity
                    popup = ~paste("<b>","City:","</b>",DEST_CITY_NAME,"<br>",
                                   "<b>","Total Flights:","</b>",as.character(total),"<br>",
                                   "<b>","Delayed Flights:","</b>",as.character(delays),"<br>")
                    
                ) %>%
                addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE))
        } else{
            m<-leaflet()%>% addTiles()%>% setView(lng=-94.6,lat=39.15,zoom=3) 
        }
        m
    })
    
    
    output$doughnutplot <- renderPlot({
        
        data1<- db_flights[db_flights$ORIGIN_CITY_NAME %in% input$OriginAirport & db_flights$DEST_STATE_NM %in% input$DestState,]
        deldet <- data1 %>% filter(!is.na(DEP_DELAY)) %>%
            mutate(delayed = ifelse(DEP_DELAY >= input$mins, 1, 0)) %>% group_by(DEST_CITY_NAME) %>%
            summarise(delays = sum(delayed),
                      total = n()) %>% mutate(percent = delays / total)
        data1 <- merge(data1, deldet, by="DEST_CITY_NAME", all=TRUE)
        
        data <- data.frame(
            category=c("On time", "Delayed"),
            count=c(data1$total[1]-data1$delays[1], data1$delays[1])
        )
        # Compute percentages
        data$fraction <- data$count / sum(data$count)
        
        # Compute the cumulative percentages (top of each rectangle)
        data$ymax <- cumsum(data$fraction)
        
        # Compute the bottom of each rectangle
        data$ymin <- c(0, head(data$ymax, n=-1))
        
        # Compute label position
        data$labelPosition <- (data$ymax + data$ymin) / 2
        
        # Compute a good label
        data$label <- paste0(data$category, "\n value: ", data$count)
        
        # Make the plot
        
        ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
            geom_rect() +
            geom_label( x=3.5, aes(y=labelPosition, label=label), size=6, color = "#ffffff") +
            coord_polar(theta="y") +
            xlim(c(2, 4)) +
            theme_void() +
            theme(legend.position = "none")+
            scale_fill_manual(values = alpha(c("#c85a54", "#65499c")))
    })
    
    output$bardelay<-renderHighchart({
        data1<- db_flights[db_flights$ORIGIN_CITY_NAME %in% input$OriginAirport & db_flights$DEST_STATE_NM %in% input$DestState,]
        deldet <- data1 %>% filter(!is.na(DEP_DELAY)) %>%
            mutate(delayed = ifelse(DEP_DELAY >= input$mins, 1, 0)) %>% group_by(AIRLINE_NAME) %>%
            summarise(delays = sum(delayed),
                      total = n()) %>% mutate(percent = delays / total)
        
        highchart() %>% 
            hc_chart(type = "column") %>%
            
            hc_xAxis(categories = unique(deldet$AIRLINE_NAME)) %>%
            hc_add_series(name="Total Flights",
                          data = deldet$total) %>%
            hc_add_series(name="On time Flights",
                          data = deldet$total-deldet$delays) %>%
            hc_add_series(name="Delayed Flights",
                          data = deldet$delays) %>%
            hc_title(text = "Delay Analysis with respect to Airlines")
    })
    output$linedelay<-renderHighchart({
        data1<- db_flights[db_flights$ORIGIN_CITY_NAME %in% input$OriginAirport & db_flights$DEST_STATE_NM %in% input$DestState,]
        deldet <- data1 %>% filter(!is.na(DEP_DELAY)) %>%
            mutate(delayed = ifelse(DEP_DELAY >= input$mins, 1, 0)) %>% group_by(MONTH) %>%
            summarise(delays = sum(delayed),
                      total = n()) %>% mutate(percent = delays / total)
   
        highchart() %>% 
            hc_chart(type = "line") %>%
            
            hc_xAxis(categories = month.abb[unique(deldet$MONTH)]) %>%
            hc_add_series(name="Total Flights",
                          data = deldet$total) %>%
            hc_add_series(name="On time Flights",
                          data = deldet$total-deldet$delays) %>%
            hc_add_series(name="Delayed Flights",
                          data = deldet$delays) %>%
            hc_title(text = paste("Monthly delay analysis between ",input$OriginAirport, " and ", input$DestState))
    })
    output$text <- renderText({
        data1<- db_flights[db_flights$ORIGIN_CITY_NAME %in% input$OriginAirport,]
        deldet <- data1 %>% filter(!is.na(DEP_DELAY)) %>%
            mutate(delayed = ifelse(DEP_DELAY >= input$mins, 1, 0)) %>% group_by(DEST_CITY_NAME) %>%
            summarise(delays = sum(delayed),
                      total = n()) %>% mutate(percent = delays / total)
        percent <- round(sum(deldet$delays)/sum(deldet$total)*100, digits = 0)
        return(paste(percent,"% of flights from ", input$OriginAirport, "are reaching", input$DestState, "with a delay more than", input$mins, " minutes"))
        
    })
    output$predDelay <- renderText({
        test1 <- data.frame("MONTH"=as.factor(input$predict_month),"OP_UNIQUE_CARRIER"=input$predict_airline,"HOUR"=as.numeric(input$predict_hour),"DISTANCE"=as.numeric(input$predict_distance),"AIR_TIME"=as.numeric(input$predict_airtime))
        log_reg_predict <- predict(log_reg_mod, test1)
        return(paste("Expected Delay in minutes = ", round(log_reg_predict, digits=5)))
    })
    
}



shinyApp(ui, server)