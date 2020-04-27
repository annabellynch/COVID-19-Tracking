#load libraries
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(formattable)
#import data
map_daily = read.csv("https://covidtracking.com/api/v1/states/daily.csv")

# Converting the values in state from a factor to a character
map_daily$state = as.character(map_daily$state)

# Converting the date from a factor to a date
#map_daily$date = as.Date(map_daily$date, format = "%m/%d/%Y")
map_daily <- transform(map_daily, date = as.Date(as.character(date), "%Y%m%d"))

# Removing unnecessary columns
map_daily = select(map_daily, date, state, positive, negative, death, total, totalTestResults)

#Removing unnecesary rows
map_daily <- subset(map_daily, state != 'MP' & state!='AS' & state!='PR' & state!='GU' & state!='VI',)

#https://www.kaggle.com/washimahmed/usa-latlong-for-state-abbreviations/data#statelatlong.csv
# State Latitude and Longitude 
state_lat_long = read.csv("C:\\Users\\student\\Downloads\\statelatlong.csv")

# Removing unnecessary columns
state_lat_long = select(state_lat_long, State, Latitude, Longitude)
names(state_lat_long)[1]<- 'state'

#Alphabetizing the state latitude and longitude data
state_lat_long = arrange(state_lat_long, state)

#Reindexing the rows
row.names(map_daily) <- NULL

#converting state value from an integer into a character
state_lat_long$state = as.character(state_lat_long$state)

# Adjusting Michigan's latitude and longitude
state_lat_long[23,2] <- 43.268750
state_lat_long[23,3] <- -84.690478

# Adjusting New York's latitude and longitude
state_lat_long[35,2] <- 43.253776
state_lat_long[35,3] <- -75.230854

#Adjusting Washington's latitude and longitude
state_lat_long[48,2] <- 47.448497
state_lat_long[48,3] <- -120.356860
#47.448497, -120.356860

#Adjusting Maryland's latitude and longitude
state_lat_long[21,2] <- 39.404101
state_lat_long[21,3] <- -77.018755
#39.404101, -77.018755

#Adjusting Virginia's latitude and longitude
state_lat_long[46,2] <- 37.712113
state_lat_long[46,3] <- -78.353607
#37.712113, -78.353607

#Adjusting Florida's latitude and longitude
state_lat_long[10,2] <- 28.287065
state_lat_long[10,3] <- -81.581208
#28.287065, -81.581208

#Adjusting Idaho's latitude and longitude
state_lat_long[14,2] <- 43.766799
state_lat_long[14,3] <- -114.291415
#43.766799, -114.291415

#Adjusting Louisiana's latitude and longitude
state_lat_long[19,2] <- 31.671375
state_lat_long[19,3] <- -92.654910
#31.671375, -92.654910


# Joining the two dataframes
map_daily = left_join(map_daily, state_lat_long)
names(map_daily)[8]<- 'latitude'
names(map_daily)[9]<- 'longitude'

#changing the format of the positive and totalTestResults so it is more readable
#map_daily$positive = comma(map_daily$positive, format = "d")
#map_daily$totalTestResults = comma(map_daily$totalTestResults, format = "d")

# creating variables for specific dates
cv_min_date = min(map_daily$date)
current_date = max(map_daily$date)
cv_today = subset(map_daily, date==current_date) 

#creating the ui
ui <- fluidPage(
  titlePanel("COVID-19 Total and Positive Tests in the United States",
             windowTitle = "COVID-19 Total and Positive Tests in the United States"),
  sidebarPanel(
    flowLayout(strong("How have COVID-19 testing and the number of positive tests in each state progressed over time?"),
               " ",
               "Large gray circles indicate the total number of tests for a specific state, while the smaller
    red circles indicate the number of positive test result in the state.",
               " ",
               "Click on a circle to see the 1) state's name, 2) number of positive test results
    and 3) number of total tests in the state.",
               " ",
               "Data Source: The COVID Tracking Project")
  ),
  mainPanel( 
    #this will create a space for us to display our map
    leafletOutput(outputId = "mymap" ), 
    #this allows me to put the checkmarks ontop of the map to allow people to view earthquake depth or overlay a heatmap
    absolutePanel( left = 20, 
                  sliderInput("plot_date",
                              label = h5("Select Mapping Date:"),
                              min = as.Date(cv_min_date,"%Y-%m-%d"),
                              max = as.Date(current_date,"%Y-%m-%d"),
                              value = as.Date(current_date),
                              timeFormat = "%d %b", 
                              animate=animationOptions(interval = 3000, loop = FALSE))
    ))
)

server <- function(input, output, session) {
  
  output$clean_date_reactive <- renderText({
    format(as.POSIXct(input$plot_date),"%d %B %Y")
  })
  
  reactive_db = reactive({
    cv_date <- filter(map_daily, date == input$plot_date)
    #reactive = cv_cases %>% filter(date == "2020-04-25")
  })
  
  #define the color pallate for the circles
  pal <- colorNumeric(
    palette = c('red'),
    domain = map_daily$positive)
  
  pal2 <- colorNumeric(
    palette = c('gray'),
    domain = map_daily$totalTestResults)
  
  #create the map
  output$mymap <- renderLeaflet({
    leaflet(map_daily) %>% 
      setView(lng = -99, lat = 45, zoom = 3.3)  %>% #setting the view over ~ center of North America
      addLayersControl(
        position = "topleft") %>% 
      addTiles() %>% 
      addCircles(data = cv_today, lat = ~ latitude, lng = ~ longitude, weight = 1, 
                 radius = ~(totalTestResults), popup = ~as.character(paste(paste(strong("State: "), state), 
                                                                           paste(strong("Positive Tests: "), positive), 
                                                                           paste(strong("Total Tests: "), totalTestResults), sep = "<br/>")), 
                 color = ~pal2(totalTestResults), fillOpacity = 0.5) %>%
      addCircles(data = cv_today, lat = ~ latitude, lng = ~ longitude, weight = 1, 
                 radius = ~(positive),  popup = ~as.character(paste(paste(strong("State: "), state), 
                                                                    paste(strong("Positive Tests: "), positive), 
                                                                    paste(strong("Total Tests: "), totalTestResults), sep = "<br/>")),
                 color = ~pal(positive), fillOpacity = 0.5)
      
  })
  

  observeEvent(input$plot_date, {
    leafletProxy("mymap") %>% 
      clearMarkers() %>%
      clearShapes() %>%
      addCircles(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, 
                 radius = ~(totalTestResults), popup = ~as.character(paste(paste(strong("State: "), state), 
                                                                           paste(strong("Positive Tests: "), positive), 
                                                                           paste(strong("Total Tests: "), totalTestResults), sep = "<br/>")), 
                 color = ~pal2(totalTestResults), fillOpacity = 0.5)%>%
      addCircles(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, 
                 radius = ~(positive),  popup = ~as.character(paste(paste(strong("State: "), state), 
                                                                    paste(strong("Positive Tests: "), positive), 
                                                                    paste(strong("Total Tests: "), totalTestResults), sep = "<br/>")),
                 color = ~pal(positive), fillOpacity = 0.5) 
      
  })
  
  
}

shinyApp(ui, server)
