#load libraries
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
#import data
map_daily = read.csv("https://covidtracking.com/api/v1/states/daily.csv")

# Converting the values in state from a factor to a character
map_daily$state = as.character(map_daily$state)

# Converting the date from a factor to a date
#map_daily$date = as.Date(map_daily$date, format = "%m/%d/%Y")
map_daily <- transform(map_daily, date = as.Date(as.character(date), "%Y%m%d"))

# Removing unnecessary columns
map_daily = select(map_daily, date, state, positive, negative, pending, death, total, totalTestResults)

#Removing unnecesary rows
map_daily <- subset(map_daily, state != 'MP' & state!='AS' & state!='PR' & state!='GU' & state!='VI',)

#https://www.kaggle.com/washimahmed/usa-latlong-for-state-abbreviations/data#statelatlong.csv
# State Latitude and Longitude 
state_lat_long = read.csv("C:\\Users\\student\\Downloads\\statelatlong.csv")

# Removing unnecessary columns
state_lat_long = select(state_lat_long, State, Latitude, Longitude)
names(state_lat_long)[1]<- 'state'

# Joining the two dataframes
map_daily = left_join(map_daily, state_lat_long)
names(map_daily)[9]<- 'latitude'
names(map_daily)[10]<- 'longitude'

cv_min_date = min(map_daily$date)
current_date = max(map_daily$date)
# creat variable for today's data
cv_today = subset(map_daily, date==current_date) 

#creating the ui
ui <- fluidPage(
  mainPanel( 
    #this will create a space for us to display our map
    leafletOutput(outputId = "mymap"), 
    #this allows me to put the checkmarks ontop of the map to allow people to view earthquake depth or overlay a heatmap
    absolutePanel( left = 20, 
                  sliderInput("plot_date",
                              label = h5("Select mapping date"),
                              min = as.Date(cv_min_date,"%Y-%m-%d"),
                              max = as.Date(current_date,"%Y-%m-%d"),
                              value = as.Date(current_date),
                              timeFormat = "%d %b", 
                              animate=animationOptions(interval = 3000, loop = FALSE))
    )
  ))

server <- function(input, output, session) {
  
  # covid tab 
  output$clean_date_reactive <- renderText({
    format(as.POSIXct(input$plot_date),"%d %B %Y")
  })
  
  reactive_db = reactive({
    cv_date <- filter(map_daily, date == input$plot_date)
    #reactive = cv_cases %>% filter(date == "2020-04-25")
  })
  
  
  
  #define the color pallate for the magnitidue of the earthquake
  pal <- colorNumeric(
    palette = c('red'),
    domain = map_daily$positive)
  
  #create the map
  output$mymap <- renderLeaflet({
    leaflet(map_daily) %>% 
      setView(lng = -99, lat = 45, zoom = 3)  %>% #setting the view over ~ center of North America
      addLayersControl(
        position = "topleft") %>% 
      addTiles() %>% 
      addCircles(data = cv_today, lat = ~ latitude, lng = ~ longitude, weight = 1, 
                 radius = ~(positive), popup = ~as.character(positive), 
                 label = ~as.character(paste0("Positive Cases: ", sep = " ", positive)), 
                 color = ~pal(positive), fillOpacity = 0.5)
  })
  

  observeEvent(input$plot_date, {
    leafletProxy("mymap") %>% 
      clearMarkers() %>%
      clearShapes() %>%
      addCircles(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, 
                 radius = ~(positive), popup = ~as.character(positive), 
                 label = ~as.character(paste0("Positive Cases: ", sep = " ", positive)), 
                 color = ~pal(positive), fillOpacity = 0.5)
  })
  
  
}

shinyApp(ui, server)
