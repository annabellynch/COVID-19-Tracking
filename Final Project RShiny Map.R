#load libraries
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
#import data
map_daily = read.csv("C:\\Users\\student\\Documents\\Semester 4\\SYS 2202\\Final Project\\4.23 states daily.csv")

# Converting the values in state from a factor to a character
map_daily$state = as.character(map_daily$state)

# Converting the date from a factor to a date
map_daily$date = as.Date(map_daily$date, format = "%m/%d/%Y")

# Removing unnecessary columns
map_daily = select(daily, date, state, positive, negative, pending, death, total, totalTestResults)


# State Latitude and Longitude 
state_lat_long = read.csv("C:\\Users\\student\\Documents\\Semester 4\\SYS 2202\\Final Project\\state_lat_long.csv")

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
    absolutePanel(top = 60, left = 20, 
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
  #define the color pallate for the magnitidue of the earthquake
  pal <- colorNumeric(
    palette = c('red'),
    domain = map_daily$positive)
  
  #create the map
  output$mymap <- renderLeaflet({
    leaflet(map_daily) %>% 
      setView(lng = -99, lat = 45, zoom = 2)  %>% #setting the view over ~ center of North America
      addTiles() %>% 
      addCircles(data = cv_today, lat = ~ latitude, lng = ~ longitude, weight = 1, 
                 radius = ~(cv_today$positive)^(1/4), popup = ~as.character(cv_today$positive), 
                 label = ~as.character(paste0("Positive Cases: ", sep = " ", cv_today$positive)), 
                 color = ~pal(cv_today$positive), fillOpacity = 0.5)
  })
  
  observeEvent(input$plot_date, {
    leafletProxy("mymap") %>% 
      clearMarkers() %>%
      clearShapes() %>%
      addCircles(data = map_daily, lat = ~ latitude, lng = ~ longitude, weight = 1, 
                 radius = ~sqrt((map_daily$positive)), popup = ~as.character(map_daily$positive), 
                 label = ~as.character(paste0("Positive Cases: ", sep = " ", map_daily$positive)), 
                 color = ~pal(map_daily$positive), fillOpacity = 0.5)
  })
  
  
  
  #observe({
    #date <- as.Date(paste0("2020-04-", input$n))
    #updateDateInput(session, "inDate",
                    #label = paste("Date label", input$n),
                    #value = date,
                    #min   = date - 3,
                    #max   = date + 3
    #)
  #})
  
}

shinyApp(ui, server)
