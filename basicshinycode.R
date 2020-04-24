# Aditya Kannoth (ank4an)

library(shiny)


# read data in here ----------

# user interface
ui <- fluidPage(
  titlePanel("US Coronavirus Map"), #basic title panel
  
  sidebarLayout(
    sidebarPanel(#inserts a slider for the date
      sliderInput(inputId = "WeMakeThisRange", 
                  label = "Date",
                  min = as.Date("3/4/2020","%m/%d/%y"),
                  max = as.Date("4/23/2020","%m/%d/%y"),
                  value = as.Date("3/4/2020")
      )
    )
  ),
  
  mainPanel(# main plot is called "map" now, output$map is dependent on this being called "map"
    plotOutput("map")
  )
)

# server logic
server <- function(input, output) {
  output$map <- renderPlot({
    # actual map code should go here
    
    # WeMakeThisRange <- input$WeMakeThisRange
    
  })
}

shinyApp(ui, server)