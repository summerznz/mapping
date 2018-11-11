library(shiny)
library(leaflet)
boston_food <- read.csv("boston1.csv")
boston_food <- boston_food[1:500,]
#########
ui <- bootstrapPage(
  navbarPage("Boston Food Inspection", 
  tabPanel("Mapping",
  leafletOutput("map", width = "100%", height = 600),
  absolutePanel(top = 55, right = 25,
                h2("Inspection Search"), 
                sliderInput("count", "Violation counts", min(boston_food$count), max(boston_food$count),
                            value = range(boston_food$count)),
                selectInput("city", "Choose a City", boston_food$CITY))
)))

#########
server <- function(input, output, session) {
  
  getColor <- function(boston_food) {
    sapply(boston_food$count, function(count) {
      if(count >= 10 & count <20) {
        "beige"
      } else if(count < 10 & count >=1){
        "green"
      } else if( count >=20 & count <100){
        "purple"
      } else if( count >=100 & count <234){
        "blue"
      } 
    })
  }
  icons <- awesomeIcons(
    icon = "coffee",
    iconColor = 'white',
    library = 'ion',
    markerColor = getColor(boston_food)
  )
  
  ########  
  filteredData <- reactive({
    boston_food[boston_food$count >= input$count[1] & boston_food$count <= input$count[2],]  

     })
  
  
  #########  
  output$map <- renderLeaflet({
    
    leaflet(boston_food) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))  %>% 
      addLegend("bottomright",colors=c("green","beige","violet","blue"),
                labels = c("<10","10~20","20~100",">100"),title = "Violation Counts", opacity = 1) %>%  
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479")
  })
  
  #########
  observe({
   
    
    leafletProxy("map", data=filteredData()) %>% 
      clearMarkers() %>%
      addAwesomeMarkers(data=filteredData(), ~ long, ~lat, 
                        icon=icons, popup = ~as.character(businessName), 
                        options = popupOptions(closeButton = FALSE), label = ~paste(Address)
      )}
  )
}
shinyApp(ui, server)