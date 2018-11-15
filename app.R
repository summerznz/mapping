library(shiny)
library(leaflet)
library(geojsonio)
library(raster)
library(tigris)
library(dplyr)
I <- read.csv("I.csv")
D <- read.csv("D.csv")
R <- read.csv("R.csv")


ui <- navbarPage("Political Party Donations", id="nav",
                 
                 tabPanel("mapping",
                          div(class="outer",
                              leafletOutput("map", width="100%", height = 585),
                              absolutePanel(top = 70,right = 50, width = 200,
                                            h2("Search"), 
                                            selectInput("var","Select Political Party",
                                                        choices=c("Democrats","Republican","Independent"))
                              ))))

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    states <- states(cb=T)
    
    if (input$var == "Democrats") {
      d <- D %>% rename(state=D_st_abrev)
      d_join <- geo_join(states, d, "STUSPS", "state")
      pal <- colorNumeric("Greens", domain = d_join$Donations)
      popup_sb <- paste0("<strong>", d_join$state, 
                         "</strong><br />Donations: ", d_join$Donations,
                         "<br />Donors: ", 
                         as.character(d_join$Donors))
      leaflet() %>%
        addTiles() %>%
        setView(-98.483330, 38.712046, zoom = 4) %>% 
        addPolygons(data = d_join , 
                    fillColor = ~pal(d_join$Donations), 
                    fillOpacity = 0.7, 
                    weight = 0.4, 
                    smoothFactor = 0.2, 
                    popup = ~popup_sb) %>%
        addLegend(pal = pal, 
                  values = d_join$Donations, 
                  position = "bottomright", 
                  title = "Donations")
      
      
      
      
    } else if (input$var == "Republican") {
      r <- R %>% rename(state=R_st_abrev)
      r_join <- geo_join(states, r, "STUSPS", "state")
      pal <- colorNumeric("Oranges", domain=r_join$Donations)
      popup_sb <- paste0("<strong>", r_join$state, 
                         "</strong><br />Donations: ", r_join$Donations,
                         "<br />Donors: ", 
                         as.character(r_join$Donors))
      leaflet() %>%
        addTiles() %>%
        setView(-98.483330, 38.712046, zoom = 4) %>% 
        addPolygons(data = r_join , 
                    fillColor = ~pal(r_join$Donations), 
                    fillOpacity = 0.7, 
                    weight = 0.2, 
                    smoothFactor = 0.2, 
                    popup = ~popup_sb) %>%
        addLegend(pal = pal, 
                  values = r_join$Donations, 
                  position = "bottomright", 
                  title = "Donations")
      
    } else if (input$var == "Independent") {
      i <- I %>% rename(state=I_st_abrev)
      i_join <- geo_join(states, i, "STUSPS", "state")
      pal <- colorNumeric("Blues", domain=i_join$Donations)
      popup_sb <- paste0("<strong>", i_join$state, 
                         "</strong><br />Donations: ", i_join$Donations,
                         "<br />Donors: ", 
                         as.character(i_join$Donors))
      leaflet() %>%
        addTiles() %>%
        setView(-98.483330, 38.712046, zoom = 4) %>% 
        addPolygons(data = i_join , 
                    fillColor = ~pal(i_join$Donations), 
                    fillOpacity = 0.7, 
                    weight = 0.2, 
                    smoothFactor = 0.2, 
                    popup = ~popup_sb) %>%
        addLegend(pal = pal, 
                  values = i_join$Donations, 
                  position = "bottomright", 
                  title = "Donations")
      
    }
    
    
    
  })
  
}

shinyApp(ui = ui,server = server)
