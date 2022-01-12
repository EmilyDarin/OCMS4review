library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(htmltools)
library(tidyverse)
library(sf)
library(mapview)
library(leaflet)
library(lubridate)
library(leafsync)
library(viridisLite)
library(lubridate)
library(gridExtra)
library(stargazer)
library(EnvStats)
library(shiny)
library(kableExtra)
library(mgcv)
library(metR)
library(shinyWidgets)
library(plotly)
library(english)
library(vegan)
library(ggord)
library(patchwork)
library(plotly)
library(yaml)
library(pracma)
library(shinythemes)
library(shinycssloaders)
library(pwr)
library(purrr)


mptyps <- c("CartoDB.Positron", "CartoDB.DarkMatter", "OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap")

shinyApp(
  ui <- fluidPage(
    
    titlePanel("Hot Spots"),
    fileInput("file_upload", "Data Upload", buttonLabel = "Upload a .csv", 
              placeholder = "No File Selected...", width = "255px", 
              accept = c(".csv","text/csv")),
    leafletOutput("myMap")
  ),
  
  server = function(input, output) {
    
    my_table <- reactive({
      
      inFile <- input$file_upload
      if (is.null(inFile))
        return(NULL)
      
      myData = read.csv(inFile$datapath)
      
      df0 = data.frame(myData$StationCode, myData$Latitude, myData$Longitude)
      df = unique(df0)
      names(df)[2] = 'Latitude'
      names(df)[3] = 'Longitude'
      print(df)
      return(df)
    })
    
    output$myMap = renderLeaflet({
      if(is.null(my_table()))
      {
        return(leaflet()  %>% addProviderTiles(providers$CartoDB.Positron))
      }
      else
      {
        leaflet(data = my_table()) %>% 
          
          m <- mapview(tomap, cex = tomap$cexs, label = labs, legend = F, layer.name = F, col.regions = tomap$cols, homebutton = F, map.types = mptyps)
          
          # add legend
          out <- m@map %>% 
            addLegend("bottomright", pal = hotcol, title = leglab, opacity = 1, values = tomap$exceeds) 
          
          addProviderTiles(providers$CartoDB.Positron) %>% addMarkers()
      }
    })
  }
)