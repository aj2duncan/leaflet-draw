# load packages
library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(sf)
library(shinyjs)

# Define UI for application that draws a histogram
ui <- bootstrapPage(
    useShinyjs(),
    fluidRow(leafletOutput("map")),
    hidden(
        div(id = "downloaddiv",
            fluidRow(
                downloadButton("downloadData", "Download Shape File")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            setView(lng = -5.3, lat = 56.3, zoom = 6) %>%
            addDrawToolbar(rectangleOptions = FALSE,
                           editOptions = editToolbarOptions(edit = FALSE,
                                                            remove = TRUE),
                           polylineOptions = FALSE,
                           circleOptions = FALSE,
                           circleMarkerOptions = FALSE,
                           markerOptions = FALSE)
    })
    
    observeEvent(input$map_draw_new_feature, {
        show("downloaddiv")
    })

    output$downloadData <- downloadHandler(
        filename = function() {
            paste("shapefile", "zip", sep=".")
        },
        content = function(file) {
            temp_shp <- tempdir()
            geo = input$map_draw_new_feature$geometry$coordinates[[1]]
            lng = map_dbl(geo, `[[`, 1)
            lat = map_dbl(geo, `[[`, 2)
            shp = st_as_sf(tibble(lon = lng, lat = lat), 
                           coords = c("lon", "lat"),
                           crs = 4326) %>%
                summarise(geometry = st_combine(geometry)) %>%
                st_cast("POLYGON")

            shp_files <- list.files(temp_shp, "shapefile*", 
                                    full.names = TRUE)
            if(length(shp_files) != 0) {
                file.remove(shp_files)
            }
            st_write(shp, paste(temp_shp, "shapefile.shp", sep = "\\"))
            # copy the zip file to the file argument
            shp_files <- list.files(temp_shp, "shapefile*", 
                                    full.names = TRUE)
            zip(zipfile = file, files = shp_files, flags = "-j")
            file.remove(shp_files)
        }
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
