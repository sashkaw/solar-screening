#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#clear environment
rm(list = ls())

#install packages
library(tidyverse)
library(sf)
library(leaflet)
library(shiny)

#https://github.com/r-spatial/sf/issues/1762
#https://grantmcdermott.com/ds4e/spatial-analysis.html
sf_use_s2(FALSE)

#read in the data
inputDir <- "solar-screening/data/clean"
pvShpName <- "pv_sir_10000kw_summaryByCounty_ProjWGS.shp"
countyShpName <- "/2018_us_county_20m_FixGeom_FilterOR_ProjWGS.shp"
pvPath <- paste0(inputDir, "/", pvShpName) #savings to investment ratio data
countyPath <- paste0(inputDir, "/", countyShpName) #us county boundaries

#set sf precision (to speed up processing time) and project to commmon CRS for Oregon (2992)
pvShp <- st_set_precision(st_read(pvPath), 1e8)
countyShp <- st_set_precision(st_read(countyPath), 1e8)

#plot the result (note: switching to AGG graphics device may speed up sf plotting)

#plot with base r:
#plot(shp1OR["pvsir10kw"])

#plot with ggplot:
# ggplot(data = shp2OR) +
#   geom_sf() +
#   title("Solar Screening Data") +
#   geom_sf(data = shp1OR, aes(fill = pvsir10kw)) +
#   scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
#   geom_sf_label(aes(label = NAME), alpha = .7)

#plot with leaflet: (have to make sure input doesn't contain geometry collection and that crs of inputs is WGS84)
# leaflet() %>% 
#   addPolygons(data = (shp1CleanWGS))


#create shiny app UI
ui <- fluidPage(
  titlePanel("Oregon Solar Savings to Investment Ratio By County (10 kw systems"),
  verticalLayout(
    mainPanel(
      leafletOutput("solarMap")
    )
  )
)

#create shiny server function
server <- function(input, output, session) {
  
  #colors for solar screening data
  # shp1Pal <- colorQuantile(
  #   palette = "magma", 
  #   domain = pvShp$pvsir10kw)
  shp1Pal <- colorFactor(
    palette = "magma",
    domain = pvShp$pv10kwB
  )
  
  #colors for counties data
  shp2Pal <- colorFactor(
    palette = "viridis",
    domain = countyShp$NAME)
  
  output$solarMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Voyager") %>% 
      
      addPolygons(data = pvShp,
                  fillOpacity = 0.5,
                  fillColor = ~shp1Pal(pv10kwB),
                  color = NA
                  #fillColor =  ~shp1Pal(shp1ORProj4326$pvsir10kw),
                  # highlightOptions = highlightOptions(color = "white",
                  #                                     weight = 2,
                  #                                     bringToFront = FALSE))
      ) %>%
      addPolygons(data = countyShp,
                  #fillOpacity = 0.5,
                  #fillColor =  ~shp2Pal(NAME),
                  #fillColor = NA,
                  color =  ~shp2Pal(NAME),
                  #color = "grey",
                  weight = 5,
                  label = ~NAME,
                  labelOptions = labelOptions(noHide = TRUE, 
                                              textOnly = TRUE, 
                                              opacity = 1 , 
                                              #textsize='8px', 
                                              style = list(color = "white", weight = 1))
                  # highlightOptions = highlightOptions(color = "white",
                  #                                     weight = 2,
                  #                                     bringToFront = TRUE)
      ) %>%
      
      addLegend(data = pvShp, 
                "bottomright", 
                pal = shp1Pal, 
                values = ~pv10kwB,
                title = "Savings to Investment Ratio",
                #labFormat = labelFormat(prefix = "$"),
                opacity = 1
      )
    })
}

shinyApp(ui, server)
