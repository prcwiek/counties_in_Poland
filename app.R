library(leaflet)
library(rgdal)
library(shiny)
library(tidyverse)

# Powiaty w Polsce / Counties in Poland -----------------------------------
# https://gis-support.pl/granice-administracyjne/

countyshp <- readOGR("data/powiaty_pl/Powiaty.shp")


# Kody TERYT powiatów / TERTYT codes of counties --------------------------
# http://eteryt.stat.gov.pl/eTeryt/rejestr_teryt/udostepnianie_danych/baza_teryt/uzytkownicy_indywidualni/pobieranie/pobieranie.aspx?contrast=default

counties <- read.table(file = "data/TERC_filtered_20200530.csv",
                       header = TRUE,
                       colClasses = c("character", "character"),
                       sep = ",")

ui <- fluidPage(
    
    titlePanel("County map viewer"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "county",
                label = "Counties in Poland",
                choices = counties$county, 
                size = 15, 
                selectize = FALSE,
                selected = "Warszawa"
            ),
            actionButton("showcounty", "Show county borders")
        ),
        
        mainPanel(
            tabsetPanel(
                id = "mainpanels",
                tabPanel("County map", leafletOutput("view_map", width = "100%", height = "800px"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    point <- eventReactive(input$showcounty, {
        

        # find a code for a selected county ---------------------------------------
        jpt_code_je <- counties$kod_je[which(counties$county == input$county)]
        
        # extract a shpe for a selected county ------------------------------------
        pow <- countyshp[countyshp$JPT_KOD_JE == jpt_code_je,]
        
        # transform to geographical coordinates -----------------------------------
        #pow <- spTransform(pow, CRS("+proj=longlat +datum=WGS84 +no_defs"))
        pow <- spTransform(pow, CRS("+init=epsg:4326"))
        
        list(countyshp = pow, lng = coordinates(pow)[1], lat = coordinates(pow)[2])   

    }, ignoreNULL = FALSE)
    
    output$view_map <- renderLeaflet({
        
        if(is.null(point())){
            return(NULL)
        } else point_location <- point()
        
        leaflet() %>% 
            setView(lng = point_location$lng, lat = point_location$lat, zoom = 10) %>% 
            addPolygons(data = point_location[[1]], color = "black", weight = 3, opacity = 0.5, fill = FALSE) %>% 
            addMarkers(data = point_location[[1]], lng = point_location$lng, lat = point_location$lat) %>% 
            addProviderTiles(providers$OpenStreetMap)
    
    })
    
    # End application after closing a window or tab ---------------------------
    session$onSessionEnded(stopApp)
}

# Run the application 
shinyApp(ui = ui, server = server)
