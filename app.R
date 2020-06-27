library(leaflet)
library(sf)
library(shiny)
library(tmap)


# Powiaty w Polsce / Counties in Poland -----------------------------------
# https://gis-support.pl/granice-administracyjne/

countyshp <- st_read("data/powiaty_pl/Powiaty.shp")


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

server <- function(input, output, session) {
    
    points <- eventReactive(input$showcounty, {
        
        
        # find a code for a selected county ---------------------------------------
        jpt_code_je <- counties$kod_je[which(counties$county == input$county)]
        
        # extract a shpe for a selected county ------------------------------------
        pow <- countyshp[countyshp$JPT_KOD_JE == jpt_code_je,]
        
        # transform to geographical coordinates -----------------------------------
        #pow <- spTransform(pow, CRS("+proj=longlat +datum=WGS84 +no_defs"))
        st_transform(pow, 4326)
    }, ignoreNULL = FALSE)
    
    output$view_map <- renderLeaflet({
        
        if(is.null(points())){
            return(NULL)
        } else county_borders <- points()
        
        tmap_mode("view")
        tm <- tm_shape(county_borders, name = "County borders") + 
            tm_borders(lwd = 2) +
            tmap_options(basemaps = c("OpenStreetMap", "Esri.WorldTopoMap", "Esri.WorldGrayCanvas"))
        
        tmap_leaflet(tm)
    })
    
    # End application after closing a window or tab ---------------------------
    session$onSessionEnded(stopApp)
}

# Run the application 
shinyApp(ui = ui, server = server)