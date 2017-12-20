
# Shiny app to visualize SF crime by category

# BUG: leaflet map does not update when switching tabs from density map

# Load required packages
library(tidyverse)
library(lubridate)
library(leaflet)
library(shiny)
library(ggmap)

# Load in full dataset
load("data/data.rda")
map <- get_map(location="sanfrancisco",zoom=12,source="google")

# Only consider crimes in top 13 categories
top13 <- c("LARCENY/THEFT", "OTHER OFFENSES", "NON-CRIMINAL" ,"ASSAULT", "DRUG/NARCOTIC",
            "VEHICLE THEFT", "VANDALISM", "BURGLARY", "WARRANTS", "SUSPICIOUS OCC", "
            MISSING PERSON", "ROBBERY", "FRAUD")
df_top13 <- df %>%
  filter(Category %in% top13)

###########################
### User Interface Code ###
###########################
ui <- fluidPage(
  
  titlePanel("San Francisco Crime over Time"),

  sidebarLayout(
    # Code for sidebar
    # User inputs all in sidebar
    sidebarPanel(
      id = "controls", width = 4,
      checkboxGroupInput("category", "Select Categories:", choices = top13, 
                         selected = top13[1]),
      sliderInput("month", "Select Month:", value = 1, min = 1, max = 12,
                  step = 1, animate = animationOptions(interval = 2000, loop = FALSE)),
      sliderInput("year", "Select Year:", value = 2003, min = 2003, max = 2015,
                  step = 1, animate = animationOptions(interval = 2000, loop = FALSE))
    ),
    # Code for main panel
    # Map visualizations all in main panel
    mainPanel(
      tabsetPanel(
        tabPanel("Leaflet Plot",
          leafletOutput("leafletmap")
        ),
        tabPanel("Density Plot",
          plotOutput("densitymap")
        )
      )
    )
  )
)

########################
### Server-Side Code ###
########################
server <- function(input, output) {
  
  # create data frame for plots, based on input
  df_map <- reactive({
    df %>%
      filter(
        Category %in% input$category,
        Year %in% input$year,
        Month %in% input$month) %>%
      select(Category, PdDistrict, Street1, Dates, lng = X, lat = Y)
  })
  
  # for leaflet plot:
  # create popup for point
  df_popups <- reactive({
    paste("Category", df_map()$Category, "<br>",
          "Police District Region:", df_map()$PdDistrict, "<br>",
          "Street:", df_map()$Street1, "<br>",
          "Dates:", df_map()$Dates)
  })
  
  # render ggplot density plot
  output$densitymap <- renderPlot({
    ggmap(map) +
      geom_density_2d(
        data=df_map(),
        aes(x=lng, y=lat, color=Category)
      ) +
      labs(
        x="",
        y="",
        color="Crime Category"
      )
  })
  
  # render leaflet point plot
  output$leafletmap <- renderLeaflet(
    df_map() %>%
      leaflet() %>%
      setView(lng = -122.43, lat = 37.76, zoom = 12) %>%
      addProviderTiles(providers$CartoDB.Positron ,
                       options = providerTileOptions(opacity = 1))
  )
  
  # update leaflet plot
  observe({
    pal <- colorFactor(
      palette = 'Set1',
      domain = df_map()$Category
    )
    
    proxy <- leafletProxy("leafletmap", data = df_map())
    proxy %>% 
      clearControls() %>%
      clearShapes() %>%
      addCircleMarkers(lng = ~lng, lat = ~lat, weight = 3, 
                       popup = df_popups(), color = ~pal(Category), radius = 3) %>%
      addLegend(values = df_map()$Category, pal = pal)
  })
}

shinyApp(ui = ui, server = server)
