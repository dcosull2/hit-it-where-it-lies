library(shiny)
library(leaflet)

topDistanceBatter <- as_tibble(read.csv("topDistanceBatter2019.csv"))
playerNames <- pull(topDistanceBatter, player_name)


ui <- fluidPage(
  
  # App title ----
  titlePanel("Play it where it lies!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input ----
      selectInput(inputId = "Player", 
                  label = "Player", 
                  choices = playerNames, 
                  selected = TRUE, 
                  multiple = FALSE,
                  selectize = TRUE, 
                  width = NULL, 
                  size = NULL)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output ----
      leafletOutput("hitMap")
      
    )
  )
)

server <- function(input, output) {
  
  output$hitMap <- renderLeaflet({
    
    currentPlayer <- topDistanceBatter %>% 
      filter(player_name == input$Player)
    
    
    m <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(lng=currentPlayer$homeplate_lon, lat=currentPlayer$homeplate_lat) %>% 
      addMarkers(lng=currentPlayer$current_lon, lat=currentPlayer$current_lat)
    
    m
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
