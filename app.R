library(shiny)
library(leaflet)
library(dplyr)
library(data.table)
library(curl)
library(XML)
library(RCurl)

hitsBatter <- as_tibble(fread("https://raw.githubusercontent.com/dcosull2/hit-it-where-it-lies/master/hitsBatter.csv"))
topDistanceBatter <- head(hitsBatter, 10)

# Double Team Cities----
dataCubs <- getURL("https://www.baseball-reference.com/teams/CHC/2019.shtml#team_batting::none")
dataGrabCubs <- readHTMLTable(dataCubs, stringsAsFactors = FALSE)
currentCubs <- dataGrabCubs$team_batting$Name

dataDodgers <- getURL("https://www.baseball-reference.com/teams/LAD/2019.shtml#team_batting::none")
dataGrabDodgers <- readHTMLTable(dataDodgers, stringsAsFactors = FALSE)
currentDodgers <- dataGrabDodgers$team_batting$Name


dataYankees <- getURL("https://www.baseball-reference.com/teams/NYY/2019.shtml#team_batting::none")
dataGrabYankees <- readHTMLTable(dataYankees, stringsAsFactors = FALSE)
currentYankees <- dataGrabYankees$team_batting$Name

rosterNameFix <- function(names){
  names <- gsub("\\*", "", names)
  names <- gsub("\\(10-day IL\\)", "", names)
  names <- gsub("\\(40-man\\)", "", names)
  names <- gsub("\\(60-day IL\\)", "", names)
  names <- trimws(names, which = "both")
  names <- gsub("\\#", "", names)
  names
  
}

currentDodgers <- rosterNameFix(currentDodgers)
currentCubs <- rosterNameFix(currentCubs)
currentYankees <- rosterNameFix(currentYankees)

cubsPattern <- paste(currentCubs, collapse = "|")
dodgersPattern <- paste(currentDodgers, collapse = "|")
yankeesPattern <- paste(currentYankees, collapse = "|")
#----
cityTeam <- function(x){
  
  
  
  
  
  if(x=="Arizona"){return("Arizona Diamondbacks")}
  if(x=="Atlanta"){return("Atlanta Braves")}
  if(x=="Baltimore"){return("Baltimore Orioles")}
  
  if(x=="Chicago"){
    if(grepl(cubsPattern, x)==T){return("Chicago Cubs")}
    else{return("Chicago White Sox")}}
  
  if(x=="Boston"){return("Boston Red Sox")}
  if(x=="Cincinnati"){return("Cincinatti Reds")}
  if(x=="Cleveland"){return("Cleveland Indians")}
  if(x=="Colorado"){return("Colorado Rockies")}
  if(x=="Detroit"){return("Detroit Tigers")}
  if(x=="Houston"){return("Houston Astros")}
  if(x=="Kansas City"){return("Kansas City Royals")}
  
  if(x=="Los Angeles"){
    if(grepl(dodgersPattern, x)==T){return("Los Angeles Dodgers")}
    else{return("Los Angeles Angels")}}
  
  if(x=="Miami"){return("Miami Marlins")}
  if(x=="Milwaukee"){return("Milwuakee Brewers")}
  if(x=="Minnesota"){return("Minnesota Twins")}
  
  if(x=="New York"){
    if(grepl(yankeessPattern, x$Name)==T){return("New York Yankees")}
    else{return("New York Mets")}}
  
  if(x=="Oakland"){return("Oakland Athletics")}
  if(x=="Philadelphia"){return("Philadelphia Phillies")}
  if(x=="Pittsburgh"){return("Pittsburgh Pirates")}
  if(x=="San Diego"){return("San Diego Padres")}
  if(x=="San Francisco"){return("San Francisco Giants")}
  if(x=="Seattle"){return("Seattle Mariners")}
  if(x=="St. Louis"){return("St. Louis Cardinals")}
  if(x=="Tampa Bay"){return("Tampa Bay Rays")}
  if(x=="Texas"){return("Texas Rangers")}
  if(x=="Toronto"){return("Toronto Blue Jays")}
  else{return("NA")}
  
}


topDistanceBatter$Team <- unname(sapply(topDistanceBatter$Team, FUN = cityTeam))


topDistanceBatter$player_name <- factor(topDistanceBatter$player_name)
stadiumCord <- as_tibble(fread("https://raw.githubusercontent.com/dcosull2/hit-it-where-it-lies/master/final_stadium_cord.csv"))
playerNames <- pull(topDistanceBatter, player_name)

topDistanceBatter$Team
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
                  selectize = FALSE, 
                  width = NULL, 
                  size = NULL)
      #,
      
      # selectInput(inputId = "Stadium", 
      #             label = "Stadium", 
      #             choices = stadiumCord$team_name, 
      #             selected = TRUE, 
      #             multiple = FALSE,
      #             selectize = TRUE, 
      #             width = NULL, 
      #             size = NULL)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output ----
      leafletOutput("hitMap"),
      textOutput("text1"), 
      textOutput("text2"),
      tags$head(tags$style("#text1{color: black;
                                 font-size: 22px;
                                 }")),
      tags$head(tags$style("#text2{color: black;
                                 font-size: 22px;
                                 }"))
                
      )
      
      
      
    )
  )


server <- function(input, output) {
  
  output$hitMap <- renderLeaflet({
    
    currentPlayer <- topDistanceBatter %>% 
      filter(player_name == input$Player)
    
    selectedStadiumCord <- stadiumCord %>%
      filter(team_name == currentPlayer$Team)
    
    currentLat <- selectedStadiumCord$home_lat + (selectedStadiumCord$lat_127*currentPlayer$multiplier)
    currentLon <- selectedStadiumCord$home_lon + (selectedStadiumCord$lon_127*currentPlayer$multiplier)

    baseballIcon <- makeIcon(
      iconUrl = "https://raw.githubusercontent.com/dcosull2/hit-it-where-it-lies/master/baseball_pin.png",
      iconWidth = 33, iconHeight = 33,
      iconAnchorX = 16, iconAnchorY = 29
    )
    
    m <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addPolylines(lng = c(currentLon, selectedStadiumCord$home_lon), lat = c(currentLat, selectedStadiumCord$home_lat)) %>% 
      #addMarkers(lng=selectedStadiumCord$home_lon, lat=selectedStadiumCord$home_lat) %>%  
      
      
      addMarkers(lng = currentLon, lat = currentLat, icon = baseballIcon) 
    #%>% 
      #addCircleMarkers(lng=currentLon, lat=currentLat, 
       #                color = "red" ,
       #                fillColor = "white", 
       #                fillOpacity = 1.0)
      
    
    m
    
  })
  
  output$text1 <- renderText({ 
    currentPlayer<- topDistanceBatter %>% 
      filter(player_name == input$Player)
    
    paste(input$Player, "has hit the ball", sep = " ")
  })
  
  output$text2 <- renderText({
    currentPlayer<- topDistanceBatter %>% 
      filter(player_name == input$Player)
    paste(toString(round(currentPlayer$total_distance/5280, 4)), "miles this season", sep = " ")
    
  })


}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
