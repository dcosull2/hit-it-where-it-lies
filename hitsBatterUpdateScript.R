require(devtools)
install_github("BillPetti/baseballr")
require(baseballr)
library(ggplot2)
library(dplyr)
library(data.table)
library(R.utils)

# Functions

fullBatterUpdate <- function(fullBatter){
  startDate <- as.Date(max(fullBatter$game_date)) + 1
  
  nextScrape <- scrape_statcast_savant(start_date = startDate, 
                                       end_date = Sys.Date(),
                                       player_type = "batter") 
  
  as_tibble(bind_rows(fullBatter, nextScrape))
}

hitsBatterUpdate <- function(fullBatter){
  hitsBatter <- fullBatter %>% 
    select(player_name,
           hit_distance_sc) %>%  
    na.omit(hit_distance_sc)
  
  hitsBatter <- hitsBatter %>% 
    group_by(player_name) %>% 
    summarise(total_distance = sum(hit_distance_sc)) %>% 
    arrange(desc(total_distance))
  
  hitsBatter <- hitsBatter %>% mutate(multiplier = total_distance/127)
  
  playerTeam <- daily_batter_bref(as.Date(Sys.Date())-14, Sys.Date()) %>% select(Name, Team) 
  playerTeam <- as_tibble(playerTeam)
  playerTeam$Team <- unlist(lapply(unname(sapply(playerTeam$Team, FUN = strsplit, split = ",", fixed = T)), tail, n=1))
  
  left_join(x = hitsBatter, y = playerTeam, by = c("player_name" = "Name"))
  
}

fullBatter <- fullBatterUpdate(fullBatter)
hitsBatter <- hitsBatterUpdate(fullBatter)
fwrite(fullBatter, "fullBatter.csv")
fwrite(hitsBatter, "hitsBatter.csv")
