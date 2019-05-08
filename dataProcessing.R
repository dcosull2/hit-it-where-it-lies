install.packages("devtools")
require(devtools)
install_github("BillPetti/baseballr")
require(baseballr)
library(ggplot2)
library(dplyr)


# Scraping all 2019 statcast batter info


fullBatter <- scrape_statcast_savant(start_date = "2019-03-28", 
                                          end_date = Sys.Date(),
                                          player_type = "batter") 


# Only including the player name, distance of a batted ball, its x and y positon, description, and event
hitsBatter <- fullBatter %>% 
  select(player_name,
         hit_distance_sc,
         hc_x,
         hc_y,
         description,
         events) %>%  
  na.omit(hc_x)

# Grouping by batter and getting sum of hit distance

hitsBatter <- hitsBatter %>% 
  group_by(player_name) %>% 
  summarise(total_distance = sum(hit_distance_sc)) %>% 
  arrange(desc(total_distance))

# Develop method for quick team assignment for more than top 10 probably function in conjunction with name

topDistanceBatter <- head(hitsBatter, 10)
topDistanceBatterTeam <- c("HOU", "ARI", "MIN", "STL", "CHC", "HOU", "TEX", "NYY", "STL", "OAK")
topDistanceBatter$team <- topDistanceBatterTeam

playerNames <- pull(topDistanceBatter, player_name)

# Changes to be made, normalize these latitude indicators

homeplate_lat <- c(29.756694,33.445000,44.981717,38.622411,41.947908,29.756694,32.751580,40.829489,38.622411,37.751328)
homeplate_lon <- c(-95.355361,-112.066722,-93.278297,-90.193393,-87.655819,-95.355361,-97.082989,-73.926951,-90.193393,-122.201074)

second_base_lat <- c(29.757005,33.445340,44.981718,38.622566,41.948176,29.757005,32.751332,40.829577,38.622566,37.751522)
second_base_lon <- c(-95.355473,-112.066712,-93.277824,-90.193001,-87.655537,-95.355473,-97.082706,-73.926509,-90.193001,-122.200718)

topDistanceBatter$homeplate_lat <- homeplate_lat  
topDistanceBatter$homeplate_lon <- homeplate_lon  

topDistanceBatter$second_base_lat <- second_base_lat  
topDistanceBatter$second_base_lon <- second_base_lon  

topDistanceBatter <- topDistanceBatter %>% 
  mutate(ref_distance_lat_127 = second_base_lat-homeplate_lat,
         ref_distance_lon_127 = second_base_lon-homeplate_lon) 

# Values should be equal
topDistanceBatter$ref_distance_lat_127^2+topDistanceBatter$ref_distance_lon_127^2


topDistanceBatter <- topDistanceBatter %>% 
  mutate(ref_units = total_distance/127) %>% 
  mutate(current_lat = homeplate_lat+(ref_units*ref_distance_lat_127)) %>% 
  mutate(current_lon = homeplate_lon+(ref_units*ref_distance_lon_127))

write.csv(topDistanceBatter, file = "topDistanceBatter2019.csv")




# TEAM, HOME PLATE, SECOND BASE (127 ft)
# HOU
# 29.756694, -95.355361
# 29.757005, -95.355473
# ARI
# 33.445000, -112.066722
# 33.445340, -112.066712
# MIN
# 44.981717, -93.278297
# 44.981718, -93.277824
# STL
# 38.622411, -90.193393
# 38.622566, -90.193001
# CHC
# 41.947908, -87.655819
# 41.948176, -87.655537
# TEX
# 32.751580, -97.082989
# 32.751332, -97.082706
# NYY
# 40.829489, -73.926951
# 40.829577, -73.926509
# OAK
# 37.751328, -122.201074
# 37.751522, -122.200718








