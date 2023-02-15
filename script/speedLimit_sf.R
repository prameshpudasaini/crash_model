library(tidyverse)
library(sf)

roadName_replace <- c(
    "\\bAve\\b" = "Avenue",
    "\\bBlvd\\b" = "Boulevard",
    "\\bCir\\b" = "Circle",
    "\\bDr\\b" = "Drive",
    "\\bE\\b" = "East",
    "\\bFwy\\b" = "Freeway",
    "\\bHwy\\b" = "Highway",
    "\\bLn\\b" = "Lane",
    "\\bMcdowell\\b" = "McDowell",
    "\\bN\\b" = "North",
    "\\bPkwy\\b" = "Parkway",
    "\\bRd\\b" = "Road",
    "\\bS\\b" = "South",
    "\\bSt\\b" = "Street",
    "\\bTrl\\b" = "Trail",
    "\\bW\\b" = "West"
)

roadName_correct <- c("Sweerwater" = "Sweetwater", "Orborn" = "Osborn")

speedLimit_sf <- st_transform(st_read("data/SpeedLimits/SpeedLimits.shp"), 
                              crs = "+proj=longlat +datum=WGS84") %>% 
    rename(Location = LOCATION) %>% 
    mutate(LimitID = row_number()) %>% 
    select(LimitID, Location, SpeedLimit) %>% 
    mutate(Location = str_to_title(str_squish(str_trim(Location, side = "both")))) %>% 
    mutate(Location = str_replace_all(Location, pattern = roadName_replace)) %>% 
    mutate(Location = str_replace_all(Location, pattern = roadName_correct))

saveRDS(speedLimit_sf, "data/seg_files/speedLimit_sf.rds")
