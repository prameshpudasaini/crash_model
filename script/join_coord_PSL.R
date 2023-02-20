library(tidyverse)
library(sf)

# STREET CODES

# A - Freeway
# B - Major Street
# C - Canal
# D - Minor Street
# E - Freeway Ramp
# F - Frontage
# G - 
# H - Highway
# I - Intersection Stub for Phoenix Fire CAD System
# P - Private Street
# R - Railroad
# X - Unfinished
# Y - 

street_PSL_sf <- st_transform(st_read("data/Street_Name_Labels/Street_Name_Labels.shp"),
                             crs = "+proj=longlat +datum=WGS84") %>% # 89382 objects
    select(OBJECTID, STREETCODE, SPEED, FULLNAME) %>% 
    filter(STREETCODE %in% c("B", "D")) %>% # 84366 objects
    mutate(Road_Code_SNL = case_when(STREETCODE == "B" ~ "Major Street",
                                     TRUE ~ "Minor Street"),
           Speed_Limit_SNL = case_when(SPEED == "0" ~ 5,
                                       SPEED == "1" ~ 10,
                                       SPEED == "2" ~ 15,
                                       SPEED == "3" ~ 20,
                                       SPEED == "4" ~ 25,
                                       SPEED == "5" ~ 30,
                                       SPEED == "6" ~ 35,
                                       SPEED == "7" ~ 40,
                                       SPEED == "8" ~ 45,
                                       SPEED == "9" ~ 50,
                                       SPEED == "10" ~ 55,
                                       SPEED == "11" ~ 60,
                                       SPEED == "12" ~ 65),
           Road_Name_SNL = FULLNAME) %>% 
    select(OBJECTID, Road_Name_SNL, Road_Code_SNL, Speed_Limit_SNL) %>% 
    filter(!Speed_Limit_SNL %in% c(5, 10)) # 83089 objects

# saveRDS(street_PSL_sf, "data/seg_files/street_PSL_sf.rds")

coord_sf <- readRDS("data/seg_files/coord_sf.rds")
coord_sf_marker <- readRDS("data/seg_files/coord_sf_marker.rds")

joinPointLine <- function(x_point, y_line, dist, crs = "+proj=longlat +datum=WGS84") {
    temp_join <- st_join(x_point, y_line, join = st_is_within_distance, dist = dist, 
                         left = FALSE)
    
    temp_ID <- temp_join %>% 
        st_drop_geometry() %>% 
        select(SegmentID) %>% 
        distinct() %>% 
        pull(SegmentID)
    
    st_join(x_point %>% filter(SegmentID %in% temp_ID),
            y_line,
            join = st_nearest_feature, left = FALSE)
}

coord_PSL <- joinPointLine(coord_sf_marker, street_PSL_sf, dist = 10)

join_coord_PSL <- coord_sf %>% 
    inner_join(coord_PSL %>% 
                   st_drop_geometry() %>% 
                   select(-OBJECTID),
               by = "SegmentID")

# saveRDS(join_coord_PSL, "data/seg_files/join_coord_PSL.rds")
