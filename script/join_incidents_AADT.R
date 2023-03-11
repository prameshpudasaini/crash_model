library(tidyverse)
library(sf)

# join incidents and AADT

incident_data_sf <- readRDS("data/incident_data_sf.rds")
AADT_sf <- readRDS("data/seg_files/AADT_sf.rds")

joinPointLine <- function(x_point, y_line, dist, crs = "+proj=longlat +datum=WGS84") {
    temp_join <- st_join(x_point, y_line, join = st_is_within_distance, dist = dist, 
                         left = FALSE)
    
    temp_ID <- temp_join %>% 
        st_drop_geometry() %>% 
        select(IncidentID) %>% 
        distinct() %>% 
        pull(IncidentID)
    
    st_join(x_point %>% filter(IncidentID %in% temp_ID),
            y_line,
            join = st_nearest_feature, left = FALSE)
}

crash_AADT <- joinPointLine(incident_data_sf, AADT_sf, dist = 10)

# saveRDS(crash_AADT, "data/crash_AADT.rds")

# final join

join_coord_PSL_class_AADT <- readRDS("data/seg_files/join_coord_PSL_class_AADT.rds")

join_final <- st_join(crash_AADT %>% 
                          mutate(Road_Name_Crash = RouteName) %>% 
                          select(IncidentID, InjurySeverity, Road_Name_Crash),
                      join_coord_PSL_class_AADT,
                      join = st_nearest_feature)

# saveRDS(join_final, "data/join_final.rds")
