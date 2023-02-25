library(tidyverse)
library(sf)

AADT_sf <- st_transform(st_read("ignore/AZ_HPMS/Arizona_Highway_Performance_Monitoring_System_(HPMS)_2020_Data_.shp"),
                        crs = "+proj=longlat +datum=WGS84") %>% 
    select(OBJECTID, RouteId, AADT) %>% 
    filter(!str_detect(RouteId, "^[I ]")) %>%
    filter(!str_detect(RouteId, "^[S ]")) %>%
    filter(!str_detect(RouteId, "^[U ]")) %>% 
    filter(AADT > 0) %>% 
    rename(Road_Name_AADT = RouteId)

# saveRDS(AADT_sf, "data/seg_files/AADT_sf.rds")

coord_PSL_class_sf <- readRDS("data/seg_files/join_coord_PSL_class.rds")

coord_PSL_class_sf_marker <- st_as_sf(coord_PSL_class_sf %>% 
                                    st_drop_geometry() %>% 
                                    select(SegmentID, Longitude, Latitude),
                                coords = c("Longitude", "Latitude"),
                                crs = "+proj=longlat +datum=WGS84")


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

coord_PSL_class_AADT <- joinPointLine(coord_PSL_class_sf_marker, AADT_sf, dist = 10)

join_coord_PSL_class_AADT <- coord_PSL_class_sf %>% 
    inner_join(coord_PSL_class_AADT %>% 
                   st_drop_geometry() %>% 
                   select(-OBJECTID),
               by = "SegmentID")

# saveRDS(join_coord_PSL_class_AADT, "data/seg_files/join_coord_PSL_class_AADT.rds")
