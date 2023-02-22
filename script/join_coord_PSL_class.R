library(tidyverse)
library(sf)

street_class_sf <- st_transform(st_read("data/Street_Centerline/Street_Centerline.shp"),
                                crs = "+proj=longlat +datum=WGS84") %>%
    select(OBJECTID, STREETCLAS, ANNAME, JURISDICTI) %>%
    rename(Road_Class_SCL = STREETCLAS,
           Road_Name_SCL = ANNAME,
           Jurisdiction = JURISDICTI) %>% # 69989 objects
    filter(Jurisdiction == "Phoenix") %>% # 59497 objects
    select(-Jurisdiction)

# saveRDS(street_class_sf, "data/seg_files/street_class_sf.rds")

coord_PSL_sf <- readRDS("data/seg_files/join_coord_PSL.rds")

coord_PSL_sf_marker <- st_as_sf(coord_PSL_sf %>% 
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

coord_PSL_class <- joinPointLine(coord_PSL_sf_marker, street_class_sf, dist = 10)

join_coord_PSL_class <- coord_PSL_sf %>% 
    inner_join(coord_PSL_class %>% 
                   st_drop_geometry() %>% 
                   select(-OBJECTID),
               by = "SegmentID")

# saveRDS(join_coord_PSL_class, "data/seg_files/join_coord_PSL_class.rds")
