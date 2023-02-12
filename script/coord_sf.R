library(tidyverse)
library(RODBC)
library(sf)
library(naniar)
library(leaflet)

source("ignore/keys.R")

correction_inrix <- c("43th" = "43rd")

# DATABASE TABLE: ADOT_INRIX.dbo.InrixSegments_Geometry_backup
sql_seg_geo <- sqlQuery(conn, 
                        "SELECT [SegmentID],[FRC],[RoadName],[Miles],[StartLat],[StartLong],[EndLat],[EndLong],[Bearing],[XDGroup]
                   FROM [ADOT_INRIX].[dbo].[InrixSegments_Geometry_backup] AS d
                   WHERE d.District = 'Phoenix'") %>% 
    replace_with_na(replace = list(RoadName = "")) %>% 
    mutate(RoadName = str_replace_all(RoadName, pattern = correction_inrix)) %>%
    mutate(Miles = as.numeric(Miles)) %>% 
    mutate_at(vars(SegmentID, StartLat, EndLat, StartLong, EndLong), as.character)

coord_sep <- sql_seg_geo %>% 
    filter(!Bearing %in% c("E","W","N","S")) %>% 
    separate(XDGroup, c("p1","p2","p3","p4","p5"), ",", remove = FALSE, fill = "right")

coord_seg_geo <- bind_rows(sql_seg_geo %>% filter(Bearing %in% c("E","W","N","S")),
                           
                           coord_sep %>% filter(!is.na(p5)) %>%
                               mutate(StartLat = Bearing, StartLong = p1, EndLat = p2, 
                                      EndLong = p3, Bearing = p4, XDGroup = p5) %>% 
                               select(-p1, -p2, -p3, -p4, -p5),
                           
                           coord_sep %>% filter(is.na(p5)) %>% filter(!is.na(p4)) %>% 
                               mutate(StartLat = EndLong, StartLong = Bearing, EndLat = p1, 
                                      EndLong = p2, Bearing = p3, XDGroup = p4) %>% 
                               select(-p1, -p2, -p3, -p4, -p5),
                           
                           coord_sep %>% filter(is.na(p5)) %>% filter(is.na(p4)) %>%
                               filter(!is.na(p3)) %>% 
                               mutate(StartLat = EndLat, StartLong = EndLong, EndLat = Bearing, 
                                      EndLong = p1, Bearing = p2, XDGroup = p3) %>% 
                               select(-p1, -p2, -p3, -p4, -p5),
                           
                           coord_sep %>% filter(is.na(p5)) %>% filter(is.na(p4)) %>%
                               filter(is.na(p3)) %>% filter(!is.na(p2)) %>% 
                               mutate(StartLat = StartLong, StartLong = EndLat, EndLat = EndLong, 
                                      EndLong = Bearing, Bearing = p1, XDGroup = p2) %>% 
                               select(-p1, -p2, -p3, -p4, -p5)) %>% 
    mutate_at(vars(StartLat, EndLat, StartLong, EndLong), as.numeric) %>% 
    mutate(Bearing = as.factor(Bearing)) %>% 
    rename(Road_Name_INRIX = RoadName,
           Bearing_INRIX = Bearing) %>% 
    select(-FRC, -Miles, -XDGroup)


# DATABASE TABLE: ADOT_INRIX.dbo.InrixSegments_backup
sql_seg <- sqlQuery(conn, "SELECT * FROM ADOT_INRIX.dbo.InrixSegments_backup as d WHERE d.Type = 'XDS'") %>% 
    rename(SegmentID = ID) %>% 
    mutate(SegmentID = as.character(SegmentID),
           Road_Class_INRIX = as.factor(FRCLevel)) %>% 
    rename(Length_INRIX = Length) %>% 
    select(SegmentID, Length_INRIX, Road_Class_INRIX, Latitude, Longitude)


# Join two tables
coord <- coord_seg_geo %>% 
    left_join(sql_seg, by = "SegmentID")


# Conversion to sf object
ls <- apply(coord, 1, function(x) {
    v <- as.numeric(x[c("StartLong", "Longitude", "EndLong", "StartLat", "Latitude", "EndLat")])
    m <- matrix(v, nrow = 3)
    return(st_sfc(st_linestring(m), crs = 4326))
})

coord_geom <- Reduce(c, ls)

coord_sf <- st_set_geometry(
    coord %>% 
        select(SegmentID, Road_Class_INRIX, Road_Name_INRIX, Length_INRIX, Bearing_INRIX, Longitude, Latitude), 
    coord_geom
)

# saveRDS(coord_sf, file = "data/seg_files/coord_sf.rds")

coord_sf_marker <- st_as_sf(coord_sf %>% 
                                st_drop_geometry() %>% 
                                filter(Road_Class_INRIX %in% c("3", "4", "5"),
                                       Length_INRIX > 0.01) %>% 
                                select(SegmentID, Longitude, Latitude),
                            coords = c("Longitude", "Latitude"),
                            crs = "+proj=longlat +datum=WGS84")

# saveRDS(coord_sf_marker, file = "data/seg_files/coord_sf_marker.rds")