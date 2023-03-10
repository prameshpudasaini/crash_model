library(tidyverse)
library(ggmap)
library(sf)
library(leaflet)
library(readr)

# incident data -> information on incident
# person data -> information on persons involved
# unit data -> information on vehicles involved

# CityId 214, CountyId 13 & StateId 3
raw_incident_data <- read_csv(file = "ignore/AZ_2021_Crash Data/Incident_Data.csv", show_col_types = FALSE)

incident_data <- raw_incident_data %>% 
    select(IncidentID, CollisionManner, TotalUnits, TotalInjuries, TotalFatalities, 
           InjurySeverity, RouteName, CrossingFeatureName, Latitude, Longitude) %>% 
    mutate(InjurySeverity = case_when(InjurySeverity == 5 ~ "K",
                                      InjurySeverity == 4 ~ "A",
                                      InjurySeverity == 3 ~ "B",
                                      InjurySeverity == 2 ~ "C",
                                      InjurySeverity == 1 ~ "O"))

incident_data_NA <- incident_data %>% 
    filter(is.na(Longitude) | is.na(Latitude)) %>% 
    select(IncidentID, RouteName, CrossingFeatureName, Longitude, Latitude) %>% 
    mutate(RouteName = str_replace_all(RouteName, "[^[:alnum :][:space:]]", ""),
           CrossingFeatureName = str_replace_all(CrossingFeatureName, "[^[:alnum :][:space:]]", "")) %>% 
    mutate(RouteName = trimws(str_replace_all(RouteName, "[^[:alnum:]]*\\d{4,}", "")),
           CrossingFeatureName = str_replace_all(CrossingFeatureName, "[^[:alnum:]]*\\d{4,}", "")) %>% 
    mutate(Address = paste0(RouteName, ", ", CrossingFeatureName))

api_key <- read_file("ignore/google_api_key.txt")
register_google(key = api_key)

result <- geocode(incident_data_NA$Address, output = "latlona", source = "google")

# saveRDS(result, "data/crash_geocode_result.rds")

result <- readRDS("data/crash_geocode_result.rds")

incident_data_geocoded <- bind_cols(incident_data_NA, result)

incident_data_final <- incident_data %>% 
    filter(is.na(Longitude) | is.na(Latitude)) %>% 
    left_join(incident_data_geocoded %>% 
                  select(IncidentID, lon, lat), by = "IncidentID") %>% 
    mutate(Longitude = lon,
           Latitude = lat) %>% 
    select(-lon, -lat) %>% 
    bind_rows(incident_data %>% 
                  filter(!is.na(Longitude))) %>% 
    filter(!is.na(Longitude)) %>% 
    select(IncidentID, InjurySeverity, RouteName, Latitude, Longitude)

incident_data_sf <- st_as_sf(incident_data_final,
                             coords = c("Longitude", "Latitude"),
                             crs = "+proj=longlat +datum=WGS84")

# saveRDS(incident_data_sf, "data/incident_data_sf.rds")

# leaflet map incidents
pal <- colorFactor(palette = "RdYlGn", domain = incident_data_final$InjurySeverity)

incident_data_final %>% 
    leaflet() %>% 
    addTiles() %>% 
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>% 
    addCircleMarkers(lng = ~Longitude, lat = ~Latitude, color = pal(incident_data_final$InjurySeverity),
                     radius = ~InjurySeverity) %>% 
    addLegend("topright", pal = pal, values = incident_data_final$InjurySeverity, 
              opacity = 3, title = "Injury Severity")
