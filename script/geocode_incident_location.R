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
