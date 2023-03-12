library(tidyverse)
library(sf)

join_final <- readRDS("data/join_final.rds")

segment_crash <- join_final %>% 
    st_drop_geometry() %>% 
    select(SegmentID, IncidentID, InjurySeverity, Road_Class_SCL, Length_INRIX, Speed_Limit_SNL, AADT) %>% 
    mutate(IncidentID = as.character(IncidentID),
           InjurySeverity = as.factor(InjurySeverity)) %>% 
    group_by(SegmentID, InjurySeverity) %>% 
    summarize(Crashes = n()) %>% 
    ungroup() %>% 
    pivot_wider(names_from = InjurySeverity, values_from = Crashes)

segment_crash[is.na(segment_crash)] <- 0

segment_crash <- segment_crash %>%
    mutate(K_A = K + A,
           B_C = B + C) %>% 
    mutate(KABCO = K_A + B_C + O) %>% 
    select(SegmentID, KABCO, K_A, B_C, O)

# total crash by KABCO scale
sum(segment_crash$KABCO) # 11594
sum(segment_crash$K_A) # 270
sum(segment_crash$B_C) # 2781
sum(segment_crash$O) # 8543

join_coord_PSL_class_AADT <- readRDS("data/seg_files/join_coord_PSL_class_AADT.rds")

bff_speed <- readRDS("data/bff_speed.rds") %>% 
    select(SegmentID, speed_15, speed_50, speed_85)

mdata <- segment_crash %>% 
    left_join(join_coord_PSL_class_AADT %>% 
                  st_drop_geometry() %>% 
                  select(SegmentID, AADT, Speed_Limit_SNL, Length_INRIX, Road_Code_SNL),
              by = "SegmentID") %>% 
    rename(PSL = Speed_Limit_SNL,
           Length = Length_INRIX,
           Road_Class = Road_Code_SNL) %>% 
    inner_join(bff_speed, by = "SegmentID") %>% 
    mutate(P50_P15 = speed_50 - speed_15,
           P85_P50 = speed_85 - speed_50,
           P85_P15 = speed_85 - speed_15,
           PSL_P15 = PSL - speed_15,
           PSL_P50 = PSL - speed_50,
           P85_PSL = speed_85 - PSL) %>% 
    rename(P15 = speed_15,
           P50 = speed_50,
           P85 = speed_85) %>% 
    mutate(AADT = as.numeric(AADT),
           Road_Class = as.factor(Road_Class)) %>% 
    select(-SegmentID)

corr_matrix <- as.data.frame(round(cor(mdata[, -8]), 2))

write_csv(corr_matrix, path = "output/corr_matrix.csv")

# saveRDS(mdata, "data/mdata.rds")
