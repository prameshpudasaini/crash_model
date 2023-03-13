library(tidyverse)
library(MASS)
library(jtools)
library(stargazer)

mdata <- readRDS("data/mdata.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Model 1: Percentile Speed Models
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pos_kabco <- mdata %>% 
    dplyr::select(KABCO, AADT, PSL, Length, Road_Class, P15, P50, P85)
pos_ka <- mdata %>% 
    dplyr::select(K_A, AADT, PSL, Length, Road_Class, P15, P50, P85)
pos_bc <- mdata %>% 
    dplyr::select(B_C, AADT, PSL, Length, Road_Class, P15, P50, P85)
pos_o <- mdata %>% 
    dplyr::select(O, AADT, PSL, Length, Road_Class, P15, P50, P85)

m_pos_kabco <- glm.nb(KABCO ~ log(AADT) + PSL + Length + Road_Class + P15 + P50 + P85,
                      data = pos_kabco)
m_pos_ka <- glm.nb(K_A ~ log(AADT) + PSL + Length + Road_Class + P15 + P50 + P85,
                   data = pos_ka)
m_pos_bc <- glm.nb(B_C ~ log(AADT) + PSL + Length + Road_Class + P15 + P50 + P85,
                   data = pos_bc)
m_pos_o <- glm.nb(O ~ log(AADT) + PSL + Length + Road_Class + P15 + P50 + P85,
                  data = pos_o)

summ(m_pos_kabco, exp = TRUE)

stargazer(m_pos_kabco, m_pos_ka, m_pos_bc, m_pos_o, 
          type = "html",
          out = "output/m1_operating_speed_percentiles.html",
          star.cutoffs = c(.05, .01, .001),
          title = "Model 1: Operating Speed Percentiles",
          no.space = TRUE, digits = 3)
