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


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Model 2: Difference within Percentile Speed Models
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dpos_kabco <- mdata %>% 
    dplyr::select(KABCO, AADT, PSL, Length, Road_Class, P50_P15, P85_P50, P85_P15)
dpos_ka <- mdata %>% 
    dplyr::select(K_A, AADT, PSL, Length, Road_Class, P50_P15, P85_P50, P85_P15)
dpos_bc <- mdata %>% 
    dplyr::select(B_C, AADT, PSL, Length, Road_Class, P50_P15, P85_P50, P85_P15)
dpos_o <- mdata %>% 
    dplyr::select(O, AADT, PSL, Length, Road_Class, P50_P15, P85_P50, P85_P15)

m_dpos_kabco <- glm.nb(KABCO ~ log(AADT) + PSL + Length + Road_Class + P50_P15 + P85_P50,
                       data = dpos_kabco)
m_dpos_ka <- glm.nb(K_A ~ log(AADT) + PSL + Length + Road_Class + P50_P15 + P85_P50,
                    data = dpos_ka)
m_dpos_bc <- glm.nb(B_C ~ log(AADT) + PSL + Length + Road_Class + P50_P15 + P85_P50,
                    data = dpos_bc)
m_dpos_o <- glm.nb(O ~ log(AADT) + PSL + Length + Road_Class + P50_P15 + P85_P50,
                   data = dpos_o)

stargazer(m_dpos_kabco, m_dpos_ka, m_dpos_bc, m_dpos_o, 
          type = "html", 
          out = "output/m2_diff_operating_speed_percentiles.html",
          star.cutoffs = c(.05, .01, .001),
          title = "Model 2: Difference within Operating Speed Percentiles",
          no.space = TRUE, digits = 3)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Model 3: Difference between Posted Speed Limit & Operating Speed Percentiles
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ppos_kabco <- mdata %>% 
    dplyr::select(KABCO, AADT, PSL, Length, Road_Class, PSL_P15, PSL_P50, P85_PSL)
ppos_ka <- mdata %>% 
    dplyr::select(K_A, AADT, PSL, Length, Road_Class, PSL_P15, PSL_P50, P85_PSL)
ppos_bc <- mdata %>% 
    dplyr::select(B_C, AADT, PSL, Length, Road_Class, PSL_P15, PSL_P50, P85_PSL)
ppos_o <- mdata %>% 
    dplyr::select(O, AADT, PSL, Length, Road_Class, PSL_P15, PSL_P50, P85_PSL)

m_ppos_kabco <- glm.nb(KABCO ~ log(AADT) + PSL + Length + Road_Class + PSL_P15 + PSL_P50 + P85_PSL,
                       data = ppos_kabco)
m_ppos_ka <- glm.nb(K_A ~ log(AADT) + PSL + Length + Road_Class + PSL_P15 + PSL_P50 + P85_PSL,
                    data = ppos_ka)
m_ppos_bc <- glm.nb(B_C ~ log(AADT) + PSL + Length + Road_Class + PSL_P15 + PSL_P50 + P85_PSL,
                    data = ppos_bc)
m_ppos_o <- glm.nb(O ~ log(AADT) + PSL + Length + Road_Class + PSL_P15 + PSL_P50 + P85_PSL,
                   data = ppos_o)

stargazer(m_ppos_kabco, m_ppos_ka, m_ppos_bc, m_ppos_o, 
          type = "html", 
          out = "output/m3_w_psl_diff_psl_operating_speed_percentiles.html",
          star.cutoffs = c(.05, .01, .001),
          title = "Model 3: Difference between Posted Speed Limit & Operating Speed Percentiles",
          no.space = TRUE, digits = 3)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Model 4: Difference between Posted Speed Limit & Operating Speed Percentiles
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# without PSL

ppos_kabco <- mdata %>% 
    dplyr::select(KABCO, AADT, PSL, Length, Road_Class, PSL_P15, PSL_P50, P85_PSL)
ppos_ka <- mdata %>% 
    dplyr::select(K_A, AADT, PSL, Length, Road_Class, PSL_P15, PSL_P50, P85_PSL)
ppos_bc <- mdata %>% 
    dplyr::select(B_C, AADT, PSL, Length, Road_Class, PSL_P15, PSL_P50, P85_PSL)
ppos_o <- mdata %>% 
    dplyr::select(O, AADT, PSL, Length, Road_Class, PSL_P15, PSL_P50, P85_PSL)

m_ppos_kabco <- glm.nb(KABCO ~ log(AADT) + Length + Road_Class + PSL_P15 + PSL_P50 + P85_PSL,
                       data = ppos_kabco)
m_ppos_ka <- glm.nb(K_A ~ log(AADT) + Length + Road_Class + PSL_P15 + PSL_P50 + P85_PSL,
                    data = ppos_ka)
m_ppos_bc <- glm.nb(B_C ~ log(AADT) + Length + Road_Class + PSL_P15 + PSL_P50 + P85_PSL,
                    data = ppos_bc)
m_ppos_o <- glm.nb(O ~ log(AADT) + Length + Road_Class + PSL_P15 + PSL_P50 + P85_PSL,
                   data = ppos_o)

stargazer(m_ppos_kabco, m_ppos_ka, m_ppos_bc, m_ppos_o, 
          type = "html", 
          out = "output/m4_wo_psl_diff_psl_operating_speed_percentiles.html",
          star.cutoffs = c(.05, .01, .001),
          title = "Model 4: Difference between Posted Speed Limit (w/o) & Operating Speed Percentiles",
          no.space = TRUE, digits = 3)
