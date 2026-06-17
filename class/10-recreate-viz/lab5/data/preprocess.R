library(tidyverse)

ucmr5 <- read_csv("data/UCMR5_data_allsamples.csv")

dat <- ucmr5 %>%
  dplyr::select(PWSID, PWSName, State, CollectionDate, FacilityName, Size)

write_csv(dat, "data/ucmr5_dat.csv")
