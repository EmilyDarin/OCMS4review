library(tidyverse)
library(lubridate)
library(here)

# dry weather monitoring --------------------------------------------------

dw_dat <- read.csv(here::here('data/raw', 'DWM_ALL_DATA.csv'), stringsAsFactors = F)
dw_stat <- read.csv(here::here('data/raw', 'DWM_Stations.csv'), stringsAsFactors = F)
dw_parm <- read.csv(here::here('data/raw', 'DWM_PARAMETER.csv'), stringsAsFactors = F)

dwdat <- dw_dat %>% 
  mutate(
    Date = ymd_hms(Date, tz = 'Pacific/Pitcairn'), 
    Date = as.Date(Date)
  ) %>% 
  rename(StationCode = Station) %>% 
  inner_join(dw_stat, by = c('StationCode', 'Watershed')) %>% 
  select(StationCode, Watershed, Date, Parameter, Result, Units, Longitude, Latitude)

save(dwdat, file = here::here('data', 'dwdat.RData'), compress = 'xz')

# mass emissions ----------------------------------------------------------

me_dat <- read.csv(here::here('data/raw', 'ME_ALL_DATA.csv'), stringsAsFactors = F)
me_stat <- read.csv(here::here('data/raw', 'ME_Stations.csv'), stringsAsFactors = F)
me_parm <- read.csv(here::here('data/raw', 'ME_PARAMETER.csv'), stringsAsFactors = F)

# get select columns, join with station locations
medat <- me_dat %>% 
  mutate(
    Date = ymd_hms(Date, tz = 'Pacific/Pitcairn'), 
    Date = as.Date(Date)
  ) %>% 
  rename(StationCode = Station) %>% 
  inner_join(me_stat, by = c('StationCode', 'Watershed')) %>% 
  select(StationCode, Watershed, Date, Parameter, Result, Units, Longitude, Latitude)

save(medat, file = here::here('data', 'medat.RData'), compress = 'xz')
