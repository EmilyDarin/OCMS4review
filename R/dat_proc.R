library(tidyverse)
library(lubridate)
library(here)
library(mgcv)
library(doParallel)
library(foreach)
library(EnvStats)

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
  mutate(
    Watershed = case_when(
      Watershed == '' ~ 'San Gabriel River - Coyote Creek',
      T ~ Watershed
    )
  ) %>%
  select(StationCode, Watershed, Date, Parameter, Result, Units, Longitude, Latitude)

save(dwdat, file = here::here('data', 'dwdat.RData'), compress = 'xz')

# mass emissions ----------------------------------------------------------

me_dat <- read.csv(here::here('data/raw', 'ME+NUT_ALL_DATA.csv'), stringsAsFactors = F)
me_stat <- read.csv(here::here('data/raw', 'ME_Stations.csv'), stringsAsFactors = F)
me_parm <- read.csv(here::here('data/raw', 'ME_PARAMETER.csv'), stringsAsFactors = F)

# get select columns, join with station locations
medat <- me_dat %>% 
  mutate(
    Date = mdy_hm(Date, tz = 'Pacific/Pitcairn'), 
    Date = as.Date(Date)
  ) %>% 
  rename(StationCode = Station) %>% 
  inner_join(me_stat, by = c('StationCode', 'Watershed')) %>% 
  select(StationCode, Watershed, Date, Parameter, Result, Units, Qualifier, Longitude, Latitude)

save(medat, file = here::here('data', 'medat.RData'), compress = 'xz')


# power analysis for trends -----------------------------------------------

data(medat)

powdat <- medat %>% 
  filter(Parameter %in% c('AmmoniaN', 'NitrateNitriteNO3', 'TKN', 'OrthoPhosphateP', 'TotalPhosphorusPO4')) %>% 
  mutate(
    Parameter = case_when(
      Parameter %in% 'AmmoniaN' ~ 'Ammonia', 
      Parameter %in% 'NitrateNitriteNO3' ~ 'Nitrate, Nitrite', 
      Parameter %in% 'TKN' ~ 'Total Kjeldahl Nitrogen',
      Parameter %in% 'OrthoPhosphateP' ~ 'Orthophosphate', 
      Parameter %in% 'TotalPhosphorusPO4' ~ 'Total Phosphorus'
    )
  ) 

# simdat <- simvals(powdat, chg = 0.5, eff = 1, sims = 1000)
# 
# powfun(simdat)
# 
# ggplot() + 
#   geom_line(data = simdat, aes(x = Date, y = simrand, group = sims)) +
#   geom_line(data = powdat, aes(x= Date, y = log(Result), col = 'red'))

scns <- crossing(
  sta = unique(powdat$StationCode),
  par = unique(powdat$Parameter), 
  chg = seq(0.1, 1, length = 10),
  eff = seq(0.1, 1,length = 10)
  ) %>% 
  filter(!sta %in% 'SICG03') 

# setup parallel backend
ncores <- detectCores() - 1 
cl <- makeCluster(ncores)
registerDoParallel(cl)
strt <- Sys.time()

# process all stations ~ 15 min
res <- foreach(i = 1:nrow(scns), .packages = c('lubridate', 'tidyverse', 'mgcv')) %dopar% {
  
  sink('log.txt')
  cat(i, 'of', nrow(scns), '\n')
  print(Sys.time()-strt)
  sink()
  
  source("R/funcs.R")
  
  sta <- scns[i, ][['sta']]
  par <- scns[i, ][['par']]
  chg <- scns[i, ][['chg']]
  eff <- scns[i, ][['eff']]
  
  topow <- powdat %>% 
    filter(StationCode %in% sta) %>% 
    filter(Parameter %in% par)
  
  simdat <- simvals(topow, chg = chg, eff = eff, sims = 1000)
  powfun(simdat)
  
}

# combine results with scns
pows <- scns %>% 
  mutate(
    pow = unlist(res)
  )

save(pows,file = here::here('data', 'pows.RData'), compress = 'xz')

# power analysis for threshold values ------------------------------------

data(medat)

# data to eval
scns <- medat %>% 
  filter(Parameter %in% c('AmmoniaN', 'NitrateNitriteNO3', 'TKN', 'OrthoPhosphateP', 'TotalPhosphorusPO4')) %>% 
  filter(!StationCode %in% 'SICG03') %>% 
  mutate(
    Parameter = case_when(
      Parameter %in% 'AmmoniaN' ~ 'Ammonia', 
      Parameter %in% 'NitrateNitriteNO3' ~ 'Nitrate, Nitrite', 
      Parameter %in% 'TKN' ~ 'Total Kjeldahl Nitrogen',
      Parameter %in% 'OrthoPhosphateP' ~ 'Orthophosphate', 
      Parameter %in% 'TotalPhosphorusPO4' ~ 'Total Phosphorus'
    ),
    Year = year(Date), 
    Season = yday(Date),
    dectime = decimal_date(Date)
  ) %>% 
  group_by(StationCode, Parameter) %>% 
  nest

# setup parallel backend
ncores <- detectCores() - 1 
cl <- makeCluster(ncores)
registerDoParallel(cl)
strt <- Sys.time()

# process all stations ~ 4 min
res <- foreach(i = 1:nrow(scns), .packages = c('lubridate', 'tidyverse', 'mgcv', 'EnvStats')) %dopar% {
  
  sink('log.txt')
  cat(i, 'of', nrow(scns), '\n')
  print(Sys.time()-strt)
  sink()
  
  source("R/funcs.R")
  
  dat <- scns[i, 'data'] %>% 
    .[[1]] %>% 
    .[[1]]
  
  # median concentration as baseline
  medv <- thrvals(dat, 1, sims = 100) %>% 
    pull(simrand) %>% 
    median() %>% 
    exp()
  
  sims <- crossing(
      thr = seq(log(medv), log(20*medv), length = 20),
      eff = seq(0.05, 1, length = 20)
    ) %>% 
    group_by(eff, thr) %>% 
    mutate(
      simdat = purrr::map(eff, ~thrvals(dat, eff, sims = 1000))
    )
  
  # get power 
  thrpow <- sims %>% 
    group_by(eff, thr) %>% 
    mutate(
      pow = purrr::pmap(list(simdat, thr), ~thrfun(simdat, thr))
    ) %>% 
    select(-simdat) %>%
    unnest(pow) %>% 
    ungroup()
  
  return(thrpow)
  
}

# combine results with scns
thrs <- scns %>% 
  bind_cols(enframe(res)) %>% 
  select(-data, -name) %>%   
  unnest(value)
    
save(thrs, file = here::here('data', 'thrs.RData'), compress = 'xz')