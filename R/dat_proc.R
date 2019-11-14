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
  filter(Parameter %in% c('NitrateNitriteNO3')) %>% 
  filter(StationCode %in% 'SDMF05') %>% 
  mutate(
    Parameter = case_when(
      Parameter %in% 'NitrateNitriteNO3' ~ 'Nitrate, Nitrite'
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
  chg = seq(0.1, 1, length = 20),
  eff = seq(0.1, 1,length = 20)
)

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
  
  chg <- scns[i, ][['chg']]
  eff <- scns[i,][['eff']]
  
  simdat <- simvals(powdat, chg = chg, eff = eff, sims = 10000)
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
powdat <- medat %>% 
  filter(Parameter %in% c('NitrateNitriteNO3')) %>% 
  filter(StationCode %in% 'SDMF05') %>% 
  mutate(
    Parameter = case_when(
      Parameter %in% 'NitrateNitriteNO3' ~ 'Nitrate, Nitrite'
    )
  )

# grid scenarios to eval
scns <- crossing(
  thr = seq(0.1, 10, length = 10),
  eff = seq(0.1, 1,length = 10)
)

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
  
  thr <- scns[i, ][['thr']]
  eff <- scns[i,][['eff']]
  
  thrdat <- thrvals(powdat, eff = eff, sims = 1000)
  thrfun(thrdat, thr = thr)
  
}

# combine results with scns
thrs <- scns %>% 
  mutate(
    pow = unlist(res)
  )

save(thrs, file = here::here('data', 'thrs.RData'), compress = 'xz')
