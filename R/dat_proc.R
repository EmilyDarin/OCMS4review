library(tidyverse)
library(lubridate)
library(here)
library(mgcv)
library(doParallel)
library(foreach)
library(EnvStats)
library(readxl)

# dry weather monitoring --------------------------------------------------

dw_dat <- read.csv(here::here('data/raw', 'DWM_ALL_DATA.csv'), stringsAsFactors = F)
dw_stat <- read.csv(here::here('data/raw', 'DWM_Stations.csv'), stringsAsFactors = F)
dw_parm <- read.csv(here::here('data/raw', 'DWM_PARAMETER.csv'), stringsAsFactors = F)

dwdat <- dw_dat %>% 
  mutate(
    Date = ymd_hms(Date, tz = 'Pacific/Pitcairn'), 
    Date = as.Date(Date),
    Parameter = case_when(
      Parameter %in% c('AmmoniaN', 'AmmoniaAsN') ~ 'Ammonia', 
      Parameter %in% 'NitrateNitriteNO3' ~ 'Nitrate, Nitrite', 
      Parameter %in% 'TKN' ~ 'Total Kjeldahl Nitrogen',
      Parameter %in% 'NitrateAsN' ~ 'Total Nitrogen',
      Parameter %in% 'OrthoPhosphateP' ~ 'Orthophosphate', 
      Parameter %in% 'TotalPhosphorusPO4' ~ 'Total Phosphorus', 
      T ~ Parameter
    )
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
    Date = as.Date(Date), 
    Parameter = case_when(
      Parameter %in% 'AmmoniaN' ~ 'Ammonia', 
      Parameter %in% 'NitrateNitriteNO3' ~ 'Nitrate, Nitrite', 
      Parameter %in% 'TKN' ~ 'Total Kjeldahl Nitrogen',
      Parameter %in% 'OrthoPhosphateP' ~ 'Orthophosphate', 
      Parameter %in% 'TotalPhosphorusPO4' ~ 'Total Phosphorus', 
      T ~ Parameter
    ), 
    Type = gsub('T$|F$', '', Type)
  ) %>% 
  rename(StationCode = Station) %>% 
  inner_join(me_stat, by = c('StationCode', 'Watershed')) %>%
  select(StationCode, Watershed, Date, Parameter, Result, Units, Type, Qualifier, Longitude, Latitude)

save(medat, file = here::here('data', 'medat.RData'), compress = 'xz')

# tissue concentrations ---------------------------------------------------

ts_dat <- read.csv(here::here('data/raw', 'NSMP Tissue Data.csv'), stringsAsFactors = F)
ts_stat <- read.csv(here::here('data/raw', 'NSMP_Stations.csv'), stringsAsFactors = F)

# organize tissue stations
tsstat <- ts_stat %>% 
  select(StationCode, Watershed, Latitude, Longitude)

# wrangle
tsdat <- ts_dat %>% 
  mutate(
    Date = mdy_hm(Date, tz = "Pacific/Pitcairn"),
    Date = as.Date(Date), 
    Parameter = case_when(
      Parameter %in% '% Solid' ~ 'Percent Solids', 
      Parameter %in% 'Percent Solid' ~ 'Percent Solids', 
      T ~ Parameter
    )
  ) %>% 
  rename(StationCode = Station) %>% 
  left_join(tsstat, by = 'StationCode') %>%
  select(StationCode, Date, Parameter, Result, Units, Qualifier, Longitude, Latitude)
  
# remove stations with less than 15 obs
tsdat <- tsdat %>% 
  group_by(StationCode) %>% 
  filter(n() > 15) %>% 
  ungroup()

save(tsdat, file = here::here('data', 'tsdat.RData'), compress = 'xz')

# loading data ------------------------------------------------------------

# constituents
metals <- c("Ag", "As", "Cd", "Cr", "Cu", "Fe", "Hg", "Ni", "Pb", "Se", 
            "Zn")
nutrs <- c('Ammonia', 'Nitrate, Nitrite', 'Total Kjeldahl Nitrogen', 'Orthophosphate', 'Total Phosphorus')

# 2011-2012.xlsx ----------------------------------------------------------

tmp1 <- read_excel(here::here('data/raw/meloads/2011-2012.xlsx'), sheet = '2 - Mass Lo-Loads (2)', skip = 2) %>% 
  .[-1, ] %>% 
  fill(Station) %>% 
  select(-Weather, -Sampled) %>% 
  gather('Parameter', 'Result', -Station, -Period, -Type) %>% 
  mutate(
    Period = gsub('\\-[0-9]+', '', Period), 
    Period = mdy(Period), 
    Parameter = case_when(
      Parameter %in% 'as N' ~ 'Ammonia', 
      Parameter %in% 'as P' ~ 'Orthophosphate', 
      Parameter %in% 'as PO4' ~ 'Total Phosphorus', 
      Parameter %in% 'As NO3' ~ 'Nitrate, Nitrite', 
      Parameter %in% 'TKN' ~ 'Total Kjeldahl Nitrogen', 
      T ~ Parameter
    ), 
    Result = as.numeric(Result),
    Units = case_when(
      Parameter %in% !!nutrs ~ 'tons', 
      Parameter %in% !!metals ~ 'lbs'
    )
  ) %>% 
  filter(Type %in% 'Total') %>% 
  filter(!Parameter %in% c('as CaCO3', 'TSS', 'VSS')) %>% 
  select(StationCode = Station, Date = Period, Parameter, Result, Units)

# 2012-2013.xlsx ----------------------------------------------------------

tmp2 <- read_excel(here::here('data/raw/meloads/2012-2013.xlsx'), sheet = '1 Table 1', skip = 5) %>% 
  .[-1, ] %>% 
  fill(Station) %>% 
  select(-Sampled) %>% 
  gather('Parameter', 'Result', -Station, -Period, -Type) %>% 
  mutate(
    Period = gsub('(^[a-z,A-Z]+\\s[0-9]+)\\-[a-z,A-Z]+\\s[0-9]+,\\s([0-9]+)$', '\\1, \\2', Period), 
    Period = gsub('\\-[0-9]*', '', Period),
    Period = mdy(Period), 
    Parameter = case_when(
      Parameter %in% 'as N' ~ 'Ammonia', 
      Parameter %in% 'as P' ~ 'Orthophosphate', 
      Parameter %in% 'as PO4' ~ 'Total Phosphorus', 
      Parameter %in% 'As NO3' ~ 'Nitrate, Nitrite', 
      Parameter %in% 'TKN' ~ 'Total Kjeldahl Nitrogen', 
      T ~ Parameter
    ), 
    Result = as.numeric(Result),
    Units = case_when(
      Parameter %in% !!nutrs ~ 'tons', 
      Parameter %in% !!metals ~ 'lbs'
    )
  ) %>% 
  filter(Type %in% 'Total') %>% 
  filter(!Parameter %in% c('as CaCO3', 'TSS', 'VSS')) %>% 
  select(StationCode = Station, Date = Period, Parameter, Result, Units)


# SAR TAb C-11-ll.1 -------------------------------------------------------

tmp3 <- read_excel(here::here('data/raw/meloads/SAR Tab C-11-II.1.xls'), skip = 2) %>% 
  .[-1, ] %>% 
  fill(Station) %>% 
  select(-Sampled, -Weather) %>% 
  gather('Parameter', 'Result', -Station, -Period, -Type) %>% 
  mutate(
    Period = gsub('(^[a-z,A-Z]+\\s[0-9]+)\\-[a-z,A-Z]+\\s[0-9]+,\\s([0-9]+)$', '\\1, \\2', Period), 
    Period = gsub('\\-[0-9]*', '', Period),
    Period = mdy(Period), 
    Parameter = case_when(
      Parameter %in% 'as N' ~ 'Ammonia', 
      Parameter %in% 'as P' ~ 'Orthophosphate', 
      Parameter %in% 'as PO4' ~ 'Total Phosphorus', 
      Parameter %in% 'As NO3' ~ 'Nitrate, Nitrite', 
      Parameter %in% 'TKN' ~ 'Total Kjeldahl Nitrogen', 
      T ~ Parameter
    ), 
    Result = as.numeric(Result),
    Units = case_when(
      Parameter %in% !!nutrs ~ 'tons', 
      Parameter %in% !!metals ~ 'lbs'
    )
  ) %>% 
  filter(Type %in% 'Total') %>% 
  filter(!Parameter %in% c('as CaCO3', 'TSS', 'VSS')) %>% 
  select(StationCode = Station, Date = Period, Parameter, Result, Units)

# SAR TAb C-11-ll.2 -------------------------------------------------------

tmp4 <- read_excel(here::here('data/raw/meloads/SAR Tab C-11-II.2.xls'), skip = 2) %>% 
  .[-1, ] %>% 
  fill(Station) %>% 
  select(-Sampled, -Weather) %>% 
  gather('Parameter', 'Result', -Station, -Period, -Type) %>% 
  mutate(
    Period = gsub('(^[a-z,A-Z]+\\s[0-9]+)\\-[a-z,A-Z]+\\s[0-9]+,\\s([0-9]+)$', '\\1, \\2', Period), 
    Period = gsub('\\-[0-9]*', '', Period),
    Period = mdy(Period), 
    Parameter = case_when(
      Parameter %in% 'as N' ~ 'Ammonia', 
      Parameter %in% 'as P' ~ 'Orthophosphate', 
      Parameter %in% 'as PO4' ~ 'Total Phosphorus', 
      Parameter %in% 'As NO3' ~ 'Nitrate, Nitrite', 
      Parameter %in% 'TKN' ~ 'Total Kjeldahl Nitrogen', 
      T ~ Parameter
    ), 
    Result = as.numeric(Result),
    Units = case_when(
      Parameter %in% !!nutrs ~ 'tons', 
      Parameter %in% !!metals ~ 'lbs'
    )
  ) %>% 
  filter(Type %in% 'Total') %>% 
  filter(!Parameter %in% c('as CaCO3', 'TSS', 'VSS')) %>% 
  select(StationCode = Station, Date = Period, Parameter, Result, Units)


# Tab C-11.I.1 Mass Emissions Storm Mass Loading --------------------------

tmp5 <- read_excel(here::here('data/raw/meloads/Tab C-11-I.1 Mass Emissions Storm Mass Loading.xlsx'), skip = 0) %>% 
  .[-1,] %>% 
  fill(Station) %>% 
  select(-`Flow Volume`) %>% 
  gather('Parameter', 'Result', -Station, -Date, -Type) %>% 
  mutate(
    Period = gsub('(^[a-z,A-Z]+\\s[0-9]+)\\-[a-z,A-Z]+\\s[0-9]+,\\s([0-9]+)$', '\\1, \\2', Date), 
    Period = gsub('\\-[0-9]*', '', Period),
    Period = mdy(Period), 
    Parameter = case_when(
      Parameter %in% 'Ammonia as N' ~ 'Ammonia', 
      Parameter %in% 'Ortho Phosphate as P' ~ 'Orthophosphate', 
      Parameter %in% 'Total Phosphorus as PO4' ~ 'Total Phosphorus', 
      Parameter %in% 'Nitrate Nitrite NO3' ~ 'Nitrate, Nitrite', 
      Parameter %in% 'TKN' ~ 'Total Kjeldahl Nitrogen', 
      T ~ Parameter
    ), 
    Result = as.numeric(Result),
    Units = case_when(
      Parameter %in% !!nutrs ~ 'tons', 
      Parameter %in% !!metals ~ 'lbs'
    )
  ) %>% 
  filter(Type %in% 'TOTAL') %>% 
  filter(!Parameter %in% c('as CaCO3', 'TSS', 'VSS')) %>% 
  select(StationCode = Station, Date = Period, Parameter, Result, Units) %>% 
  na.omit

# Tab C-11.II.1 Storm Mass Loading --------------------------

tmp6 <- read_excel(here::here('data/raw/meloads/Tab C-11-II.1 Storm Mass Loading.xlsx'), skip = 0) %>% 
  .[-1,] %>% 
  fill(Station) %>% 
  select(-`Volume (Ac-Ft)`) %>% 
  gather('Parameter', 'Result', -Station, -Date, -Type) %>% 
  mutate(
    Period = gsub('(^[a-z,A-Z]+\\s[0-9]+)\\-[a-z,A-Z]+\\s[0-9]+,\\s([0-9]+)$', '\\1, \\2', Date), 
    Period = gsub('\\-[0-9]*', '', Period),
    Period = mdy(Period),
    Parameter = case_when(
      Parameter %in% 'Ammonia as N' ~ 'Ammonia', 
      Parameter %in% 'Ortho Phosphate as P' ~ 'Orthophosphate', 
      Parameter %in% 'Total Phosphorus as PO4' ~ 'Total Phosphorus', 
      Parameter %in% 'Nitrate Nitrite NO3' ~ 'Nitrate, Nitrite', 
      Parameter %in% 'TKN' ~ 'Total Kjeldahl Nitrogen', 
      T ~ Parameter
    ), 
    Result = as.numeric(Result),
    Units = case_when(
      Parameter %in% !!nutrs ~ 'tons', 
      Parameter %in% !!metals ~ 'lbs'
    )
  ) %>% 
  filter(Type %in% 'TOTAL') %>% 
  filter(!Parameter %in% c('as CaCO3', 'TSS', 'VSS')) %>% 
  select(StationCode = Station, Date = Period, Parameter, Result, Units) 

# Tab C-11.II.1 Storm Mass Loads --------------------------

tmp7 <- read_excel(here::here('data/raw/meloads/Tab C-11-II.1 Storm Mass Loads.xlsx'), skip = 2) %>% 
  .[-1,] %>% 
  fill(Station) %>% 
  select(-`Sampled`, -Weather) %>% 
  gather('Parameter', 'Result', -Station, -Period, -Type) %>% 
  mutate(
    Period = gsub('(^[a-z,A-Z]+\\s[0-9]+)\\-[a-z,A-Z]+\\s[0-9]+,\\s([0-9]+)$', '\\1, \\2', Period), 
    Period = gsub('\\-[0-9]*', '', Period),
    Period = mdy(Period),
    Parameter = case_when(
      Parameter %in% 'as N' ~ 'Ammonia', 
      Parameter %in% 'as P' ~ 'Orthophosphate', 
      Parameter %in% 'as PO4' ~ 'Total Phosphorus', 
      Parameter %in% 'As NO3' ~ 'Nitrate, Nitrite', 
      Parameter %in% 'TKN' ~ 'Total Kjeldahl Nitrogen', 
      T ~ Parameter
    ), 
    Result = as.numeric(Result),
    Units = case_when(
      Parameter %in% !!nutrs ~ 'tons', 
      Parameter %in% !!metals ~ 'lbs'
    )
  ) %>% 
  filter(Type %in% 'Total') %>% 
  filter(!Parameter %in% c('as CaCO3', 'TSS', 'VSS')) %>% 
  select(StationCode = Station, Date = Period, Parameter, Result, Units) 

# Table C-11.II.1 --------------------------

tmp8 <- read_excel(here::here('data/raw/meloads/Table C-11.II.1.xls'), skip = 5) %>% 
  .[-1, ] %>% 
  fill(Station) %>% 
  select(-Sampled) %>% 
  gather('Parameter', 'Result', -Station, -Period, -Type) %>% 
  mutate(
    Period = gsub('(^[a-z,A-Z]+\\s[0-9]+)\\-[a-z,A-Z]+\\s[0-9]+,\\s([0-9]+)$', '\\1, \\2', Period), 
    Period = gsub('\\-[0-9]*', '', Period),
    Period = mdy(Period), 
    Parameter = case_when(
      Parameter %in% 'as N' ~ 'Ammonia', 
      Parameter %in% 'as P' ~ 'Orthophosphate', 
      Parameter %in% 'as PO4' ~ 'Total Phosphorus', 
      Parameter %in% 'As NO3' ~ 'Nitrate, Nitrite', 
      Parameter %in% 'TKN' ~ 'Total Kjeldahl Nitrogen', 
      T ~ Parameter
    ), 
    Result = as.numeric(Result),
    Units = case_when(
      Parameter %in% !!nutrs ~ 'tons', 
      Parameter %in% !!metals ~ 'lbs'
    )
  ) %>% 
  filter(Type %in% 'Total') %>% 
  filter(!Parameter %in% c('as CaCO3', 'TSS', 'VSS')) %>% 
  select(StationCode = Station, Date = Period, Parameter, Result, Units)

# Table C-11.II.1 Storm Mass Loading --------------------------

tmp9 <- read_excel(here::here('data/raw/meloads/Table C-11-II.1 Storm Mass Loading Final.xlsx'), skip = 1) %>% 
  .[-c(1, 2), ] %>% 
  rename(
    Station = `...1`,
    Period = `...2`,
    Type = `...3`,
    Sampled = `...4`,
  ) %>% 
  fill(Station) %>% 
  select(-Sampled) %>% 
  gather('Parameter', 'Result', -Station, -Period, -Type) %>% 
  mutate(
    Period = gsub('(^[a-z,A-Z]+\\s[0-9]+)\\s\\-\\s[a-z,A-Z]+\\s[0-9]+,\\s([0-9]+)$', '\\1, \\2', Period), 
    Period = gsub('\\-[0-9]*', '', Period),
    Period = mdy(Period), 
    Parameter = case_when(
      Parameter %in% 'Tot Ammonia as N' ~ 'Ammonia', 
      Parameter %in% 'OrthoPhosphate as P' ~ 'Orthophosphate', 
      Parameter %in% 'TotalPhosphorus as PO4' ~ 'Total Phosphorus', 
      Parameter %in% 'Nitrate+Nitrite as NO3' ~ 'Nitrate, Nitrite', 
      Parameter %in% 'TKN' ~ 'Total Kjeldahl Nitrogen', 
      T ~ Parameter
    ), 
    Result = as.numeric(Result),
    Units = case_when(
      Parameter %in% !!nutrs ~ 'tons', 
      Parameter %in% !!metals ~ 'lbs'
    )
  ) %>% 
  filter(Type %in% 'Total') %>% 
  filter(!Parameter %in% c('as CaCO3', 'TSS', 'VSS')) %>% 
  select(StationCode = Station, Date = Period, Parameter, Result, Units) %>% 
  na.omit

# combine all
lddat <- bind_rows(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9) %>% 
  unique

save(lddat, file = here::here('data/lddat.RData'), compress = 'xz')

# power analysis for trends -----------------------------------------------

data(medat)

metals <- c("Ag", "As", "Cd", "Cr", "Cu", "Fe", "Hg", "Ni", "Pb", "Se", 
            "Zn")
organs <- c("Azinphos methyl (Guthion)", "Bolstar", "Chlorpyrifos", "Coumaphos", 
            "Demeton-o", "Demeton-s", "Diazinon", "Dichlorvos", "Dimethoate", 
            "Disulfoton", "Ethoprop", "Ethyl Parathion", "Fensulfothion", 
            "Fenthion", "GLYP", "Malathion", "Merphos", "Mevinphos", 
            "Parathion-methyl", "Phorate", "Ronnel", "Tetrachlorovinphos", 
            "Tokuthion", "Trichloronate")
nutrs <- c('Ammonia', 'Nitrate, Nitrite', 'Total Kjeldahl Nitrogen', 'Orthophosphate', 'Total Phosphorus')

powdat <- medat %>% 
  filter(Parameter %in% c(metals, organs, nutrs))

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
  eff = seq(0.1, 2,length = 10), 
  wxt = c('D', 'S')
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
  wxt <- scns[i, ][['wxt']]
  
  topow <- powdat %>% 
    filter(StationCode %in% sta) %>% 
    filter(Parameter %in% par) %>% 
    filter(Type %in% wxt) %>% 
    arrange(Date)
  
  simdat <- try({simvals(topow, chg = chg, eff = eff, sims = 1000)})
  
  if(inherits(simdat, 'try-error'))
    return(NA)
  
  out <- try({powfun(simdat)})
  
  if(inherits(out, 'try-error'))
    return(NA)
  
  return(out)
  
}

# combine results with scns
pows <- scns %>% 
  mutate(
    pow = unlist(res)
  )

# add watersheds
sheds <- medat %>% 
  select(sta = StationCode, Watershed) %>% 
  unique
pows <- pows %>% 
  left_join(sheds, by = 'sta')
  
save(pows,file = here::here('data', 'pows.RData'), compress = 'xz')

# power analysis for threshold values ------------------------------------

data(medat)

metals <- c("Ag", "As", "Cd", "Cr", "Cu", "Fe", "Hg", "Ni", "Pb", "Se", 
            "Zn")
organs <- c("Azinphos methyl (Guthion)", "Bolstar", "Chlorpyrifos", "Coumaphos", 
            "Demeton-o", "Demeton-s", "Diazinon", "Dichlorvos", "Dimethoate", 
            "Disulfoton", "Ethoprop", "Ethyl Parathion", "Fensulfothion", 
            "Fenthion", "GLYP", "Malathion", "Merphos", "Mevinphos", 
            "Parathion-methyl", "Phorate", "Ronnel", "Tetrachlorovinphos", 
            "Tokuthion", "Trichloronate")
nutrs <- c('Ammonia', 'Nitrate, Nitrite', 'Total Kjeldahl Nitrogen', 'Orthophosphate', 'Total Phosphorus')

# data to eval
scns <- medat %>% 
  filter(Parameter %in% c(metals, organs, nutrs)) %>% 
  filter(!StationCode %in% 'SICG03') %>% 
  mutate(
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
  
  # if no variation return NA
  if(length(unique(dat$Result)) == 1)
    return(NA)
  
  # model to estimate variance components
  modin <- try({lm(log(Result) ~ dectime, data = dat)})
  
  if(inherits(modin, 'try-error'))
    return(NA)
  
  varres <- resid(modin) %>% sd
  medval <- median(dat$Result, na.rm = T)
  
  topval <- qnorm(0.95, medval, varres)
  
  grids <- crossing(
    vals = seq(medval, topval, length.out = 10), 
    effs = seq(0.1, 1, length.out = 10), 
    sims = 1:1000,
    ) %>% 
    group_by(vals, effs, sims) %>% 
    mutate(
      pow = purrr::pmap(list(vals, effs), function(vals, effs, sims){
        
        simeff <- nrow(dat) * effs
        sims <- rnorm(simeff, medval, varres)
        # browser()
        # any(sims > vals)
        pval <- t.test(sims, mu = vals, alternative = 'less')$p.value
        # pow <- sum(sims > vals) / length(sims)
        
        return(pval)
        
      })
    ) %>% 
    unnest(pow) %>%
    group_by(vals, effs) %>% 
    summarise(pow = mean(pow))
  
  return(grids)
  
}

# combine results with scns
thrs <- scns %>% 
  bind_cols(enframe(res)) %>% 
  select(-data, -name) %>%   
  unnest(value) %>% 
  select(-value)
    
save(thrs, file = here::here('data', 'thrs.RData'), compress = 'xz')

# optimal effort by station, constituent ----------------------------------

source('R/funcs.R')

data(pows)
data(medat)

opteff <- pows %>% 
  crossing(., powin = seq(0.1, 0.9, by = 0.1)) %>% 
  group_by(par, sta, wxt, powin) %>% 
  nest %>% 
  mutate(
    opt = purrr::pmap(list(data, powin), function(data, powin) getopt(datin = data, pow = powin))
  ) %>% 
  dplyr::select(-data) %>% 
  unnest(opt) %>% 
  dplyr::select(-opt) %>% 
  na.omit %>% 
  ungroup

# add watersheds
sheds <- medat %>% 
  select(sta = StationCode, Watershed) %>% 
  unique
opteff <- opteff %>% 
  left_join(sheds, by = 'sta')

save(opteff, file = here::here('data', 'opteff.RData'), compress = 'xz')

# tissue power analysis for trends -----------------------------------------------

data(tsdat)

# tops
ext <- c('Se-T', 'Se', '2,4-DDD', '2,4-DDE', '2,4-DDT', '4,4-DDD', '4,4-DDE', '4,4-DDT')
tops <- table(tsdat$Parameter) %>% sort %>% rev %>% .[1:10] %>% names %>% c(., ext) %>% sort

powdat <- tsdat %>% 
  filter(Parameter %in% tops)

scns <- crossing(
  sta = unique(powdat$StationCode),
  par = unique(powdat$Parameter), 
  chg = seq(0.1, 1, length = 10),
  eff = seq(0.1, 2,length = 10)
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
  
  sta <- scns[i, ][['sta']]
  par <- scns[i, ][['par']]
  chg <- scns[i, ][['chg']]
  eff <- scns[i, ][['eff']]
  
  topow <- powdat %>% 
    filter(StationCode %in% sta) %>% 
    filter(Parameter %in% par) %>% 
    arrange(Date)
  
  simdat <- try({simvals(topow, chg = chg, eff = eff, sims = 1000)})
  
  if(inherits(simdat, 'try-error'))
    return(NA)
  
  out <- try({powfun(simdat)})
  
  if(inherits(out, 'try-error'))
    return(NA)
  
  return(out)
  
}

# combine results with scns
tspows <- scns %>% 
  mutate(
    pow = unlist(res)
  )

save(tspows,file = here::here('data', 'tspows.RData'), compress = 'xz')

# tissue power analysis for threshold values ------------------------------------

data(tsdat)

ext <- c('Se-T', 'Se', '2,4-DDD', '2,4-DDE', '2,4-DDT', '4,4-DDD', '4,4-DDE', '4,4-DDT')
tops <- table(tsdat$Parameter) %>% sort %>% rev %>% .[1:10] %>% names %>% c(., ext) %>% sort

# data to eval
scns <- tsdat %>% 
  filter(Parameter %in% tops) %>% 
  mutate(
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
  
  # if no variation return NA
  if(length(unique(dat$Result)) == 1)
    return(NA)
  
  # model to estimate variance components
  modin <- try({lm(log(Result) ~ dectime, data = dat)})
  
  if(inherits(modin, 'try-error'))
    return(NA)
  
  varres <- resid(modin) %>% sd
  medval <- median(dat$Result, na.rm = T)
  
  topval <- qnorm(0.95, medval, varres)
  
  grids <- crossing(
    vals = seq(medval, topval, length.out = 10), 
    effs = seq(0.1, 1, length.out = 10), 
    sims = 1:1000,
  ) %>% 
    group_by(vals, effs, sims) %>% 
    mutate(
      pow = purrr::pmap(list(vals, effs), function(vals, effs, sims){
        
        simeff <- nrow(dat) * effs
        sims <- rnorm(simeff, medval, varres)
        # browser()
        # any(sims > vals)
        pval <- try(t.test(sims, mu = vals, alternative = 'less')$p.value)
        # pow <- sum(sims > vals) / length(sims)
        
        if(inherits(pval, 'try-error'))
          return(NA)
        
        return(pval)
        
      })
    ) %>% 
    unnest(pow) %>%
    group_by(vals, effs) %>% 
    summarise(pow = mean(pow, na.rm = T))
  
  return(grids)
  
}

# combine results with scns
tsthrs <- scns %>% 
  bind_cols(enframe(res)) %>% 
  select(-data, -name) %>%   
  mutate(
    value = purrr::map(value, function(x){
      if(is.logical(x))
        out <- tibble(vals = NA, effs = NA, pow = NA)
      else 
        out <- x
      
      return(out)
      
    })
  ) %>% 
  unnest(value)

save(tsthrs, file = here::here('data', 'tsthrs.RData'), compress = 'xz')

# tissue optimal effort by station, constituent ----------------------------------

source('R/funcs.R')

data(tspows)

tsopteff <- tspows %>% 
  crossing(., powin = seq(0.1, 0.9, by = 0.1)) %>% 
  group_by(par, sta, powin) %>% 
  nest %>% 
  mutate(
    opt = purrr::pmap(list(data, powin), function(data, powin) getopt(datin = data, pow = powin))
  ) %>% 
  dplyr::select(-data) %>% 
  unnest(opt) %>% 
  dplyr::select(-opt) %>% 
  na.omit %>% 
  ungroup

save(tsopteff, file = here::here('data', 'tsopteff.RData'), compress = 'xz')

