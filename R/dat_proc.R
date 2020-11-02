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

# harbors and estuaries ---------------------------------------------------

metals <- c("Ag", "As", "Cd", "Cr", "Cu", "Fe", "Hg", "Ni", "Pb", "Se",
            "Zn")
organs <- c("2,4'-D", "2,4-DB", "2,4,5 TP-Silvex", "2,4,5,-T", "Aldrin", 
            "Allethrin", "Alpha-BHC", "Atrazine", "Azinphos methyl (Guthion)", 
            "Be", "Beta-BHC", "Bifenthrin", "Bolstar", "Chlordane", "Chlorpyrifos", 
            "Cis-Permethrin", "Coumaphos", "Cyfluthrin", "Cypermethrin", 
            "Dalapon", "DDT", "Delta-BHC", "Deltamethrin", "Demeton-o", "Demeton-s", 
            "Diazinon", "Dicamba", "Dichlorprop", "Dichlorvos", "Dieldrin", 
            "Dimethoate", "Dinoseb", "Disulfoton", "Endosulfan I", "Endosulfan II", 
            "Endosulfan Sulfate", "Endrin", "Endrin Aldehyde", "Endrin Ketone", 
            "Ethoprop", "Ethyl Parathion", "Fensulfothion", "Fenthion", "Gamma-BHC", 
            "GLYP", "Heptachlor Epoxide", "HPAH", "L-Cyhalothrin", "LPAH", 
            "Malathion", "MCPA", "MCPP", "Merphos", "Mevinphos", "Mirex", 
            "Nitrogen-S", "OxyChlordane", "Parathion-methyl", "PCB", "Permethrin", 
            "Perthane", "pH", "Phorate", "Phosphorus-S", "Prallethrin", "Prometon", 
            "Prometryn", "Ronnel", "Sb", "Simazine", "Tetrachlorovinphos", 
            "Tl", "TOC-S", "Tokuthion", "Total chlordane", "Toxaphene", "Trans-Permethrin", 
            "Trichloronate")
nutrs <- c('Ammonia', 'Nitrate, Nitrite', 'Total Kjeldahl Nitrogen', 'Orthophosphate', 'Total Phosphorus', 'Phosphorus-S', 'Nitrogen-S')

# water column chem
wcchem1 <- read.csv('data/raw/Harbors & Estuaries Water Column Chemistry 2010-2015.csv')
wcchem2 <- read.csv('data/raw/Harbors & Estuaries Water Column Chemistry 2015-2020.csv')

wcchem <- bind_rows(wcchem1, wcchem2) %>% 
  select(-Program, -LogNumber, -Analysis, -Result.Type, -Filtered, -MatrixCode, -Type, -Sample.Type, -QA.Type, -SampleDepth) %>% 
  mutate(
    Date = mdy_hm(Date, tz = 'Pacific/Pitcairn'), 
    Date = as.Date(Date)
    ) %>% 
  group_by(Station, Date, Parameter, Units, WaterShed) %>% 
  summarise(
    Result = mean(Result, na.rm  = T),
    Qualifier = unique(Qualifier), 
    .groups = 'drop'
  ) %>% 
  mutate(
    Parameter = case_when(
      Parameter == 'AmmoniaN' ~ 'Ammonia', 
      Parameter == 'TKN' ~ 'Total Kjeldahl Nitrogen',
      Parameter == 'OrthoPhosphateP' ~ 'Orthophosphate', 
      Parameter == 'TotalPhosphorusPO4' ~ 'Total Phosphorus', 
      Parameter == 'NitrateNitriteNO3' ~ 'Nitrate, Nitrite',
      grepl('DDD$|DDE$|DDT$', Parameter) ~ 'DDT', 
      grepl('^PCB', Parameter) ~ 'PCB', 
      grepl('^Chlordane-alpha$|^Chlordane-gamma$|^cis-Nonachlor$|^trans-Nonachlor$|^Cis-Nonachlor$|^Trans-Nonachlor$|^Heptachlor$|^Methoxychlor$', Parameter) ~ 'Total chlordane',
      T ~ Parameter
    )
  ) %>% 
  group_by(Station, Date, Parameter, Units, WaterShed, Qualifier) %>% 
  summarise(
    Result = sum(Result, na.rm = T), 
    .groups = 'drop'
  ) %>% 
  group_by(Parameter) %>% 
  filter(n() > 30) %>% 
  ungroup %>% 
  filter(Parameter %in% c(nutrs, metals, organs)) %>%
  mutate(
    location = 'wc'
  )

sedchem <- read.csv('data/raw/SAR Harbors & Estuaries Sediment Chemistry Data.csv') %>% 
  select(-Entry.Set, -Program, -LogNumber, -Analysis, -Result.Type, -Filtered, -MatrixCode, -Type, -Sample.Type, -QA.Type, -SampleDepth) %>% 
  mutate(
    Date = mdy_hm(Date, tz = 'Pacific/Pitcairn'), 
    Date = as.Date(Date)
  ) %>% 
  group_by(Station, Date, Parameter, Units, WaterShed) %>% 
  summarise(
    Result = mean(Result, na.rm  = T),
    Qualifier = unique(Qualifier), 
    .groups = 'drop'
  ) %>% 
  mutate(
    Parameter = case_when(
      grepl('DDD$|DDE$|DDT$', Parameter) ~ 'DDT', 
      grepl('^PCB', Parameter) ~ 'PCB', 
      grepl('^Chlordane-alpha$|^Chlordane-gamma$|^cis-Nonachlor$|^trans-Nonachlor$|^Cis-Nonachlor$|^Trans-Nonachlor$|^Heptachlor$|^Methoxychlor$', Parameter) ~ 'Total chlordane',
      grepl('^1\\-methylnaphthalene$|^1\\-Methylnaphthalene$|^1\\-Methylphenanthrene$|^2\\-methylnaphthalene$|^2\\-Methylnaphthalene$|^2\\,6\\-Dimethylnaphthalene$|^Acenaphthene$|^Acenapthene$|^Acenapthylene$|^Anthracene$|^Biphenyl$|^Fluorene$|^Naphthalene$|^Phenanthrene$', Parameter) ~ 'LPAH',
      grepl('^\\(1\\,2\\,3\\-CE\\)Pyrene$|^1\\,2\\,5\\,6\\-\\sDibenzanthracene$|^Benzo\\s\\(A\\)\\sAnthracene$|^Benzo\\s\\(A\\)\\sPyrene$|^Benzo\\s\\(GHI\\)\\sPerylene$|^Benzo\\s\\(K\\)\\sFluoranthene$|^Benzo\\(b\\)Fluoranthene$|^Benzo\\(e\\)pyrene$|^Benzo\\[a\\]anthracene$|^Benzo\\[a\\]pyrene$|^Benzo\\[e\\]pyrene$|^Chrysene$|^Dibenz\\[a\\,h\\]anthracene$|^Fluoranthene$|^Perylene$|^Pyrene$', Parameter) ~ 'HPAH',
      T ~ Parameter
    )
  ) %>% 
  group_by(Station, Date, Parameter, Units, WaterShed, Qualifier) %>% 
  summarise(
    Result = sum(Result, na.rm = T), 
    .groups = 'drop'
  ) %>% 
  group_by(Parameter) %>% 
  filter(n() > 30) %>% 
  ungroup %>% 
  filter(Parameter %in% c(nutrs, metals, organs)) %>%
  mutate(
    location = 'sd'
  )

# station locations
sdloc <- read.csv('data/raw/Harbors & Estuaries Station Locations.csv')

sddat <- bind_rows(wcchem, sedchem) %>% 
  rename(
    StationCode = Station,
    Watershed = WaterShed
    ) %>% 
  mutate(
    Watershed = gsub('Harbour$', 'Harbor', Watershed)
  ) %>% 
  left_join(sdloc, by = 'StationCode')

save(sddat, file = 'data/sddat.RData', compress = 'xz')

# tissue concentrations ---------------------------------------------------

# station location
ts_stat <- read.csv(here::here('data/raw', 'NSMP_Stations.csv'), stringsAsFactors = F)

# organize tissue stations
tsstat <- ts_stat %>%
  select(StationCode, Watershed, Latitude, Longitude) %>% 
  unique %>% 
  arrange(StationCode)

# import, fix names
fl <- 'data/raw/Newport tissue data June 2020 update.xlsx'
renms <- read_excel(here::here(fl), sheet = 'wet wt OCPs') 
Parameters <- names(renms)
Parenms <- read_excel(here::here(fl), sheet = 'wet wt OCPs', skip = 1)
names(renms)[grepl('\\.\\.\\.', names(renms))] <- names(Parenms)[grepl('\\.\\.\\.', names(renms))]
Parameters <- names(renms)
names(Parenms) <- Parameters

# format long, sum by DDT, PCB, total chlordanes
tsdat <- Parenms %>% 
  select(-`CH2 ID`, -`EntrySet`, -`SampleID`, -`Physis ID`) %>% 
  rename(
    StationCode = Station,
    Type = `Bird egg/fillet/composite`
  ) %>% 
  gather('Parameter', 'Result', -StationCode, -Type, -Date, -Species) %>% 
  mutate(
    Date = as.Date(Date),
    Qualifier = case_when(
      grepl('^<', Result) ~ '<', 
      grepl('e$', Result) ~ 'e', 
    ), 
    Result = gsub('^<|e$|^NR$|^2\\.32\\.554$|^0>314', '', Result),
    Result = gsub('ND', '0', Result),
    Result = as.numeric(Result)
  ) %>% 
  mutate(
    Parameter = case_when(
      grepl('DDD$|DDE$|DDT$', Parameter) ~ 'DDT', 
      grepl('^PCB', Parameter) ~ 'PCB', 
      grepl('^Chlordane-alpha$|^Chlordane-gamma$|^cis-Nonachlor$|^trans-Nonachlor$', Parameter) ~ 'Total chlordane',
      T ~ Parameter
    )
  ) %>% 
  group_by(Species, StationCode, Type, Date, Parameter, Qualifier) %>% 
  summarise(Result = sum(Result, na.rm = T)) %>% 
  ungroup %>% 
  filter(!Species %in% '??') %>% 
  mutate(
    StationCode2 = StationCode,
    StationCode = case_when(
      StationCode == 'SDC' ~ 'SDC@IRWD', 
      StationCode == 'UCI' ~ 'UCIP-7NSMP', 
      StationCode == 'BCGC' ~ 'BCW_GC1', 
      StationCode == 'PCW' ~ 'PCW_WARN', 
      StationCode == 'BCW' ~ 'BCW_NSMP', 
      StationCode == 'SAD' ~ 'SADF01', 
      StationCode == 'uPCW' ~ 'UPCW', 
      T ~ StationCode
    ),
    Units = case_when(
      Parameter %in% c('%Lipid', '%Solids') ~ '%', 
      Parameter %in% 'Se' ~ 'ug/g dw', 
      Parameter %in% 'Hg' ~ 'ng/g dw',
      T ~ 'ng/g ww'
    )
  ) %>% 
  left_join(tsstat, by = 'StationCode') %>% 
  mutate(StationCode = StationCode2) %>% 
  select(-StationCode2)

save(tsdat, file = here::here('data', 'tsdat.RData'), compress = 'xz')

# constituent thresholds --------------------------------------------------

# these apply only to dry weather monitoring
thrsdat <- read_csv(here::here('data/raw/thresholds.csv')) %>% 
  select_if(~!any(is.na(.))) %>% 
  select(-StationCode) %>% 
  unique %>% 
  gather('Parameter', 'Threshold', everything())

save(thrsdat, file = here::here('data/thrsdat.RData'), compress = 'xz')

# mass emission thresholds ------------------------------------------------

data(medat)

me_dat <- read.csv(here::here('data/raw', 'ME+NUT_ALL_DATA.csv'), stringsAsFactors = F)
me_stat <- read.csv(here::here('data/raw', 'ME_Stations.csv'), stringsAsFactors = F)

# get hardness data, metals have variable wqo (thresholds) based on hardness
mehard <- me_dat %>% 
  mutate(
    Date = mdy_hm(Date, tz = 'Pacific/Pitcairn'), 
    Date = as.Date(Date), 
    Parameter = case_when(
      Parameter %in% 'Hardness as CaCO3' ~ 'Hardness',
      T ~ Parameter
    )
  ) %>% 
  rename(StationCode = Station) %>% 
  inner_join(me_stat, by = c('StationCode', 'Watershed')) %>%
  filter(Parameter %in% 'Hardness') %>% 
  select(StationCode, Watershed, Date, Parameter, Result, Units) %>% 
  group_by(StationCode) %>% 
  summarise(hardness = median(Result))

# format threshold data based on info in raw csv file
# metals thresholds that vary with hardness are based on equations and median hardness
# ammonia varies with ph and temp, but here is based on average of tables 4-2, 4-3
# in https://www.waterboards.ca.gov/santaana/water_issues/programs/basin_plan/docs/2019/New/Chapter_4_June_2019.pdf
methrsdat <- read_csv(here::here('data/raw/template3 sg.csv'), na = 'N/A') %>% 
  mutate_all(function(x) gsub('\\sng/L|\\sug/L|\\smg/L', '', x)) %>% 
  .[1:16, ] %>% 
  mutate(`Parathion-methyl` = 0.013) %>% 
  mutate_at(vars(Ag, Ammonia, Cd, Cr, Cu, Ni, Pb, Zn), function(x) NA) %>% 
  mutate_at(vars(-StationCode), as.numeric) %>% 
  gather('par', 'Threshold', -StationCode) %>% 
  left_join(mehard, by = 'StationCode') %>% 
  mutate(
    Threshold = case_when(
      par == 'Ag' ~ 0.85 * exp(1.72 * log(hardness) - 6.52), 
      par == 'Cd' ~ (1.101672 - log(hardness) * 0.041838) * exp(0.7852 * log(hardness) - 2.715), 
      par == 'Cr' ~ 0.86 * exp(0.819 * log(hardness) + 1.561), 
      par == 'Cu' ~ 0.96 * exp(0.8545 * log(hardness) - 1.702), 
      par == 'Ni' ~ 0.997 * exp(0.846 * log(hardness) + 0.0584), 
      par == 'Pb' ~ (1.46203 - log(hardness) * 0.145712) * exp(1.273 * log(hardness) - 4.705), 
      par == 'Zn' ~ 0.986 * exp(0.8473 * log(hardness) + 0.884), 
      par == 'Ammonia' ~ 0.489605844, # this is average of all values in Santa Ana River Basin Plan
      T ~ Threshold
    )
  ) %>% 
  select(-hardness) %>% 
  rename(sta = StationCode)
  
save(methrsdat, file = here::here('data/methrsdat.RData'))


# harbors and estuaries thresholds ----------------------------------------

# the raw file was made my hand from harbors_estuaries_thresholds.csv
sdthrsdat <- read.csv('data/raw/sdthrsdat.csv', stringsAsFactors = F)

save(sdthrsdat, file = 'data/sdthrsdat.RData', compress = 'xz')

# dry weather sampling tmdl waterbodies -----------------------------------


tmdldat <- read_csv(here::here('data/raw/tmdlwaterbodies.csv')) %>% 
  gather('var', 'val', -StationCode) %>% 
  na.omit() %>% 
  mutate(
    ind = gsub('^.*([0-9])$', '\\1', var), 
    var = gsub('[0-9]', '', var)
  ) %>% 
  spread(var, val) %>% 
  select(StationCode, Receiving = receiving, Parameter = param, ind) %>% 
  arrange(StationCode, ind) %>% 
  mutate(
    Parameter = case_when(
      Parameter %in% 'Selenium' ~ 'Se', 
      Parameter %in% 'E. coli' ~ 'EC', 
      Parameter %in% 'Sediment' ~ 'TSS', 
      Parameter %in% 'Pesticides/PCBs' ~ 'Pesticides',
      T ~ Parameter
    )
  )

save(tmdldat, file = here::here('data/tmdldat.RData'), compress = 'xz')

# loading data ------------------------------------------------------------

data(medat)

wshd <- medat %>% 
  select(StationCode, Watershed, Longitude, Latitude) %>% 
  unique

# constituents
metals <- c("Ag", "As", "Cd", "Cr", "Cu", "Fe", "Hg", "Ni", "Pb", "Se", 
            "Zn")
nutrs <- c('Ammonia', 'Nitrate, Nitrite', 'Total Kjeldahl Nitrogen', 'Orthophosphate', 'Total Phosphorus')

# 2011-2012.xlsx ----------------------------------------------------------

tmp1 <- read_excel(here::here('data/raw/meloads/2011-2012.xlsx'), sheet = '2 - Mass Lo-Loads (2)', skip = 2) %>% 
  .[-1, ] %>% 
  fill(Station) %>% 
  select(-Weather) %>% 
  gather('Parameter', 'Result', -Station, -Sampled, -Period, -Type) %>% 
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
  select(StationCode = Station, Date = Period, Parameter, Flow = Sampled, Result, Units)

# 2012-2013.xlsx ----------------------------------------------------------

tmp2 <- read_excel(here::here('data/raw/meloads/2012-2013.xlsx'), sheet = '1 Table 1', skip = 5) %>% 
  .[-1, ] %>% 
  fill(Station) %>% 
  gather('Parameter', 'Result', -Station, -Sampled, -Period, -Type) %>% 
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
  select(StationCode = Station, Date = Period, Parameter, Flow = Sampled, Result, Units)


# SAR TAb C-11-ll.1 -------------------------------------------------------

tmp3 <- read_excel(here::here('data/raw/meloads/SAR Tab C-11-II.1.xls'), skip = 2) %>% 
  .[-1, ] %>% 
  fill(Station) %>% 
  select(-Weather) %>% 
  gather('Parameter', 'Result', -Station, -Sampled, -Period, -Type) %>% 
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
  select(StationCode = Station, Date = Period, Parameter, Flow = Sampled, Result, Units)

# SAR TAb C-11-ll.2 -------------------------------------------------------

tmp4 <- read_excel(here::here('data/raw/meloads/SAR Tab C-11-II.2.xls'), skip = 2) %>% 
  .[-1, ] %>% 
  fill(Station) %>% 
  select(-Weather) %>% 
  gather('Parameter', 'Result', -Station, -Sampled, -Period, -Type) %>% 
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
  select(StationCode = Station, Date = Period, Parameter, Flow = Sampled, Result, Units)


# Tab C-11.I.1 Mass Emissions Storm Mass Loading --------------------------

tmp5 <- read_excel(here::here('data/raw/meloads/Tab C-11-I.1 Mass Emissions Storm Mass Loading.xlsx'), skip = 0) %>% 
  .[-1,] %>% 
  fill(Station) %>% 
  gather('Parameter', 'Result', -Station, -`Flow Volume`, -Date, -Type) %>% 
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
  select(StationCode = Station, Date = Period, Parameter, Flow = `Flow Volume`, Result, Units) %>% 
  na.omit

# Tab C-11.II.1 Storm Mass Loading --------------------------

tmp6 <- read_excel(here::here('data/raw/meloads/Tab C-11-II.1 Storm Mass Loading.xlsx'), skip = 0) %>% 
  .[-1,] %>% 
  fill(Station) %>% 
  gather('Parameter', 'Result', -Station, -`Volume (Ac-Ft)`, -Date, -Type) %>% 
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
  select(StationCode = Station, Date = Period, Parameter, Flow = `Volume (Ac-Ft)`, Result, Units) 

# Tab C-11.II.1 Storm Mass Loads --------------------------

tmp7 <- read_excel(here::here('data/raw/meloads/Tab C-11-II.1 Storm Mass Loads.xlsx'), skip = 2) %>% 
  .[-1,] %>% 
  fill(Station) %>% 
  select(-Weather) %>% 
  gather('Parameter', 'Result', -Station, -Sampled, -Period, -Type) %>% 
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
  select(StationCode = Station, Date = Period, Parameter, Flow = Sampled, Result, Units) 

# Table C-11.II.1 --------------------------

tmp8 <- read_excel(here::here('data/raw/meloads/Table C-11.II.1.xls'), skip = 5) %>% 
  .[-1, ] %>% 
  fill(Station) %>%  
  gather('Parameter', 'Result', -Station, -Sampled, -Period, -Type) %>% 
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
  select(StationCode = Station, Date = Period, Parameter, Flow = Sampled, Result, Units)

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
  gather('Parameter', 'Result', -Station, -Sampled, -Period, -Type) %>% 
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
  select(StationCode = Station, Date = Period, Parameter, Flow = Sampled, Result, Units) %>% 
  na.omit

# combine all
lddat <- bind_rows(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9) %>% 
  unique

# add watershed
lddat <- lddat %>% 
  left_join(wshd, by = 'StationCode') %>% 
  mutate(Flow = as.numeric(Flow))

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
tops <- c("%Lipid", "%Solids", "DDT", "Hg", "PCB", "Se", "Total chlordane", "Toxaphene")

powdat <- tsdat %>% 
  filter(Parameter %in% tops)

# data to eval, scns is not created all with crossing to minimize number of combos with no data
scns <- tsdat %>% 
  select(StationCode, Parameter, Type, Species) %>% 
  unique %>% 
  rename(
    sta = StationCode, 
    par = Parameter, 
    typ = Type,
    spp = Species
  ) %>% 
  crossing(
    .,
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
  typ <- scns[i, ][['typ']]
  spp <- scns[i, ][['spp']]
  chg <- scns[i, ][['chg']]
  eff <- scns[i, ][['eff']]
  
  topow <- powdat %>% 
    filter(StationCode %in% sta) %>% 
    filter(Parameter %in% par) %>% 
    filter(Type %in% typ) %>% 
    filter(Species %in% spp) %>% 
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

# tops
tops <- c("%Lipid", "%Solids", "DDT", "Hg", "PCB", "Se", "Total chlordane", "Toxaphene")

# data to eval
scns <- tsdat %>% 
  filter(Parameter %in% tops) %>% 
  mutate(
    Year = year(Date), 
    Season = yday(Date),
    dectime = decimal_date(Date)
  ) %>% 
  group_by(StationCode, Parameter, Type, Species) %>% 
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
  group_by(par, sta, typ, spp, powin) %>% 
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

# tissue power analysis for years from current ----------------------------

data(tsdat)

# tops
tops <- c("%Lipid", "%Solids", "DDT", "Hg", "PCB", "Se", "Total chlordane", "Toxaphene")

powdat <- tsdat %>% 
  filter(Parameter %in% tops)

# years in tissue data record, for simextfun
tsyrs <- tsdat %>% 
  mutate(Year = year(Date)) %>% 
  pull(Year) %>% 
  unique 

# data to eval, scns is not created all with crossing to minimize number of combos with no data
scns <- tsdat %>% 
  select(StationCode, Parameter, Type, Species) %>% 
  unique %>% 
  rename(
    sta = StationCode, 
    par = Parameter, 
    typ = Type,
    spp = Species
  ) %>% 
  crossing(
    .,
    chg = seq(0.1, 1, length = 10),
    yrs = seq(5, 50,length = 10)
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
  typ <- scns[i, ][['typ']]
  spp <- scns[i, ][['spp']]
  chg <- scns[i, ][['chg']]
  yrs <- scns[i, ][['yrs']]
  
  topow <- powdat %>% 
    filter(StationCode %in% sta) %>% 
    filter(Parameter %in% par) %>% 
    filter(Type %in% typ) %>% 
    filter(Species %in% spp) %>% 
    arrange(Date)
  
  simdat <- try({simextvals(topow, chg = chg, yrs = yrs, origminyr = min(tsyrs), origmaxyr = max(tsyrs), sims = 1000)})
  
  if(inherits(simdat, 'try-error'))
    return(NA)
  
  out <- try({powfun(simdat)})
  
  if(inherits(out, 'try-error'))
    return(NA)
  
  return(out)
  
}

# combine results with scns
tsextpows <- scns %>% 
  mutate(
    pow = unlist(res)
  )

save(tsextpows,file = here::here('data', 'tsextpows.RData'), compress = 'xz')

# tisue optimal effort, years from current --------------------------------

source('R/funcs.R')

data(tsextpows)

tsopteffext <- tsextpows %>% 
  crossing(., powin = seq(0.1, 0.9, by = 0.1)) %>% 
  rename(eff = yrs) %>% 
  group_by(par, sta, typ, spp, powin) %>%
  nest %>% 
  mutate(
    opt = purrr::pmap(list(data, powin), function(data, powin) getopt(datin = data, pow = powin))
  ) %>% 
  dplyr::select(-data) %>% 
  unnest(opt) %>% 
  dplyr::select(-opt) %>% 
  na.omit %>% 
  ungroup %>% 
  rename(yrs = eff)

save(tsopteffext, file = here::here('data', 'tsopteffext.RData'), compress = 'xz')

# harbors and estuaries power analysis for trends -------------------------

data(sddat)

# constituents
metals <- c("Ag", "As", "Cd", "Cr", "Cu", "Fe", "Hg", "Ni", "Pb", "Se",
            "Zn")
organs <- c("2,4'-D", "2,4-DB", "2,4,5 TP-Silvex", "2,4,5,-T", "Aldrin", 
            "Allethrin", "Alpha-BHC", "Atrazine", "Azinphos methyl (Guthion)", 
            "Be", "Beta-BHC", "Bifenthrin", "Bolstar", "Chlordane", "Chlorpyrifos", 
            "Cis-Permethrin", "Coumaphos", "Cyfluthrin", "Cypermethrin", 
            "Dalapon", "DDT", "Delta-BHC", "Deltamethrin", "Demeton-o", "Demeton-s", 
            "Diazinon", "Dicamba", "Dichlorprop", "Dichlorvos", "Dieldrin", 
            "Dimethoate", "Dinoseb", "Disulfoton", "Endosulfan I", "Endosulfan II", 
            "Endosulfan Sulfate", "Endrin", "Endrin Aldehyde", "Endrin Ketone", 
            "Ethoprop", "Ethyl Parathion", "Fensulfothion", "Fenthion", "Gamma-BHC", 
            "GLYP", "Heptachlor Epoxide", "HPAH", "L-Cyhalothrin", "LPAH", 
            "Malathion", "MCPA", "MCPP", "Merphos", "Mevinphos", "Mirex", 
            "Nitrogen-S", "OxyChlordane", "Parathion-methyl", "PCB", "Permethrin", 
            "Perthane", "pH", "Phorate", "Phosphorus-S", "Prallethrin", "Prometon", 
            "Prometryn", "Ronnel", "Sb", "Simazine", "Tetrachlorovinphos", 
            "Tl", "TOC-S", "Tokuthion", "Total chlordane", "Toxaphene", "Trans-Permethrin", 
            "Trichloronate")
nutrs <- c('Ammonia', 'Nitrate, Nitrite', 'Total Kjeldahl Nitrogen', 'Orthophosphate', 'Total Phosphorus', 'Phosphorus-S', 'Nitrogen-S')

powdat <- sddat %>% 
  filter(Parameter %in% c(metals, organs, nutrs))

# data to eval, scns is not created all with crossing to minimize number of combos with no data
scns <- powdat %>% 
  select(StationCode, Parameter, location) %>% 
  unique %>% 
  rename(
    sta = StationCode, 
    par = Parameter, 
    loc = location
  ) %>% 
  crossing(
    .,
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
  loc <- scns[i, ][['loc']]
  chg <- scns[i, ][['chg']]
  eff <- scns[i, ][['eff']]
  
  topow <- powdat %>% 
    filter(StationCode %in% sta) %>% 
    filter(Parameter %in% par) %>% 
    filter(location %in% loc) %>% 
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
sdpows <- scns %>% 
  mutate(
    pow = unlist(res)
  )

save(sdpows, file = here::here('data', 'sdpows.RData'), compress = 'xz')

# harbors and estuaries power analysis for threshold values ---------------

data(sddat)

# constituents
metals <- c("Ag", "As", "Cd", "Cr", "Cu", "Fe", "Hg", "Ni", "Pb", "Se",
            "Zn")
organs <- c("2,4'-D", "2,4-DB", "2,4,5 TP-Silvex", "2,4,5,-T", "Aldrin", 
            "Allethrin", "Alpha-BHC", "Atrazine", "Azinphos methyl (Guthion)", 
            "Be", "Beta-BHC", "Bifenthrin", "Bolstar", "Chlordane", "Chlorpyrifos", 
            "Cis-Permethrin", "Coumaphos", "Cyfluthrin", "Cypermethrin", 
            "Dalapon", "DDT", "Delta-BHC", "Deltamethrin", "Demeton-o", "Demeton-s", 
            "Diazinon", "Dicamba", "Dichlorprop", "Dichlorvos", "Dieldrin", 
            "Dimethoate", "Dinoseb", "Disulfoton", "Endosulfan I", "Endosulfan II", 
            "Endosulfan Sulfate", "Endrin", "Endrin Aldehyde", "Endrin Ketone", 
            "Ethoprop", "Ethyl Parathion", "Fensulfothion", "Fenthion", "Gamma-BHC", 
            "GLYP", "Heptachlor Epoxide", "HPAH", "L-Cyhalothrin", "LPAH", 
            "Malathion", "MCPA", "MCPP", "Merphos", "Mevinphos", "Mirex", 
            "Nitrogen-S", "OxyChlordane", "Parathion-methyl", "PCB", "Permethrin", 
            "Perthane", "pH", "Phorate", "Phosphorus-S", "Prallethrin", "Prometon", 
            "Prometryn", "Ronnel", "Sb", "Simazine", "Tetrachlorovinphos", 
            "Tl", "TOC-S", "Tokuthion", "Total chlordane", "Toxaphene", "Trans-Permethrin", 
            "Trichloronate")
nutrs <- c('Ammonia', 'Nitrate, Nitrite', 'Total Kjeldahl Nitrogen', 'Orthophosphate', 'Total Phosphorus', 'Phosphorus-S', 'Nitrogen-S')

# data to eval
scns <- sddat %>% 
  filter(Parameter %in% c(metals, organs, nutrs)) %>% 
  mutate(
    Year = year(Date), 
    Season = yday(Date),
    dectime = decimal_date(Date)
  ) %>% 
  group_by(StationCode, Parameter, location) %>% 
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
sdthrs <- scns %>% 
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

save(sdthrs, file = here::here('data', 'sdthrs.RData'), compress = 'xz')

# harbors and estuaries optimal effort by station, constituent ------------

source('R/funcs.R')

data(sdpows)

sdopteff <- sdpows %>% 
  crossing(., powin = seq(0.1, 0.9, by = 0.1)) %>% 
  group_by(par, sta, loc, powin) %>% 
  nest %>% 
  mutate(
    opt = purrr::pmap(list(data, powin), function(data, powin) getopt(datin = data, pow = powin))
  ) %>% 
  dplyr::select(-data) %>% 
  unnest(opt) %>% 
  dplyr::select(-opt) %>% 
  na.omit %>% 
  ungroup

save(sdopteff, file = here::here('data', 'sdopteff.RData'), compress = 'xz')

