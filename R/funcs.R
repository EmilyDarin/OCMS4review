# function for formatting p-values in tables
p_ast <- function(x){
  
  sig_cats <- c('**', '*', 'ns')
  sig_vals <- c(-Inf, 0.005, 0.05, Inf)
  
  out <- cut(x, breaks = sig_vals, labels = sig_cats, right = FALSE)
  out <- as.character(out)
  
  return(out)
  
}

# simulate time series given annual, seasonal variance component of input data
# chg is desired annual change trend
# eff is sample effort as proportion from input data
# sims is numer of time series to simulate
simvals <- function(powdat, chg = 0.5, eff = 1, sims = 100){
  
  # total obs, simulation effort
  ntot <- nrow(powdat)
  simeff <- round(ntot * eff, 0)
  
  # add year, season
  powdat <- powdat %>%
    mutate(
      Year = year(Date), 
      Season = yday(Date),
      dectime = decimal_date(Date)
    )
    
  # model to estimate variance components
  # modin <- gam(log(Result) ~ Year + s(Season, bs = 'cc'), data = powdat)
  modin <- lm(log(1 + Result) ~ dectime, data = powdat)

  # total variation is the sum of annual, seasonal, and residual variation
  resdvar <- resid(modin) %>% var
  # seasvar <- gam.vcomp(modin, rescale = F)[[1]]
  # yearvar <- (summary(modin)$se[['Year']] * sqrt(ntot)) ^ 2

  # estimate annual linear trend given actual signal
  # chg is desired change from starting value
  # strt is starting value, arbitrary
  # a is rate of change per step
  # ntot is total obs to estimate
  # tot is vector of values to estimate
  # N is the result
  No <- median(log(1 + powdat$Result), na.rm = T)
  a <- -1 * chg * No / simeff
  tot <- 1:simeff
  N <- No + tot * a - a
  
  # base simulation to add to total annual change, scaled by desired effort
  # seasonal component from gam plus rnorm terms for variance stochasticity
  dtrng <- range(powdat$Date)
  basedts <- seq.Date(dtrng[1], dtrng[2], length.out = simeff)
  basedts <- data.frame(
    Date = basedts, 
    Year = year(basedts), 
    Season = yday(basedts), 
    Month = month(basedts),
    dectime = decimal_date(basedts)
  )
  
  # seasonal component from model
  # seascmp <- predict(modin, type = 'terms', exclude = 'Year', newdata = basedts) %>% 
  #   as.numeric
  trndcmp <- predict(modin, newdata = basedts)
  
  # simdat
  out <- basedts %>% 
    mutate(
      # trndcmp = trndcmp#,
      annscmp = N#,
      # seascmp = seascmp
    ) %>% 
    crossing(
      sims = 1:sims
    ) %>% 
    mutate(
      simresd = rnorm(simeff * sims, 0, resdvar),
      # simseas = rnorm(simeff * sims, 0, seasvar),
      # simyear = rnorm(simeff * sims, 0, yearvar),
      # simrand = annscmp + seascmp + simresd + simseas + simyear
      simrand = annscmp + simresd
    ) %>% 
    arrange(sims, Date)
  
  return(out)
  
}

# simulate time series to a given date for annual, seasonal variance component of input data
# chg is desired annual change trend
# yrs is number of years out from final date
# origminyr starting year for observed data (entire tissue dataset)
# origmaxyr ending year for observed data (entire tissue dataset)
# sims is numer of time series to simulate
simextvals <- function(powdat, chg = 0.5, yrs = 5, origminyr = 2012, origmaxyr = 2018, sims = 100){

  # only run if enough data
  stopifnot(nrow(powdat) > 2)
  
  # add year, season
  powdat <- powdat %>%
    mutate(
      Year = year(Date), 
      Season = yday(Date),
      dectime = decimal_date(Date)
    )

  # total obs
  ntot <- nrow(powdat)
  
  # get current sample effort (events per year)
  cureff <- ntot / (1 + origmaxyr - origminyr)
    
  # base simulation to add to total annual change, scaled by desired effort
  basedts <- year(powdat$Date) %>% unique
  endyr <- origmaxyr + yrs
  
  # total number of samples for the desired period
  simeff <- round(cureff * (1 + diff(range(c(endyr,basedts)))))
  
  # model to estimate variance components
  # modin <- gam(log(Result) ~ Year + s(Season, bs = 'cc'), data = powdat)
  modin <- lm(log(1 + Result) ~ dectime, data = powdat)
  
  # total variation is the sum of annual, seasonal, and residual variation
  resdvar <- resid(modin) %>% var
  # seasvar <- gam.vcomp(modin, rescale = F)[[1]]
  # yearvar <- (summary(modin)$se[['Year']] * sqrt(ntot)) ^ 2
  
  # estimate annual linear trend given actual signal
  # chg is desired change from starting value
  # strt is starting value, arbitrary
  # a is rate of change per step
  # ntot is total obs to estimate
  # tot is vector of values to estimate
  # N is the result
  No <- median(log(1 + powdat$Result), na.rm = T)
  a <- -1 * chg * No / (simeff - 1)
  tot <- 1:simeff
  N <- No + tot * a - a
  
  # dates to simulate
  basedts <- mdy(paste0('05-01-', c(origminyr, endyr)))
  basedts <- seq.Date(basedts[1], basedts[2], length.out = simeff)
  basedts <- data.frame(
    Date = basedts, 
    Year = year(basedts), 
    Season = yday(basedts), 
    Month = month(basedts),
    dectime = decimal_date(basedts)
  )
  
  # seasonal component from model
  # seascmp <- predict(modin, type = 'terms', exclude = 'Year', newdata = basedts) %>% 
  #   as.numeric
  trndcmp <- predict(modin, newdata = basedts)
  
  # simdat
  out <- basedts %>% 
    mutate(
      # trndcmp = trndcmp#,
      annscmp = N#,
      # seascmp = seascmp
    ) %>% 
    crossing(
      sims = 1:sims
    ) %>% 
    mutate(
      simresd = rnorm(simeff * sims, 0, resdvar),
      # simseas = rnorm(simeff * sims, 0, seasvar),
      # simyear = rnorm(simeff * sims, 0, yearvar),
      # simrand = annscmp + seascmp + simresd + simseas + simyear
      simrand = annscmp + simresd
    ) %>% 
    arrange(sims, Date)
  
  return(out)
  
}

# simulate time series same as input with random residual component
# eff is sample effort as proportion from input data
# sims is numer of time series to simulate
thrvals <- function(powdat, eff = 1, sims = 100){

  # total obs, simulation effort
  ntot <- nrow(powdat)
  simeff <- round(ntot * eff, 0)
  
  # add year, season
  powdat <- powdat %>%
    mutate(
      Year = year(Date), 
      Season = yday(Date)
    )
  
  # model to estimate variance components
  modin <- gam(log(1 + Result) ~ Year + s(Season, bs = 'cc'), data = powdat)
  
  # total variation is the sum of annual, seasonal, and residual variation
  resdvar <- resid(modin) %>% var
  
  # base simulation to add to total annual change, scaled by desired effort
  # seasonal component from gam plus rnorm terms for variance stochasticity
  dtrng <- range(powdat$Date)
  basedts <- seq.Date(dtrng[1], dtrng[2], length.out = simeff)
  basedts <- data.frame(
    Date = basedts, 
    Year = year(basedts), 
    Season = yday(basedts), 
    Month = month(basedts),
    dectime = decimal_date(basedts)
  )

  # seasonal component from model, then add long-term average
  ltave <- mean(log(1 + powdat$Result), na.rm = T)
  predcmp <- predict(modin, newdata = basedts, terms = 's(Season)', type = 'terms') %>% 
    as.numeric %>% 
    `+`(ltave)

  # simdat
  out <- basedts %>% 
    mutate(
      predcmp = predcmp
    ) %>% 
    crossing(
      sims = 1:sims
    ) %>% 
    mutate(
      simresd = rnorm(simeff * sims, 0, resdvar),
      simrand = predcmp + simresd
    ) %>% 
    arrange(sims, Date)
  
  return(out)
  
}

# get power estimates for seasonal kendall using output form simvals function
powfun <- function(simdat, alpha = 0.05){
  
  powest <- simdat %>% 
    group_by(sims) %>% 
    nest %>% 
    mutate(
      pval = purrr::map(data, function(x){
        
        mod <- lm(simrand ~ dectime, data = x) %>% summary %>% .$fstatistic
        pf(mod[1], mod[2], mod[3], lower.tail = F)
        
      }),
    ) %>% 
    select(-data) %>% 
    unnest(pval)
  
  pow <- sum(powest$pval < alpha) / nrow(powest)
  
  return(pow)
  
}

# optimal sample effort
# datin is power results for one station, one parameter
# pow is desired level of power
getopt <- function(datin, pow = 0.5){
  
  p <- ggplot(datin) +
    geom_contour(aes(x = eff, y = chg, z = pow), breaks = pow) 
  
  dat <- ggplot_build(p)$data[[1]]
  
  if(nrow(dat) == 0)
    return(NA)
  
  # check if multiple pieces
  if(length(unique(dat$piece)) > 1)
    return(NA)
  
  dat <- dat %>% 
    arrange(x)
  
  # check if not monotonic
  chk <- diff(dat$y)
  if(any(sign(chk) == 1))
    return(NA)
  
  slopey <- diff(dat$y) / diff(dat$x)
  slopex <- diff(dat$x) / diff(dat$y)
  loc <- which(slopey > slopex)[1]
  
  out <- dat[loc + 1, ] %>% 
    dplyr::select(eff = x, chg = y)
  
  return(out)
  
}