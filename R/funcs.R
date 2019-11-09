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
  
  # model to estimate variance components
  modin <- gam(log(Result) ~ Year + s(Season, bs = 'cc'), data = powdat)

  # total variation is the sum of annual, seasonal, and residual variation
  resdvar <- resid(modin) %>% var
  seasvar <- gam.vcomp(modin, rescale = F)[[1]]
  yearvar <- (summary(modin)$se[['Year']] * sqrt(ntot)) ^ 2

  # estimate annual linear trend given actual signal
  # chg is desired change from starting value
  # strt is starting value, arbitrary
  # a is rate of change per step
  # ntot is total obs to estimate
  # tot is vector of values to estimate
  # N is the result
  No <- 2.63
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
    Month = month(basedts)
  )
  
  # seasonal component from model
  seascmp <- predict(modin, type = 'terms', exclude = 'Year', newdata = basedts) %>% 
    as.numeric
  
  # simdat
  out <- basedts %>% 
    mutate(
      annscmp = N,
      seascmp = seascmp
    ) %>% 
    crossing(
      sims = 1:sims
    ) %>% 
    mutate(
      simresd = rnorm(simeff * sims, 0, resdvar),
      simseas = rnorm(simeff * sims, 0, seasvar),
      simyear = rnorm(simeff * sims, 0, yearvar),
      simrand = annscmp + seascmp + simresd + simseas + simyear
    ) %>% 
    arrange(sims, Date)
  
  return(out)
  
}

# get power estimates for seasonal kendall using output form simvals function
powfun <- function(simdat, alpha = 0.05){
  
  powest <- simdat %>% 
    group_by(sims) %>% 
    summarise(
      pval = kendallSeasonalTrendTest(exp(simrand) ~ Month + Year)$p.value[2]
    )
  
  pow <- sum(powest$pval < alpha) / nrow(powest)
  
  return(pow)
  
}
