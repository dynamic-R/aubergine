## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width=9, fig.height=6, out.width=600, out.height=400, fig.align="center")
library("deSolve")

## ---- eval=FALSE--------------------------------------------------------------
#  afun <- approxfun(data)

## ---- eval=FALSE--------------------------------------------------------------
#  tvalue <- afun(t)

## -----------------------------------------------------------------------------
times <- seq(0, 100, by = 0.1)
signal <- data.frame(times = times, import = rep(0, length(times)))
signal$import <- ifelse((trunc(signal$times) %% 2 == 0), 0, 1)
signal[8:12,]

## -----------------------------------------------------------------------------
input <- approxfun(signal, rule = 2)
input(seq(from = 0.98, to = 1.01, by = 0.005))

## -----------------------------------------------------------------------------
SPCmod <- function(t, x, parms) {
  with(as.list(c(parms, x)), {
    import <- input(t)   # <---- here
    dS <- import - b * S * P + g * C
    dP <- c * S * P - d * C * P
    dC <- e * P * C - f * C
    res <- c(dS, dP, dC)
    list(res, signal = import)
  })
}
parms <- c(b = 0.1, c = 0.1, d = 0.1, e = 0.1, f = 0.1, g = 0)
xstart <- c(S = 1, P = 1, C = 1)

## -----------------------------------------------------------------------------
out <- ode(y = xstart, times = times, func = SPCmod, parms)
plot(out)

## -----------------------------------------------------------------------------
pharmaco <- function(t, blood, p) {
  dblood <- - b * blood
  list(dblood)
}
b <- 0.6
yini <- c(blood = 0)

## -----------------------------------------------------------------------------
injectevents <- data.frame(var = "blood",
                          time = 0:20,
                         value = 40,
                        method = "add")
head(injectevents)

## -----------------------------------------------------------------------------
times <- seq(from = 0, to = 10, by = 1/24)
outDrug <- ode(func = pharmaco, times = times, y = yini,
  parms = NULL, method = "impAdams",
  events = list(data = injectevents))

## -----------------------------------------------------------------------------
plot(outDrug)

## -----------------------------------------------------------------------------
library(deSolve)
ball <- function(t, y, parms) {
  dy1 <- y[2]
  dy2 <- -9.8
  list(c(dy1, dy2))
}
yini <- c(height = 0, velocity = 10)

## -----------------------------------------------------------------------------
rootfunc <- function(t, y, parms) return (y[1])

## -----------------------------------------------------------------------------
eventfunc <- function(t, y, parms) {
  y[1] <- 0
  y[2] <- -0.9*y[2]
  return(y)
}

## -----------------------------------------------------------------------------
times <- seq(from = 0, to = 20, by = 0.01)
out <- ode(times = times, y = yini, func = ball,
parms = NULL, rootfun = rootfunc,
events = list(func = eventfunc, root = TRUE))

## -----------------------------------------------------------------------------
attributes(out)$troot

## -----------------------------------------------------------------------------
plot(out, select = "height")

## ---- eval=FALSE--------------------------------------------------------------
#  for (i in seq(1, 2001, 10)) {
#    plot(out, which = "height", type = "l", lwd = 1,
#         main = "", xlab = "Time", ylab = "Height"
#    )
#    points(t(out[i,1:2]), pch = 21, lwd = 1, col = 1, cex = 2,
#           bg = rainbow(30, v = 0.6)[20-abs(out[i,3])+1])
#    Sys.sleep(0.01)
#  }

## ---- eval=FALSE--------------------------------------------------------------
#  ## =============================================================================
#  ## Logistic growth with harvesting
#  ## =============================================================================
#  require(deSolve)
#  
#  derivs <- function(t, y, parms)
#    list(r * y * (1-y/K))
#  
#  r <- 1
#  K <- 10
#  yini <- c(y = 2)
#  times <- seq(from = 0, to = 20, by = 0.1)
#  
#  ## =============================================================================
#  # First run: unharvested
#  ## =============================================================================
#  out1 <- ode(y = yini, times = times, func = derivs, parms = NULL)
#  
#  ## =============================================================================
#  # Second run: harvest at preset times
#  ## =============================================================================
#  
#  ## ****  Fill in this part:  ******
#  
#  harvest <- data.frame(
#                      var =   , # what is the name of the variable,
#                      time =  , # at which times do the events occur,
#                      value = , # what is the value
#                      method = )# what is the method used ("multiply", "add", ...)
#  
#  out2 <- ode(y = yini, times = times, func = derivs, parms = NULL,
#              events = list(data = harvest))
#  
#  ## =============================================================================
#  # Third run: harvest when critical density is readhed
#  ## =============================================================================
#  
#  rootfunc  <- function(t, y, p)
#  #  **** Fill in this part:   a root is when y = 0.8*K ****
#    return( )
#  
#  eventfunc <- function(t, y, p)
#  # **** Fill in this part:  variable y is reduced with 50%  ****
#    return( )
#  
#  out3 <- ode(y = yini, times = times, func = derivs, parms = NULL,
#              rootfun = rootfunc, events = list(func = eventfunc, root = TRUE))
#  
#  ## =============================================================================
#  # Plot different scenarios
#  ## =============================================================================
#  
#  plot(out1, out2, out3, lwd = 2, col = "black")
#  legend ("bottomright", lwd = 2, lty = 1:3,
#      legend = c("unharvested", "2-day harvest", "harvest at 80% of K"))

