## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width=7, fig.align="center")
library("deSolve")
library("rootSolve")
#library(learnr)

## ----chemostat----------------------------------------------------------------
library("deSolve")

chemostat <- function(time, y, parms) {
  with(as.list(c(y, parms)), {
    p_growth <- r_pgrow * N / (k_n + N) * P

    dN_dt   <- -c_pn * p_growth + d * (N0 - N)
    dP_dt   <-         p_growth + d * (0 - P)
    list(c(dN_dt, dP_dt), p_growth = p_growth)
   })
}
parms <- c(
  r_pgrow = 0.5,   # phytoplankton growth parameter (1/d)
  k_n     = 1.0,   # half saturation constant, P (mmol/m3)
  c_pn    = 1/106, # stoichiometric conversion from phosphorus P to phyto C (P:C ratio)
  d       = 0.1,   # dilution rate (1/d)
  N0      = 5      # Phosphorus in inflow (mmol/m3)
)
times <- seq(from=0, to=40, by=0.1)  # simulation time (d)
y     <- c(N = 5, P = 1)             # Nutrient (mmolP/m3) N and Phytoplankton (mmolC/m3)

out <- ode(y, times, chemostat, parms) # solve model
plot(out, mfrow=c(1, 3))

## ----chemostat-npz, eval=FALSE------------------------------------------------
#  library("deSolve")
#  npz <- function(time, y, parms) {
#    with(as.list(c(y, parms)), {
#  
#      p_growth     <- r_pgrow * N / (k_n + N) * P
#  
#      z_grazing    <- ...
#      z_growth     <- ...
#      n_recycling  <- ...
#  
#      dN_dt   <- c_pn * (-p_growth + n_recycling) + d * (N0 - N)
#      dP_dt   <- p_growth - z_grazing             + d * (0 - P)
#      dZ_dt   <- ...
#  
#      list(c(dN_dt, dP_dt, ...))
#     })
#  }
#  parms <- c(
#    r_pgrow  = 0.5,   # phytoplankton growth parameter (1/d)
#    r_zgraz  = 0.4,   # zooplankton grazing parameter (1/d)
#    r_zloss  = 0.0,   # zooplankton loss parameter (1/d)
#  
#    k_n      = 0.5,   # Monod constant of phyto growth on nutrient (mmol/m3)
#    k_p      = 100,   # Monod constant of zoo growth on phyto (mmol/m3)
#    c_pn     = 1/106, # stoichiometric conversion from phosporus P to phyto C (P:C ratio)
#    c_asseff = 0.3,   # zooplankton assimilation efficiency (-)
#  
#    d       = 0.1,    # dilution rate (1/d)
#    N0      = 5       # P in inflow (mmol/m3)
#  )
#  
#  times <- seq(0, 40, 0.1)        # (d)
#  
#  # Nutrient in Phosphorus P; Phyto and Zoo in C and (mol/m3)
#  y  <- c(N=5, P=1, Z=10)
#  
#  parms["d"] <- 0.1
#  out0 <- ode(y, times, npz, parms)
#  plot(out0, mfrow = c(1, 3))

## ----chemostat-npz-solution---------------------------------------------------
library("deSolve")
npz <- function(time, y, parms) {
  with(as.list(c(y, parms)), {
 
    p_growth     <- r_pgrow * N / (k_n + N) * P
    
    z_grazing    <- r_zgraz  * P / (k_p + P) * Z
    z_growth     <- c_asseff * z_grazing
    n_recycling  <- (1 - c_asseff) * z_grazing

    dN_dt   <- c_pn * (-p_growth + n_recycling) + d * (N0 - N)
    dP_dt   <- p_growth - z_grazing             + d * (0 - P)
    dZ_dt   <- z_growth - r_zloss * Z           + d * (0 - Z)
    
    list(c(dN_dt, dP_dt, dZ_dt))
   })
}
parms <- c(
  r_pgrow  = 0.5,   # phytoplankton growth parameter (1/d)
  r_zgraz  = 0.4,   # zooplankton grazing parameter (1/d)
  r_zloss  = 0.0,   # zooplankton loss parameter (1/d)
  
  k_n      = 0.5,   # Monod constant of phyto growth on nutrient (mmol/m3)
  k_p      = 100,   # Monod constant of zoo growth on phyto (mmol/m3)
  c_pn     = 1/106, # stoichiometric conversion from phosporus P to phyto C (P:C ratio)
  c_asseff = 0.3,   # zooplankton assimilation efficiency (-)

  d       = 0.1,    # dilution rate (1/d)
  N0      = 5       # P in inflow (mmol/m3)
)

times <- seq(0, 40, 0.1)        # (d)

# Nutrient in Phosphorus P; Phyto and Zoo in C and (mol/m3)
y  <- c(N=5, P=1, Z=10)

parms["d"] <- 0.1
out0 <- ode(y, times, npz, parms)
plot(out0, mfrow = c(1, 3))

## ----chemostat-npz-code-------------------------------------------------------
library("deSolve")
npz <- function(time, y, parms) {
  with(as.list(c(y, parms)), {
 
    p_growth     <- r_pgrow * N / (k_n + N) * P
    
    z_grazing    <- r_zgraz  * P / (k_p + P) * Z
    z_growth     <- c_asseff * z_grazing
    n_recycling  <- (1 - c_asseff) * z_grazing

    dN_dt   <- c_pn * (-p_growth + n_recycling) + d * (N0 - N)
    dP_dt   <- p_growth - z_grazing             + d * (0 - P)
    dZ_dt   <- z_growth - r_zloss * Z           + d * (0 - Z)
    
    list(c(dN_dt, dP_dt, dZ_dt))
   })
}
parms <- c(
  r_pgrow  = 0.5,   # phytoplankton growth parameter (1/d)
  r_zgraz  = 0.4,   # zooplankton grazing parameter (1/d)
  r_zloss  = 0.0,   # zooplankton loss parameter (1/d)
  
  k_n      = 0.5,   # Monod constant of phyto growth on nutrient (mmol/m3)
  k_p      = 100,   # Monod constant of zoo growth on phyto (mmol/m3)
  c_pn     = 1/106, # stoichiometric conversion from phosporus P to phyto C (P:C ratio)
  c_asseff = 0.3,   # zooplankton assimilation efficiency (-)

  d       = 0.1,    # dilution rate (1/d)
  N0      = 5       # P in inflow (mmol/m3)
)

times <- seq(0, 40, 0.1)        # (d)

# Nutrient in Phosphorus P; Phyto and Zoo in C and (mol/m3)
y  <- c(N=5, P=1, Z=10)

parms["d"] <- 0.1
out0 <- ode(y, times, npz, parms)
plot(out0, mfrow = c(1, 3))

## ----npz-scenario-exercise, eval=FALSE----------------------------------------
#  library("deSolve")
#  npz <- function(time, y, parms) {
#    with(as.list(c(y, parms)), {
#  
#      p_growth     <- r_pgrow * N / (k_n + N) * P
#  
#      z_grazing    <- r_zgraz  * P / (k_p + P) * Z
#      z_growth     <- c_asseff * z_grazing
#      n_recycling  <- (1 - c_asseff) * z_grazing
#  
#      dN_dt   <- c_pn * (-p_growth + n_recycling) + d * (N0 - N)
#      dP_dt   <- p_growth - z_grazing             + d * (0 - P)
#      dZ_dt   <- z_growth - r_zloss * Z           + d * (0 - Z)
#  
#      list(c(dN_dt, dP_dt, dZ_dt))
#     })
#  }
#  parms <- c(
#    r_pgrow  = 0.5,   # phytoplankton growth parameter (1/d)
#    r_zgraz  = 0.4,   # zooplankton grazing parameter (1/d)
#    r_zloss  = 0.0,   # zooplankton loss parameter (1/d)
#  
#    k_n      = 0.5,   # Monod constant of phyto growth on nutrient (mmol/m3)
#    k_p      = 100,   # Monod constant of zoo growth on phyto (mmol/m3)
#    c_pn     = 1/106, # stoichiometric conversion from phosporus P to phyto C (P:C ratio)
#    c_asseff = 0.3,   # zooplankton assimilation efficiency (-)
#  
#    d       = 0.1,    # dilution rate (1/d)
#    N0      = 5       # P in inflow (mmol/m3)
#  )
#  
#  times <- seq(0, 40, 0.1)        # (d)
#  
#  # Nutrient in Phosphorus P; Phyto and Zoo in C and (mol/m3)
#  y  <- c(N=5, P=1, Z=10)
#  
#  parms["d"] <- 0.1
#  out0 <- ode(y, times, npz, parms)
#  plot(out0, mfrow = c(1, 3))
#  times <- seq(0, 800)
#  parms["d"] <- 0.0
#  out <- ode(y, times, npz, parms)
#  plot(out, mfrow = c(1, 3))

## ----npz-scenario0, fig.height=2, fig.width=7, fig.keep="none"----------------
times <- seq(0, 800)
parms["d"] <- 0.0
out0 <- ode(y, times, npz, parms)
plot(out0, mfrow = c(1, 3))

## ----npz-scenario1, fig.height=2, fig.width=7, fig.keep="none"----------------
parms["d"] <- 0.2
out1 <- ode(y, times, npz, parms)
plot(out1, mfrow=c(1, 3))

## ----npz-scenario2, fig.height=2, fig.width=7, fig.keep="none"----------------
parms["d"] <- 0.07
out2 <- ode(y, times, npz, parms)
plot(out2, mfrow=c(1, 3))

## ----npz-scenario3, fig.height=2, fig.width=7, fig.keep="none"----------------
parms["d"] <- 0.09
out3 <- ode(y, times, npz, parms)
plot(out3, mfrow=c(1, 3))

## ----npz-scenario4, fig.height=2, fig.width=7, fig.keep="none"----------------
parms["d"] <- 0.5
out4 <- ode(y, times, npz, parms)
plot(out4, mfrow=c(1, 3))

## ----npz-scenario5, fig.height=2, fig.width=7, fig.keep="none"----------------
parms["d"] <- 0.081
out5 <- ode(y, times, npz, parms)
plot(out5, mfrow=c(1, 3))

## ----npz-all, fig.height=3, fig.width=7---------------------------------------
plot(out0, out1, out2, out3, out4, out5, mfrow=c(1,3), lty=1)
legend("topright",
  legend=c("EQ, No Algae",
           "Lotka-Volterra",
           "EQ, No Zoo", 
           "EQ, coexistence", 
           "EQ. both extinct", 
           "Damped oscillation"), 
  lty=1, col=1:6, cex=0.5)

