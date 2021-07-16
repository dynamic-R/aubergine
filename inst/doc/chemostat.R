## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align="center")
#library(learnr)
library(deSolve)
library(rootSolve)

## ---- chemostat---------------------------------------------------------------
library("deSolve")
library("rootSolve")

chemostat <- function(time, y, parms) {
  with(as.list(c(y, parms)), {
    p_growth <- r_pgrow * N / (k_n + N) * P

    dN_dt   <- - c_pn * p_growth + d * (N0 - N)
    dP_dt   <-          p_growth + d * (0 - P)
    list(c(dN_dt, dP_dt), p_growth = p_growth)
   })
}
parms <- c(
  r_pgrow = 0.5,   # phytoplankton growth parameter (1/d)
  k_n     = 1.0,   # half saturation constant, P (mmol/m3)
  c_pn    = 1/106, # stoichiometric conversion from phosphorus P to phyto C (P:C ratio)
  d       = 0.1,   # dilution rate 1/d
  N0      = 5      # phosphorus in inflow (mmol/m3)
)

## ---- single, fig.height=3, fig.width=7---------------------------------------
times <- seq(from=0, to=40, length.out=100)  # simulation time (d)
y     <- c(N = 5, P = 1)                     # Nutrient (mmolP/m3) and Phytoplankton (mmolC/m3)
out   <- ode(y, times, chemostat, parms)
plot(out, mfrow=c(1, 3))

## ---- chemostat-ex1, eval=FALSE, fig.height=3, fig.width=7--------------------
#  library("deSolve")
#  library("rootSolve")
#  
#  chemostat <- function(time, y, parms) {
#    with(as.list(c(y, parms)), {
#      p_growth <- r_pgrow * N / (k_n + N) * P
#  
#      dN_dt   <- - c_pn * p_growth + d * (N0 - N)
#      dP_dt   <-          p_growth + d * (0 - P)
#      list(c(dN_dt, dP_dt), p_growth = p_growth)
#     })
#  }
#  parms <- c(
#    r_pgrow = 0.5,   # phytoplankton growth parameter (1/d)
#    k_n     = 1.0,   # half saturation constant, P (mmol/m3)
#    c_pn    = 1/106, # stoichiometric conversion from phosphorus P to phyto C (P:C ratio)
#    d       = 0.1,   # dilution rate 1/d
#    N0      = 5      # phosphorus in inflow (mmol/m3)
#  )
#  times <- seq(from=0, to=40, length.out=100)  # simulation time (d)
#  
#  y  <- c(N = .. , P = .. ) ##### Give values to N and P #####
#  
#  out <- ode(y, times, chemostat, parms)
#  plot(out, mfrow=c(1, 3))

## ----chemostat-ex1-question, eval=FALSE, echo=FALSE---------------------------
#  
#  ans <- "The equilibrium is independent on initial states, except if
#    initial phytoplankton P is zero (nothing can grow without an
#    inoculum). However, the initial state impacts the time it takes to
#    reach equilibrium"
#  
#  question("How do the initial values influence the final state?",
#    answer("The final state depends on the initial states"),
#    answer("The final state is independent on initial states, except `P=0`", correct = TRUE),
#    answer("Initial state influence time until equilibrium", correct = TRUE),
#    incorrect = ans,
#    correct=ans,
#    allow_retry = TRUE
#  )

## ---- chemostat-ex2, eval=TRUE, fig.height=3, fig.width=7---------------------
library("deSolve")
library("rootSolve")

chemostat <- function(time, y, parms) {
  with(as.list(c(y, parms)), {
    p_growth <- r_pgrow * N / (k_n + N) * P

    dN_dt   <- - c_pn * p_growth + d * (N0 - N)
    dP_dt   <-          p_growth + d * (0 - P)
    list(c(dN_dt, dP_dt), p_growth = p_growth)
   })
}
parms <- c(
  r_pgrow = 0.5,   # phytoplankton growth parameter (1/d)
  k_n     = 1.0,   # half saturation constant, P (mmol/m3)
  c_pn    = 1/106, # stoichiometric conversion from phosphorus P to phyto C (P:C ratio)
  d       = 0.1,   # dilution rate 1/d
  N0      = 5      # phosphorus in inflow (mmol/m3)
)
test_parms <- parms
test_parms["d"] <- 0.1
times <- seq(from=0, to=40, length.out=100)  # simulation time (d)
y     <- c(N = 5, P = 1)                     # Nutrient (mmolP/m3) and Phytoplankton (mmolC/m3)
out <- ode(y, times, chemostat, test_parms)
plot(out, mfrow=c(1, 3))

## ----eval=FALSE, echo=FALSE---------------------------------------------------
#  
#  ans <- "The chemostat can be run from `d=0` (batch) up to `d <
#    r_pgrowth` (the maximum growth rate). If dilution exceeds growth,
#    phytoplankton dies out."
#  
#  question("What limits the range of the dilution rate so that algae survive in the chemostat?",
#    answer("The dilution rate cannot be 0."),
#    answer("A dilution rate of zero is equivalent to a batch.", correct = TRUE),
#    answer("If the dilution rate exceeds max growth rate, phytoplankton dies out.", correct = TRUE),
#    incorrect = ans,
#    correct=ans,
#    allow_retry = TRUE
#  )

## ---- multiple, fig.height=4, fig.width=7-------------------------------------
times <- seq(from=0, to=40, length.out=100)  # simulation time (d)
y     <- c(N=5, P=1)
p1 <- p2 <- p3 <- parms
p1["d"] <- 0; p2["d"] <- 0.3; p3["d"] <- 0.5
out  <- ode(y, times, chemostat, parms)
out1 <- ode(y, times, chemostat, p1)
out2 <- ode(y, times, chemostat, p2)
out3 <- ode(y, times, chemostat, p3)

plot(out, out1, out2, out3, which=c("N", "P"))

## ----analytical, fig.height=3, fig.width=7------------------------------------
d <- seq(0, 0.6, length.out = 100)
r <- 0.5;  kp <- 0.5; Y = 106; N0 = 5
 
N <- d * kp / (r - d)
N <- ifelse(d > (r * N0)/(kp + N0), N0, N)
P <- Y * (N0 - N)
 
par(mfrow=c(1, 3))
plot(d, N,     type="l")
plot(d, P,     type="l")
plot(d, d * P, type="l")

## ---- steadystate, eval=FALSE, fig.height=3, fig.width=7----------------------
#  state <- data.frame(
#    d = seq(0.01, 0.5, length.out = 100),
#    N = 0,
#    P = 0
#  )
#  
#  ## example how to calculate a single equilibrium
#  
#  parms["d"] <- state$d[5]
#  times <- c(0, Inf)
#  out <- runsteady(y, times, chemostat, parms)
#  out$y
#  
#  ## place a loop here to fill the data frame
#  
#  # .............
#  
#  ## and then outcomment the plot
#  #par(mfrow = c(1, 3))
#  #plot(N     ~ d, data = state, type = "l")
#  #plot(P     ~ d, data = state, type = "l")
#  #plot(N * P ~ d, data = state, type = "l")

## ---- steadystate-hint, eval=FALSE, fig.height=3, fig.width=7-----------------
#  for (i in 1:nrow(state)) {
#    parms["d"] <- state$d[i]
#    times <- c(0, Inf)
#    out <- runsteady(y, times, chemostat, parms)
#    state[i, 2:3] <- out$y
#  }
#  
#  par(mfrow = c(1, 3))
#  plot(N     ~ d, data = state, type = "l")
#  plot(P     ~ d, data = state, type = "l")
#  plot(d * P ~ d, data = state, type = "l")

