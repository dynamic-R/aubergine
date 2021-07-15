## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width=9, fig.height=6, out.width=600, out.height=400, fig.align="center")
library("deSolve")
library("scatterplot3d")

## ----lorenz, fig.width=9, fig.height=6, out.width=600, out.height=400, fig.align="center"----
library("deSolve")
library("scatterplot3d")

Lorenz <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dX <- a * X + Y * Z
    dY <- b * (Y - Z)
    dZ <- -X * Y + c * Y - Z
    list(c(dX, dY, dZ))
  })
}

parameters <- c(a = -8/3, b = -10, c =  28)
state <- c(X = 1, Y = 1, Z = 1)
times <- seq(0, 100, by = 0.01)

out <- ode(y = state, times = times, func = Lorenz, parms = parameters)

plot(out)
scatterplot3d(out[,-1], type="l")

## -----------------------------------------------------------------------------
system("R CMD SHLIB lorenzc.c")

## -----------------------------------------------------------------------------
library(deSolve)
library(scatterplot3d)
dyn.load("lorenzc.dll")

out <- ode(state, times, func = "derivs", parms = parameters,
  dllname = "lorenzc", initfunc = "initmod")

dyn.unload("lorenzc.dll")

plot(out)
scatterplot3d(out[,-1], type="l")

## -----------------------------------------------------------------------------
system("R CMD SHLIB lorenzf.f")

## -----------------------------------------------------------------------------
dyn.load("lorenzf.dll")

out <- ode(state, times, func = "derivs", parms = parameters,
  dllname = "lorenzf", initfunc = "initmod")

dyn.unload("lorenzf.dll")

plot(out)
scatterplot3d(out[,-1], type="l")

## ----eval=FALSE---------------------------------------------------------------
#  library(deSolve)
#  library(scatterplot3d)
#  
#  ## put the pure R model here
#  
#  ## compile the C code
#  system("R CMD SHLIB lorenzc.c")
#  
#  ## compile the fortran code
#  system(..............)
#  
#  ## load the DLLs (note .so on Linux)
#  dyn.load("lorenzc.dll")
#  dyn.load(............)
#  
#  ## run the R model 10 (or 100) times
#  system.time(
#    for(i in 1:10) out   <- ode(state, times, Lorenz, parms = parameters)
#  )
#  
#  ## run the C model 10 (or 100) times
#  ..............
#  
#  ## run the Fortran model 10 (or 100) times
#  ..............
#  
#  ## don't forget to unload the DLLs (.so on Linux)
#  dyn.unload("lorenzc.dll")
#  dyn.unload("lorenzf.dll")

