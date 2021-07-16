## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center")
library("ReacTran")
library("future")
library("gifski")

## ----river-npz----------------------------------------------------------------
library(ReacTran)

riverNPZ <- function(time, y, parms) {
  with(as.list(parms), {
    N <- y[       1  :   nbox ]
    P <- y[(  nbox+1):(2*nbox)]
    Z <- y[(2*nbox+1):(3*nbox)]

    p_growth  <- r_pgrow * N / (km_n + N) * P
    z_grazing <- r_zgraz * P / (km_p + P) * Z
    z_loss    <- r_zloss * Z

    n_transport <- tran.1D(C=N, C.up=N.up, v=v, dx=Grid)
    p_transport <- tran.1D(C=P, C.up=P.up, v=v, dx=Grid)
    z_transport <- tran.1D(C=Z, C.up=Z.up, v=v, dx=Grid)

    n_import <- n_transport$flux.up   # in [mmol/m2/d]
    n_export <- n_transport$flux.down

    dN_dt <- c_pn * (-p_growth + (1-asseff) * z_grazing + z_loss) + n_transport$dC
    dP_dt <-         p_growth -               z_grazing           + p_transport$dC
    dZ_dt <-                  +     asseff *  z_grazing - z_loss  + z_transport$dC

    list(c(dN_dt, dP_dt, dZ_dt),
         n_import=n_import, n_export=n_export,
         meanN=mean(N), meanP=mean(P), meanZ=mean(Z))
  })
}
parms <- c(
  r_pgrow    = 0.5,   # phytoplankton growth parameter (1/d)
  r_zgraz    = 0.4,   # zooplankton ingestion parameter (1/d)
  r_zloss    = 0.01,  # zooplankton loss parameter (1/d)

  km_n       = 0.5,   # Monod constant of phyto growth on nutrient (mmol/m3)
  km_p       = 100,   # Monod constant of zoo grazing on phyto (mmol/m3)
  c_pn       = 1/106, # stoichiometric conversion from phosporus P to phyto C (P:C)
  asseff     = 0.3,   # assimilation efficiency of zoo (-)

  v          = 10000, # flow velocity (m/d)
  N.up       = 500,   # nutrient (PO4) in inflow (mmolP/m3)
  P.up       = 10,    # algae in inflow (mmolC/m3)
  Z.up       = 1      # zooplankton in inflow (mmolC/m3)
)

# river morphology
Length <- 500e3  # length of the river (m)
nbox   <- 500    # number of 1D grid cells
Grid   <- setup.grid.1D(L = Length, N = nbox)

# Zoo and Phyto as C and Phosphorus P (mol/m3)
N_init  <- rep(500, times=nbox) # mmolP/m3
P_init  <- rep(100, times=nbox) # mmolC/m3
Z_init  <- rep(10,  times=nbox) # mmolC/m3
y_init  <- c(N_init, P_init, Z_init)

## ----river-npz-future, fig.height=8, fig.width=7------------------------------
library("future")

parms1 <- parms2 <- parms
system.time({
  plan(strategy="multisession")
  #plan(strategy="sequential")
  
  ## three staions for the equilibrium (=stationary state)
  parms["v"] <- 1000  # (m/d)
  std0 %<-% steady.1D(y=y_init, time=0, func=riverNPZ, parms=parms,
                       nspec=3, names=c("N","P","Z"), method="runsteady")
  
  parms1["v"] <- 5000  # m/d
  std1 %<-% steady.1D(y=y_init, time=0, func=riverNPZ, parms=parms1,
                       nspec=3, names=c("N","P","Z"), method="runsteady")
  
  parms2["v"] <- 10000 # m/d
  std2 %<-% steady.1D(y=y_init, time=0, func=riverNPZ, parms=parms2,
                       nspec=3, names=c("N","P","Z"), method="runsteady")
  
  plot(std0, std1, std2, mfrow=c(3,1), grid=Grid$x.mid/1000, lty=1, lwd=2, xlab="km")
  
  ## and a dynamic simulation (= nonstationary)
  time_seq <- seq(from = 0, to = 200, length.out=201)
  dynamic  %<-% ode.1D(y=y_init, times=time_seq, func=riverNPZ, parms=parms, nspec=3)
})

## ----river-npz-image, fig.height=8, fig.width=7-------------------------------
image(dynamic, xlab = "time, days", ylab = "Distance, m", grid= Grid$x.mid,
      main =c("PO4","Phyto","Zoo"), add.contour=TRUE, mfrow = c(3, 1)) 

## ----river-npz-ani, fig.height=8, fig.width=7---------------------------------
plot_poly <- function(data, time) {
  poly <- function(x, y, pcol, ylab, ylim) {
    plot(x, y, type="n", las=1, xlab="", ylab="", ylim=ylim)
    polygon(x, y, col=pcol, border=pcol, lty="blank")
    mtext(side=1, line=1.5, text="km")
    mtext(side=2, line=3.5, text=ylab, las=3)

  }
  y <- data[time, -1]
  N <- y[1:nbox]
  P <- y[(nbox+1):(2*nbox)]
  Z  <- y[(2*nbox+1):(3*nbox)]
  x   <- c(1, 1:nbox, nbox)
  #cat("time ", time, "\n")

  poly(x, c(0, N, 0), ylim=c(0,  1000), pcol="#a6cee3", ylab="P-PO4 (mmol/m3)")
  poly(x, c(0, P, 0), ylim=c(0, 70000), pcol="#b2df8a", ylab="Phyto (mmol/m3)")
  poly(x, c(0, Z, 0), ylim=c(0, 50000), pcol="#ee7f00", ylab="Zoo (mmol/m3)")
}

for (tt in time_seq)  {
  png(paste0("river", 1000 + tt, ".png"), width=800, height=600, pointsize = 18)
  par(mfrow=c(3,1))
  par(mar=c(3,5,.5,0), las=1, cex.axis=1.4, cex.lab=1.4, cex.main=2)
  plot_poly(dynamic, tt)
  dev.off()
}

library("gifski")
gifski(dir(pattern="^river.*png$"), gif_file = "river.gif", width=800, height=600, delay=1/25)

## ----close-workers------------------------------------------------------------
plan(strategy="sequential") # return to the default plan and close the workers

