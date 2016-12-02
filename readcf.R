
library(rgl)
library(stringr)
library(xml2)
library(hash)
#
#  Mike Wise - Nov/Dec 2016
#
#  Program in R to read in and plot CAD model of CrazyFlie mini-drone
#
#
readMesh <- function(fnameroot = "crazyflie") {

  # Components
  fname <- sprintf("%s-components.csv",fnameroot)
  cdf <- read.csv(fname)

  # Parts
  fname <- sprintf("%s-parts.csv",fnameroot)
  pdf <- read.csv(fname)

  # Points
  fname <- sprintf("%s-points.csv",fnameroot)
  ptdf <- read.csv(fname)

  # VertsIdx
  fname <- sprintf("%s-vertidx.csv",fnameroot)
  vidf <- read.csv(fname)

  rv <- list()
  rv$cdf <- cdf
  rv$pdf <- pdf
  rv$ptdf <- ptdf
  rv$vidf <- vidf
  return(rv)
}

r <- readMesh("crazyflie")

open3d()

cdf <- r$cdf
pdf <- r$pdf
ptdf <- r$ptdf
vidf <- r$vidf
ptdf$id <- 1
vidf$id <- 1

for (i in 1:nrow(r$cdf)) {
  id <- cdf$id[[i]]
  cname <- cdf$comp[i]
  print(as.character(cdf$comp[i]))
  pt1df <- ptdf[ptdf$id == id,]
  pt1df$id <- NULL
  mpt <- t(as.matrix(pt1df))
  vi1df <- vidf[vidf$id == id,]
  vi1df$id <- NULL
  mvi <- t(as.matrix(vi1df))
  part <- tmesh3d(mpt,mvi)
  print(sprintf("%s  pts:%d vidx:%d",cname,length(mpt),length(mvi)))
  shade3d(part)
}
