
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
ncomp <- nrow(r$cdf)
npart <- nrow(r$pdf)


colVekToStringColor <- function(clr) {
  clr <- pmax(0,pmin(clr,1))
  iclr <- round(255 * clr)
  hclr <- sprintf("#%2.2x%2.2x%2.2x",iclr[[1]],iclr[[2]],iclr[[3]])
  return(hclr)
}

rgbToStringColor <- function(rvek,gvek,bvek) { 
  m <- matrix(c(rvek,gvek,bvek),nrow(pdf),3)# matrix with row as r,g,b
  l <- lapply(1:dim(m)[1],function(x) m[x,]) # now unwrap into a list of rgb's
  rgb <- sapply(l,colVekToStringColor)
}

pdf$amb <- rgbToStringColor(pdf$amb.r,pdf$amb.g,pdf$amb.b)
pdf$dif <- rgbToStringColor(pdf$dif.r,pdf$dif.g,pdf$dif.b)
pdf$spc <- rgbToStringColor(pdf$spc.r,pdf$spc.g,pdf$spc.b)
pdf$ems <- rgbToStringColor(pdf$ems.r,pdf$ems.g,pdf$ems.b)

#cdf$rot <- matrix(c(cdf$rot11,cdf$rot12,cdf$13,
                      #cdf$rot21,cdf$rot22,cdf$23,
                      #cdf$rot31,cdf$rot32,cdf$33),3,3)
#cdf$trn <- 

for (cidx in 1:ncomp) {
  id <- cdf$id[cidx]
  cname <- cdf$compname[cidx]
  pname <- cdf$partname[cidx]
  pidx <- which(pdf$partname == pname)
  print(as.character(pname))
  print(pidx)

  print(as.character(cdf$compname[cidx]))
  pt1df <- ptdf[ptdf$id == id,]
  pt1df$id <- NULL
  mpt <- t(as.matrix(pt1df))
  vi1df <- vidf[vidf$id == id,]
  vi1df$id <- NULL
  mvi <- t(as.matrix(vi1df))
  mesh <- tmesh3d(mpt,mvi)
  mesh <
  print(sprintf("%s  pts:%d vidx:%d",cname,length(mpt),length(mvi)))
  shade3d(mesh,color = pdf$amb[pidx])
}
