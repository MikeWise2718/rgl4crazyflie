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
  cdf <- cdf[cdf$id>0,]

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

addAxes <- function(len = 1,t=c(0,0,0),r=NULL,tit="") {
  u <- c(0,1) * len
  v <- c(0,0)
  w <- c(0,0)
  lines3d(u+t[1],v+t[2],w+t[3],          color = c("red"))
  text3d( u+t[1],v+t[2],w+t[3],c("","X"),color = c("red"),cex=1)
  lines3d(w+t[1],u+t[2],v+t[3],          color = c("green"))
  text3d( w+t[1],u+t[2],v+t[3],c("","Y"),color = c("green"))
  lines3d(v+t[1],w+t[2],u+t[3],          color = c("blue"))
  text3d( v+t[1],w+t[2],u+t[3],c("","Z"),color = c("blue"))

}


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

getRot <- function(cdf,cidx) {
  rot <- matrix(c(cdf$rot.11[cidx],cdf$rot.12[cidx],cdf$rot.13[cidx],
                  cdf$rot.21[cidx],cdf$rot.22[cidx],cdf$rot.23[cidx],
                  cdf$rot.31[cidx],cdf$rot.32[cidx],cdf$rot.33[cidx]),3,3)
  if (sum(abs(rot)) == 0) {
    return(matrix(c(1,0,0,0,1,0,0,0,1),3,3))
  } else {
    return(rot)
  }
}
getTrn <- function(cdf,cidx) {
  trn <- c(cdf$trn.x[cidx],cdf$trn.y[cidx],cdf$trn.z[cidx])
  return(trn)
}

ctran <- c(1,1,3,3,3,3,7,7,9,9,1,12,13,14,15,1,17)

for (cidx in 1:ncomp) {
  # setup
  cid <- cdf$id[cidx]
  cname <- cdf$compname[cidx]
  pname <- cdf$partname[cidx]
  pidx <- which(pdf$partname == pname)

  # now find the first component that has the same partname as this component
  # we are experimenting with changing out for parts

  fcidx <- which(cdf$partname == pname)[[1]]
  fcid <- cdf$id[fcidx]

  # get the points for this component
  pt1df <- ptdf[ptdf$id == fcid,]
  pt1df$id <- NULL
  mpt <- t(as.matrix(pt1df))

  # get the indexs for this component
  vi1df <- vidf[vidf$id == fcid,]
  vi1df$id <- NULL
  mvi <- t(as.matrix(vi1df))

  # make the mesh, then rotate and transform if necssary
  mesh <- tmesh3d(mpt,mvi)
  rot <- getRot(cdf,cidx)
  trn <- getTrn(cdf,cidx)
  if (fcid == 1600) {
    omesh <- mesh
    omesh <- translate3d(omesh,trn[1],trn[2],trn[3])
    mesh <- rotate3d(mesh,matrix=rot)
  }
  mesh <- translate3d(mesh,trn[1],trn[2],trn[3])

  # render it
  print(sprintf("%25s cid:%4d  fcid:%4d - cidx:%2d fcidx:%2d pidx:%2d pts:%5d vidx:%5d",
               cname,cid,fcid,cidx,fcidx,pidx,length(mpt),length(mvi)))

  clr <- pdf$amb[pidx]
  shade3d(mesh,color = clr ,alpha = pdf$amb.a[pidx])
  addAxes(10,t = trn,r = rot) # show the local coordinate system

}

addAxes(50)