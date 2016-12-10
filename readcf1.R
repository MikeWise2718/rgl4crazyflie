library(rgl)
library(hash)
#
#  Mike Wise - Nov/Dec 2016
#
#  Program in R to read in and plot CAD model of CrazyFlie mini-drone
#
#

colVekToStringColor <- function(clr) {
  clr <- pmax(0,pmin(clr,1))
  iclr <- round(255 * clr)
  hclr <- sprintf("#%2.2x%2.2x%2.2x",iclr[[1]],iclr[[2]],iclr[[3]])
  return(hclr)
}

rgbToStringColor <- function(rvek,gvek,bvek) {
  nr <- length(rvek)
  m <- matrix(c(rvek,gvek,bvek),nr,3) # matrix with row as r,g,b
  l <- lapply(1:dim(m)[1],function(x) m[x,]) # now unwrap into a list of rgb's
  rgb <- sapply(l,colVekToStringColor)
}

readMesh <- function(fnameroot = "crazyflie") {

  # Components
  fname <- sprintf("%s-components.csv",fnameroot)
  cdf <- read.csv(fname)
  cdf <- cdf[cdf$id > 0,]
  nc <- nrow(cdf)
  cdf$sca <- lapply(1:nc,function(i) c(cdf$sca.x[i],cdf$sca.y[i],cdf$sca.z[i]))
  cdf$trn <- lapply(1:nc,function(i) c(cdf$trn.x[i],cdf$trn.y[i],cdf$trn.z[i]))
  cdf$rot <- lapply(1:nc,function(i) matrix(c( cdf$rot.11[i],cdf$rot.12[i],cdf$rot.13[i],
                                               cdf$rot.21[i],cdf$rot.22[i],cdf$rot.23[i],
                                               cdf$rot.31[i],cdf$rot.32[i],cdf$rot.33[i]),3,3))
  # Parts
  fname <- sprintf("%s-parts.csv",fnameroot)
  pdf <- read.csv(fname)
  pdf$amb <- rgbToStringColor(pdf$amb.r,pdf$amb.g,pdf$amb.b)
  pdf$dif <- rgbToStringColor(pdf$dif.r,pdf$dif.g,pdf$dif.b)
  pdf$spc <- rgbToStringColor(pdf$spc.r,pdf$spc.g,pdf$spc.b)
  pdf$ems <- rgbToStringColor(pdf$ems.r,pdf$ems.g,pdf$ems.b)

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


addAxes <- function(len = 1,s = NULL,r = NULL,t = NULL,tit = "",charexp = 1) {

  getHeadToTailPoints <- function(x,y,z) {
    m <- matrix(c(x,y,z),2,3)
    return(m)
  }
  u <- c(0,len)
  v <- c(0,0)
  w <- c(0,0)

  xax <- getHeadToTailPoints(u,v,w)
  yax <- getHeadToTailPoints(w,u,v)
  zax <- getHeadToTailPoints(v,w,u)

  if (!is.null(s)) {
    xax[1,] <- xax[1,] %*% s
    xax[2,] <- xax[2,] %*% s
    yax[1,] <- yax[1,] %*% s
    yax[2,] <- yax[2,] %*% s
    zax[1,] <- zax[1,] %*% s
    zax[2,] <- zax[2,] %*% s
  }
  if (!is.null(r)) {
    xax <- xax %*% r
    yax <- yax %*% r
    zax <- zax %*% r
  }
  if (!is.null(t)) {
    xax[1,] <- xax[1,] + t
    xax[2,] <- xax[2,] + t
    yax[1,] <- yax[1,] + t
    yax[2,] <- yax[2,] + t
    zax[1,] <- zax[1,] + t
    zax[2,] <- zax[2,] + t
  }

  lines3d( xax,color = c("red"))
  lines3d( yax,color = c("green"))
  lines3d( zax,color = c("blue"))

  text3d(xax[2,],text="X",color = c("red"),cex = charexp)
  text3d(yax[2,],text="Y",color = c("green"),cex = charexp)
  text3d(zax[2,],text="Z",color = c("blue"),cex = charexp)

}


plotMesh <- function(obj) {
  cdf <- obj$cdf
  pdf <- obj$pdf
  ptdf <- obj$ptdf
  vidf <- obj$vidf
  ncomp <- nrow(obj$cdf)
  npart <- nrow(obj$pdf)

  for (cidx in 1:ncomp) {
    # setup
    cid <- cdf$id[cidx]
    cname <- cdf$compname[cidx]
    pname <- cdf$partname[cidx]
    pidx <- which(pdf$partname == pname)
    partid <- pdf$partid[pidx]

    # get the points for this component
    pt1df <- ptdf[ptdf$partid == partid,]
    pt1df$partid <- NULL
    mpt <- t(as.matrix(pt1df))

    # get the indexs for this component
    vi1df <- vidf[vidf$partid == partid,]
    vi1df$partid <- NULL
    mvi <- t(as.matrix(vi1df))

    # make the mesh, then rotate and transform if necssary
    mesh <- tmesh3d(mpt,mvi)

    sca <- cdf$sca[[cidx]]
    rot <- cdf$rot[[cidx]]
    trn <- cdf$trn[[cidx]]

    mesh <- scale3d(mesh,x = sca[1],y = sca[2],z = sca[3])
    mesh <- rotate3d(mesh,matrix = rot)
    mesh <- translate3d(mesh,trn[1],trn[2],trn[3])

    # render it
    print(sprintf("%25s cid:%4d  - cidx:%2d pidx:%2d pts:%5d vidx:%5d",
               cname,cid,cidx,pidx,length(mpt),length(mvi)))

    clr <- pdf$amb[pidx]
    # mounts and motor part id numbers 
    #   1 1600 500
    #   2 1100 300
    #   3 100 600
    #   4 200 400
    shade3d(mesh,color = clr,alpha = pdf$amb.a[pidx])
    addAxes(10,t = trn,r = rot) # show the local coordinate system
  }
}



open3d()
robv <- readMesh("crazyflie")
plotMesh(robv)
addAxes(50)