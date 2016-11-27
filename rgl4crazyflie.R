library(rgl)
library(stringr)

stldir <- "../Crazyflie-CAD/STL"

stlfiles <- list.files(stldir,"\\.STL$")

addvert <- function(vlist, v) {
  if (!is.null(v)) {
    vlist[[length(vlist) + 1]] <- v
  }
  return(vlist)
}

extractvert <- function(line) {
  rv <- c(0,0,0)
  sar <- str_split(line,"[\\s]")[[1]] # only one element in line
  n <- length(sar)
  if (n >= 3) {
    rv <- c(as.numeric(sar[[n-2]]),as.numeric(sar[[n-1]]),as.numeric(sar[[n]]))
  }
  return(rv)
}

loadStl <- function(stldir, stlfname) {
  starttime <- Sys.time()
  if (!is.null(stldir) & stldir!="") {
    stlfname <- sprintf("%s/%s", stldir, stlfname)
  }
  lines <- readLines(stlfname, warn = F)
  nlines0 <- length(lines)
  if (nlines0 < 6) {
    print(sprintf("File too short:%d", length(nlines0)))
    return(NULL)
  }
  print(sprintf("Read %d lines from %s",nlines0,stlfname))
  verts <- list()

  # extract the first line which is the name of the part
  partname <- lines[[1]]
  attr(verts, "partname") <- partname
  lines <- lines[2:length(lines)]

  # trim out stuff we do not need
  lines <- lines[!str_detect(lines,"outer loop")]
  lines <- lines[!str_detect(lines,"endloop")]
  lines <- lines[!str_detect(lines,"endfacet")]
  lines <- lines[!str_detect(lines, "endsolid")]
  print(sprintf("Stripped out %d irrelevant lines", nlines0-length(lines)))

  # now loop over what is left and 
  v <- list()
  for (line in lines) {
    if (str_detect(line, "facet")) {
      verts <- addvert(verts, v)
      v <- list()
      v[[1]] <- extractvert(line)
      nxp <- 2
    } else if (str_detect(line, "vertex")) {
      v[[nxp]] <- extractvert(line)
      nxp <- nxp + 1
    } else {
      print(sprintf("opps unknown linetype:%s",line))
    }
  }
  verts <- addvert(verts, v)
  elap <- as.numeric((Sys.time() - starttime)[1], units = "secs")
  print(sprintf("Extracted %d vertices in %.1f secs for part '%s'", length(verts),elap,partname))
  return(verts)
}



plotVertsO <- function(verts,off=c(0,0,0)) {
  for (v in verts) {
    if (length(v) >= 4) {
      x <- c(v[[2]][1] + off[1], v[[3]][1] + off[1], v[[4]][1] + off[1])
      y <- c(v[[2]][2] + off[2], v[[3]][2] + off[2], v[[4]][2] + off[2])
      z <- c(v[[2]][3] + off[3], v[[3]][3] + off[3], v[[4]][3] + off[3])
      #print(sprintf("tri-x: %6.2f", x))
      #print(sprintf("tri-y: %6.2f", y))
      #print(sprintf("tri-z: %6.2f", z))
      rgl.triangles(x,y,z)
      #rgl.triangles(v[[2]], v[[3]], v[[4]])
      #triangles3d(v[[2]], v[[3]], v[[4]])
      #print(sprintf("tri-1: %6.2f", v[[2]]))
      #print(sprintf("tri-2: %6.2f", v[[3]]))
      #print(sprintf("tri-3: %6.2f", v[[4]]))
    }
  }
}
plotVerts <- function(verts, off = c(0,0,0), color="silver",alpha=1,shiny=shiny) {
  x <- list()
  y <- list()
  z <- list()
  for (v in verts) {
    if (length(v) >= 4) {
      x <- append(x, v[[2]][1] + off[1])
      x <- append(x, v[[3]][1] + off[1])
      x <- append(x, v[[4]][1] + off[1])
      y <- append(y, v[[2]][2] + off[2])
      y <- append(y, v[[3]][2] + off[2])
      y <- append(y, v[[4]][2] + off[2])
      z <- append(z, v[[2]][3] + off[3])
      z <- append(z, v[[3]][3] + off[3])
      z <- append(z, v[[4]][3] + off[3])
    }
  }
  triangles3d(unlist(x), unlist(y), unlist(z),color=color,alpha=alpha,shiny=shiny)
}

addaxes <- function(len = 1) {
  u <- c(0,1)*len
  v <- c(0,0)
  w <- c(0, 0)
  lines3d(u, v, w, color = c("red"))
  text3d(u,v,w,c("","X"),color=c("red"))
  lines3d(w,u,v,color=c("green"))
  text3d(w,u,v,c("","Y"),color=c("green"))
  lines3d(v,w,u,color=c("blue"))
  text3d(v,w,u,c("","Z"),color=c("blue"))
}


# Load everything
verts <- sapply(stlfiles, function(x) loadStl(stldir, x))


circleplot <- function(verts, radius = 80) {
  colors <- c("white", "gray", "gray", "green", "yellow", "darkgray", "black", "black")
  alpha <- c(    0.4,     1,       1,       1,        1,       1,      1,        1)
  shine <- c(    50,     50,      50,      50,       50,      50,     50,       50)
  n <- length(verts)
  for (i in 1:n) {
    rad <- radius
    ang <- (i - 1) * 2 * pi / n
    cosang <- cos(ang)
    sinang <- sin(ang)
    off <- c(rad * cosang, rad * sinang, 0)
    color <- 
    plotVerts(verts[[i]], off,colors[[i]],alpha[[i]],shine[[i]])
  }
  addaxes(len = rad/2)
}

circleplot(verts,50)