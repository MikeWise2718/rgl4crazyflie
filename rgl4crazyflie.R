library(rgl)
library(stringr)
library(xml2)
library(hash)

starttime <- Sys.time()

stldir <- "../Crazyflie-CAD/STL"

stlfiles <- list.files(stldir,"\\.STL$")

appendWithAtts <- function(vlist,v) {
  # Append to a list and retain it attributes
  if (!is.null(v)) {
    vlist[[length(vlist) + 1]] <- v
    # Note, if we use the faster method below, we loose the attributes of vlist!
    # Bad, bad, bad...
    # vlist <- c(vlist,v)
  }
  return(vlist)
}

extractVert <- function(line) {
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
  partlist <- list()

  # extract the first line which is the name of the part
  partname <- lines[[1]]
  attr(partlist, "partname") <- partname
  lines <- lines[2:length(lines)]

  # trim out stuff we do not need
  lines <- lines[!str_detect(lines,"outer loop")]
  lines <- lines[!str_detect(lines,"endloop")]
  lines <- lines[!str_detect(lines,"endfacet")]
  lines <- lines[!str_detect(lines, "endsolid")]
  print(sprintf("Stripped out %d irrelevant lines", nlines0-length(lines)))

  # now loop over what is left and 
  facet <- list()
  for (line in lines) {
    if (str_detect(line, "facet")) {
      if (length(facet) == 4) {
        partlist <- appendWithAtts(partlist, facet)
      }
      facet <- list()
      facet <- appendWithAtts(facet, extractVert(line))
    } else if (str_detect(line, "vertex")) {
      facet <- appendWithAtts(facet, extractVert(line))
    } else {
      print(sprintf("opps unknown linetype:%s",line))
    }
  }
  partlist <- appendWithAtts(partlist, facet)
  elap <- as.numeric((Sys.time() - starttime)[1], units = "secs")
  print(sprintf("Extracted %d vertices in %.1f secs for part '%s'", length(partlist),elap,partname))
  return(partlist)
}

# hash bookkeeping
vhashtab <- hash()
nvsaved <- 0
nvtot <- 0

addVertHashed <- function(mList,v,n = NULL) {
  ni <- length(mList$idxList)
  np <- length(mList$pntList)
  vkey <- paste0(sprintf("%e",v),collapse=",")
  if (has.key(vkey,vhashtab)) {
    vval <- vhashtab[[vkey]]
    #print(sprintf("np:%d ni:%d vkey:%s nval:%d",np,ni,vkey,nval))
    mList$idxList <- c(mList$idxList,vval$vnum)
    if (!is.null(n)) {
      vval$norm <- vval$norm + n
    }
    nvsaved <<- nvsaved+1
  } else {
    vnum <- (np + 3) / 3
    vval <- list()
    vval$vnum <- vnum
    if (!is.null(n)) vval$norm <- n
    vhashtab[vkey] <- vval
    mList$pntList <- c(mList$pntList,v[1],v[2],v[3])
    mList$idxList <- c(mList$idxList,vnum)
    nvtot <<- nvtot+1
  }
  return(mList)
}

addVert <- function(mList,v,n = NULL) {
  # no hashing
  ni <- length(mList$idxList)
  np <- length(mList$pntList)
  mList$pntList <- c(mList$pntList, v[1],v[2],v[3])
  mList$idxList <- c(mList$idxList, ni+1)
  nvtot <<- nvtot+1
  return(mList)
}

addTri <- function(mList, v1, v2, v3,norm) {
  mList <- addVertHashed(mList,v1,norm)
  mList <- addVertHashed(mList,v2,norm)
  mList <- addVertHashed(mList,v3,norm)
  return(mList)
}

normalize <- function(n) {
  vlen <- sqrt(n[[1]]^2 + n[[2]]^2 + n[[3]]^2)
  if (vlen>0) {
    n <- n/vlen
  }
  return(n)
}

plotPartAsMesh <- function(compname,partname,vertList,trn = c(0,0,0),rot = NULL,
                           color = "silver",alpha = 1,shiny = shiny,donorms = F,savefiles=F) {
  vhashtab <<- hash()
  mList <- list()
  mList$idxList <- list()
  mList$pntList <- list()
  mList$nrmList <- list()
  normval <- NULL
  for (v in vertList) {
    if (length(v) == 4) {
      if (donorms) normval <- v[[1]]
      mList <- addTri(mList,v[[2]],v[[3]],v[[4]],normval) 
    }
  }
  vidx <- unlist(mList$idxList)
  vpnt <- unlist(mList$pntList)
  nv <- length(vertList)
  ni <- length(vidx)
  np <- length(vpnt)

  if (donorms) {
    nn <- np / 3
    for (i in 1:nn) {
      bidx <- (i-1)*3
      v1 <- vpnt[[bidx+1]]
      v2 <- vpnt[[bidx+2]]
      v3 <- vpnt[[bidx+3]]
      v <- c(v1,v2,v3)
      vkey <- paste0(sprintf("%e",v),collapse = ",")
      vval <- vhashtab[[vkey]]
      mList$nrmList <- c(mList$nrmList,normalize(vval$norm))
    }
    vnrm <- unlist(mList$nrmList)
  }


  vsv <- 3*nv - np
  print(sprintf("   tmesh3d - nv:%d np:%d ni:%d maxi:%d mini:%d - vsaved:%d",nv,np,ni,max(vidx),min(vidx),vsv))
  if (donorms) {
    part <- tmesh3d(vpnt,vidx,homogeneous=F, normals=vnrm)
  } else {
    part <- tmesh3d(vpnt,vidx,homogeneous=F)
  }
  part <- translate3d(rotate3d(part,matrix = rot),trn[[1]],trn[[2]],trn[[3]])
  pcbcen <- -0.5*c(54.09200, 71.18820, 75.60720)
  #pcbcen <- -10 * c(3.686,3.686,3.685)
  part <- translate3d(part,pcbcen[[1]],pcbcen[[2]],pcbcen[[3]])
  part <- rotate3d(part,pi,0,1,0 )
  calccog <- T
  if (calccog) {
    vb <- part$vb
    nvb <- length(vb)
    bsq <- (1:(nvb/4) - 1)*4
    xc <- vb[bsq + 1]
    yc <- vb[bsq + 2]
    zc <- vb[bsq + 3]
    wc <- vb[bsq + 4]
    cx <- mean(min(xc) + max(xc))
    cy <- mean(min(yc) + max(yc))
    cz <- mean(min(zc) + max(zc))
    cw <- mean(min(wc) + max(wc))
    tx <- max(xc) - min(xc)
    ty <- max(yc) - min(yc)
    tz <- max(zc) - min(zc)
    tw <- max(wc) - min(wc)
#    cx <- mean(vpnt[bsq+1])
#    cy <- mean(vpnt[bsq+2])
#    cz <- mean(vpnt[bsq+3])
    print(sprintf("   cog:%.5f %.5f %.5f %.5f thickness: %.5f %.5f %.5f %.5f",cx,cy,cz,cw, tx,ty,tz,tw))
  }
  shade3d(part,color = color,alpha = alpha,shiny = shiny)
  if (savefiles) {
  }
}

plotPart <- function(compname,partname,vertList,trn = c(0,0,0),rot = NULL,color = "silver",alpha = 1,shiny = shiny) {
  lx <- list()
  ly <- list()
  lz <- list()
  xi <- 1
  yi <- 2
  zi <- 3
  for (v in vertList) {
    if (length(v) == 4) {
      lx <- c(lx,v[[2]][xi],v[[3]][xi],v[[4]][xi])
      ly <- c(ly,v[[2]][yi],v[[3]][yi],v[[4]][yi])
      lz <- c(lz,v[[2]][zi],v[[3]][zi],v[[4]][zi])
    }
  }
  vx <- unlist(lx)
  vy <- unlist(ly)
  vz <- unlist(lz)

  if (!is.null(rot)) {
    #print("rotating")
    ux <- vx
    uy <- vy
    uz <- vz
    vx <- ux*rot[1,1] + uy*rot[2,1] + uz*rot[3,1]
    vy <- ux*rot[1,2] + uy*rot[2,2] + uz*rot[3,2]
    vz <- ux*rot[1,3] + uy*rot[2,3] + uz*rot[3,3]
  }
  #print("translating")
  vx <- vx + trn[[xi]]
  vy <- vy + trn[[yi]]
  vz <- vz + trn[[zi]]

  #print(sprintf("trn-q: %.3f %.3f %.3f ",trn[[xi]],trn[[yi]],trn[[zi]]))

  nvtot <<- nvtot + length(vx)

  triangles3d(vx,vy,vz,color = color,alpha = alpha,shiny = shiny)
}


addAxes <- function(len = 1) {
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
partList <- sapply(stlfiles, function(x) loadStl(stldir, x))

# Temp and ugly
colors <- c("white", "gray", "gray", "green", "yellow", "blue", "black", "orange")
alpha <- c(0.4, 1, 1, 1, 1, 1, 1, 1)
shine <- c(50, 50, 50, 50, 50, 50, 50, 50)
for (i in 1:8) {
  attr(partList[[i]],"color") <- colors[[i]]
  attr(partList[[i]],"alpha") <- alpha[[i]]
  attr(partList[[i]],"shine") <- shine[[i]]
}

plotPartsInCircle <- function(partList, radius = 80) {
  n <- length(partList)
  for (i in 1:n) {
    prt <- partList[[i]]
    print(attr(prt,"partname"))
    rad <- radius
    ang <- (i-1)*2*pi/n
    cosang <- cos(ang)
    sinang <- sin(ang)
    trn <- c(rad * cosang, rad * sinang, 0)
    clr <- attr(prt,"color")
    alf <- attr(prt, "alpha")
    shn <- attr(prt, "shine")
    plotPart(prt, trn, NULL, clr,alf,shn)
    #plotPartAsMesh(prt, trn, NULL, clr,alf,shn)
  }
  addAxes(len = rad/2)
}

#plotPartsInCircle(partList, 50)

getElement <- function(partlist, need) {
  #print(sprintf("getElement:%s",need))
  for (p in partlist) {
    pelid <- attr(p,"partname")
    #print(sprintf("   detecting:%s", pelid))
    if (str_detect(pelid,need)) {
      #print(sprintf("returning:%s", pelid))
      return(p)
    }
  }
  #print(sprintf("returning NULL"))
  return(NULL)
}

plotWholeThing <- function(partList,doc) {
  itree <- xml_find_first(doc,"//*[local-name()='InstanceTree']")
  its <- xml_find_all(doc,"//*[local-name()='Instance']")
  for (it in its) {
    #print(as.character(it))
    compname <- xml_attr(it,"name")
    needpart <- str_split(compname,"-")[[1]][[1]]
    print(sprintf("Comp:%s need:%s",compname,needpart))
    tform <- xml_find_first(it,".//*[local-name()='Transform']")
    if (length(tform) > 0) {
      nrot <- xml_find_first(it,".//*[local-name()='Rotation']")
      rot <- matrix(as.numeric(str_split(xml_text(nrot),"\\s")[[1]]),3,3)

      ntrn <- xml_find_first(it,".//*[local-name()='Translation']")

      # no idea where this factor of 1000 comes from (mm -> meters?)
      # some STL brain damage no doubt
      trn <- 1000 * as.numeric(str_split(xml_text(ntrn),"\\s")[[1]])

      prt <- getElement(partList,needpart)
      clr <- attr(prt,"color")
      alf <- attr(prt,"alpha")
      shn <- attr(prt,"shine")
      #plotPart(compname,partname,prt, trn, rot, clr, alf, shn)
      plotPartAsMesh(compname,partname,prt,trn,rot,clr,alf,shn)
    }
  }
  addAxes(len = 50)
}


grabComposition <- function(partList,doc) {
#  xmlfile <- sprintf("%s/%s",stldir,"Crazyflie_assembly.xml")
#  doc <- read_xml(xmlfile)

  itree <- xml_find_first(doc, "//*[local-name()='InstanceTree']")
  its <- xml_find_all(doc, "//*[local-name()='Instance']")
  for (it in its) {
    #print(as.character(it))
    compname <- xml_attr(it, "name")
    needpart <- str_split(compname,"-")[[1]][[1]]
    print(sprintf("Comp:%s need:%s",compname,needpart))
    tform <- xml_find_first(it, ".//*[local-name()='Transform']")
    if (length(tform) > 0) {
      nrot <- xml_find_first(it, ".//*[local-name()='Rotation']")
      rot <- matrix(as.numeric(str_split(xml_text(nrot), "\\s")[[1]]),3,3)

      ntrn <- xml_find_first(it,".//*[local-name()='Translation']")

      # no idea where this factor of 1000 comes from (mm -> meters?)
      # some STL brain damage no doubt
      trn <- 1000 * as.numeric(str_split(xml_text(ntrn),"\\s")[[1]])

      prt <- getElement(partList,needpart)
      clr <- attr(prt, "color")
      alf <- attr(prt, "alpha")
      shn <- attr(prt, "shine")
      #plotPart(compname,partname,prt, trn, rot, clr, alf, shn)
      plotPartAsMesh(compname,partname,prt, trn, rot, clr, alf, shn)
    }
  }
  addAxes(len = 50)
}
axes3d()

xmlfile <- sprintf("%s/%s", stldir, "Crazyflie_assembly.xml")
doc <- read_xml(xmlfile)

plotWholeThing(partList,doc)
#d <- read_xml("simple1.xml")
#bstree <- xml_find_all(d, ".//bs:stmt") # Transaction Type
elap <- as.numeric((Sys.time() - starttime)[1], units = "secs")
print(sprintf("Run took %.1f secs for %d verts - saved:%d", elap,nvtot,nvsaved))
