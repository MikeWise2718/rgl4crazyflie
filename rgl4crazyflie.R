library(rgl)
library(stringr)
library(xml2)
library(hash)
#
#  Mike Wise - Nov/Dec 2016
#
#  Program in R to plot CAD model of CrazyFlie mini-drone
#
#  It performs the following septs
#   1. read in STL files from local copy of github repository Crazyflie-CAD
#   2. create optimized triangle meshes out of it (optimized in reducing the number of points)
#   3. orienting the data so it has the right x,y,z orientation
#   4. ploting in in rgl
#   5. writing out the data in a small set of csv files
#  


appendListAsSubEl <- function(vlist,v) {
  # Append a list to a list as a sub-element 
  # i.e. do not merge the lists (thus losing structure)
  # and retain it attributes 
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

readVertsFromStl <- function(stldir, stlfname,vertPartList) {
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
  vertList <- list()

  # extract the first line which is the name of the part
  partname <- lines[[1]]
  partname <- gsub("solid ","",partname)
  partname <- gsub("_Default_sldprt","",partname)

  lines <- lines[2:length(lines)]

  # trim out stuff we do not need
  lines <- lines[!str_detect(lines,"outer loop")]
  lines <- lines[!str_detect(lines,"endloop")]
  lines <- lines[!str_detect(lines,"endfacet")]
  lines <- lines[!str_detect(lines, "endsolid")]
  print(sprintf("Stripped out %d irrelevant lines", nlines0-length(lines)))

  # now loop over what is left and accumulate the facet vector
  facetvek <- list()
  for (line in lines) {
    if (str_detect(line, "facet")) {
      if (length(facetvek) == 4) {
        vertList <- appendListAsSubEl(vertList,facetvek)
      }
      facetvek <- list()
      facetvek <- appendListAsSubEl(facetvek,extractVert(line))
    } else if (str_detect(line, "vertex")) {
      facetvek <- appendListAsSubEl(facetvek,extractVert(line))
    } else {
      print(sprintf("opps unknown linetype:%s",line))
    }
  }
  vertList <- appendListAsSubEl(vertList,facetvek) # add the remaining one tp vertlist

  vertPartList[[partname]] <- vertList
  elap <- as.numeric((Sys.time() - starttime)[1], units = "secs")
  print(sprintf("Extracted %d vertices in %.1f secs for part '%s'", length(vertList),elap,partname))
  return(vertPartList)
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

addTri <- function(mList,v1,v2,v3,norm,hashemup=T) {
  if (hashemup) {
    mList <- addVertHashed(mList,v1,norm)
    mList <- addVertHashed(mList,v2,norm)
    mList <- addVertHashed(mList,v3,norm)
  } else {
    mList <- addVert(mList,v1,norm)
    mList <- addVert(mList,v2,norm)
    mList <- addVert(mList,v3,norm)
  }
  return(mList)
}

normalize <- function(n) {
  vlen <- sqrt(n[[1]]^2 + n[[2]]^2 + n[[3]]^2)
  if (vlen>0) {
    n <- n/vlen
  }
  return(n)
}

plotPartAsMesh <- function(compname,partname,vertPtList,trn = c(0,0,0),rot = NULL,
                           color = "silver",alpha = 1,shiny = 50,donorms = F,hashemup=T) {
  vhashtab <<- hash()
  mList <- list()
  mList$idxList <- list()
  mList$pntList <- list()
  mList$nrmList <- list()
  normval <- NULL
  for (v in vertPtList) {
    if (length(v) == 4) {
      if (donorms) normval <- v[[1]]
      mList <- addTri(mList,v[[2]],v[[3]],v[[4]],normval,hashemup=hashemup) 
    }
  }
  vidx <- unlist(mList$idxList)
  vpnt <- unlist(mList$pntList)
  nv <- length(vertPtList)
  ni <- length(vidx)
  np <- length(vpnt)

  # Not sure I can make donorms work right
  # giving up for now - 2016.12.1
  #
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

  nvsv <- 3*nv - np
  print(sprintf("   tmesh3d - nv:%d np:%d ni:%d maxi:%d mini:%d - vsaved:%d",nv,np,ni,max(vidx),min(vidx),nvsv))
  if (donorms) {
    part <- tmesh3d(vpnt,vidx,homogeneous=F, normals=vnrm)
  } else {
    part <- tmesh3d(vpnt,vidx,homogeneous=F)
  }
  part <- translate3d(rotate3d(part,matrix = rot),trn[[1]],trn[[2]],trn[[3]])

  # Center and orient the drone - determined these from the data
  # Drone was upside down and facing the wrong way
  # making the center to be the center of the pcb board
  #
  pcbcen <- -0.5*c(54.09200, 71.18820, 75.60720)
  part <- translate3d(part,pcbcen[[1]],pcbcen[[2]],pcbcen[[3]])
  part <- rotate3d(part,pi,0,1,0 ) # rotate 180 around the y-axis
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

    print(sprintf("   cog:%.5f %.5f %.5f %.5f thickness: %.5f %.5f %.5f %.5f",cx,cy,cz,cw, tx,ty,tz,tw))
  }

  shade3d(part,color = color,alpha = alpha,shiny = shiny)
}

plotPart <- function(compname,partname,vertPtList,trn = c(0,0,0),
                        rot = NULL,color = "silver",alpha = 1,shiny = 50) {
# This was just a dev routine using triangles3d
# it is a lot faster since we don't have to hash the vertices for optimization
# but it has a lot more points (120k instead of around 22k)
  lx <- list()
  ly <- list()
  lz <- list()

  for (v in vertPtList) {
    if (length(v) == 4) {
      lx <- c(lx,v[[2]][1],v[[3]][1],v[[4]][1])
      ly <- c(ly,v[[2]][2],v[[3]][2],v[[4]][2])
      lz <- c(lz,v[[2]][3],v[[3]][3],v[[4]][3])
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
  vx <- vx + trn[[1]]
  vy <- vy + trn[[2]]
  vz <- vz + trn[[3]]

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

plotWholeThing <- function(partAttList,partVertList,compList) {
  for (cp in compList) {

    compname <- cp$compname
    partname <- gsub("\\-\\d$","",compname)
    print(sprintf("Comp:%s part:%s",compname,partname))
    rot <- cp$rot
    trn <- cp$trn

    #vertPtList <- findVertPtList(partVertList,partname)
    vertPtList <- partVertList[[partname]]

    prta <- partAttList[[partname]]
    #if (is.null(prta)) { 
        #prta <- findPartInPartAttList(partAttList,partname)
    #}
    #print(prta)
    iclr <- round(255 * prta$ambient[1:3])
    clr <- sprintf("#%2.2x%2.2x%2.2x",iclr[[1]],iclr[[2]],iclr[[3]])
    alf <- prta$ambient[4]
    #print(prta$ambient)

    #plotPart(compname,partname,vertPtList,trn,rot,clr,alf)
    plotPartAsMesh(compname,partname,vertPtList,trn,rot,clr,alf,hashemup=T)
  }
  addAxes(len = 50)
  axes3d()
}

readCompositionFromXml <- function(stldir,xfname) {
  compList <- list()
  xmlfile <- sprintf("%s/%s",stldir,xfname)
  doc <- read_xml(xmlfile)
  itree <- xml_find_first(doc, "//*[local-name()='InstanceTree']")
  its <- xml_find_all(doc, "//*[local-name()='Instance']")
  for (it in its) {
    #print(as.character(it))
    compname <- xml_attr(it, "name")
    needpart <- gsub("\\-\\d$","",compname)
    cp <- list()
    cp$compname <- compname
    cp$partname <- needpart

    print(sprintf("Comp:%s need:%s",compname,needpart))
    tform <- xml_find_first(it, ".//*[local-name()='Transform']")
    if (length(tform) > 0) {
      nrot <- xml_find_first(it, ".//*[local-name()='Rotation']")
      rot <- matrix(as.numeric(str_split(xml_text(nrot), "\\s")[[1]]),3,3)
      cp$rot <- rot
      ntrn <- xml_find_first(it,".//*[local-name()='Translation']")
      # no idea where this factor of 1000 comes from (mm -> meters?)
      # some STL brain damage no doubt
      trn <- 1000.0*as.numeric(str_split(xml_text(ntrn),"\\s")[[1]])
      cp$trn <- trn
      compList[[compname]] <- cp
    }
  }
  return(compList)
}

readMaterialsFromXml <- function(stldir,xfname) {
  partAttList <- list()
  xmlfile <- sprintf("%s/%s",stldir,xfname)
  doc <- read_xml(xmlfile)

  its <- xml_find_all(doc,"//*[local-name()='Part']")
  for (it in its) {
    #print(as.character(it))
    partname <- xml_attr(it,"name")
    prt <- list()
    prt$partname <- partname
    if (!is.null(prt)) {
      print(sprintf("Part:%s",partname))
      nod <- xml_find_first(it,".//*[local-name()='Ambient']")
      prt$ambient <- as.numeric(c(xml_attr(nod,"r"),xml_attr(nod,"g"),xml_attr(nod,"b"),xml_attr(nod,"a")))
      nod <- xml_find_first(it,".//*[local-name()='Diffuse']")
      prt$diffuse <- as.numeric(c(xml_attr(nod,"r"),xml_attr(nod,"g"),xml_attr(nod,"b"),xml_attr(nod,"a")))
      nod <- xml_find_first(it,".//*[local-name()='Specular']")
      prt$specular <- as.numeric(c(xml_attr(nod,"r"),xml_attr(nod,"g"),xml_attr(nod,"b"),xml_attr(nod,"a")))
      nod <- xml_find_first(it,".//*[local-name()='Emissive']")
      prt$emissive <- as.numeric(c(xml_attr(nod,"r"),xml_attr(nod,"g"),xml_attr(nod,"b"),xml_attr(nod,"a")))
      print(sprintf("   amb - %.3f",prt$ambient))
      print(sprintf("   dif - %.3f",prt$diffuse))
      print(sprintf("   spc - %.3f",prt$specular))
      print(sprintf("   emi - %.3f",prt$emissive))
      partAttList[[partname]] <- prt
    }
  }
  return(partAttList)
}

dumpCompList <- function(compList) {
  for (cp in compList) {
    print(sprintf("%s - %s",cp$compname,cp$partname))
    print(cp$rot)
    print(cp$trn)
  }
}

# Start of actual program
# =======================
starttime <- Sys.time()

stldir <- "../Crazyflie-CAD/STL"

# Get the STL files with the vertices and read them in
stlfiles <- list.files(stldir,"\\.STL$")
partVertList <- list()
for (fname in stlfiles) {
  partVertList <- readVertsFromStl(stldir,fname,partVertList)
}

# Now read the composistion file which has the part instances, 
#  their transformations which position them, and their material properties
# 
compList <- readCompositionFromXml(stldir,"Crazyflie_assembly.xml")
partAttList <- readMaterialsFromXml(stldir,"Crazyflie_assembly.xml")
#dumpCompList(compList)

plotWholeThing(partAttList,partVertList,compList)

elap <- as.numeric((Sys.time() - starttime)[1], units = "secs")
print(sprintf("Run took %.1f secs for %d verts - verts optimized away:%d", elap,nvtot,nvsaved))