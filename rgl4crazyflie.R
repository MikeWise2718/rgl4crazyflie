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


readCzfVertsFromStl <- function(stldir,stlfname,vertPartList) {

  appendListAsSubEl <- function(vlist,v) {
    # Append a list to a list as a sub-element 
    # i.e. do not merge the lists (thus losing structure)
    # and retain the top list's attributes 
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
      rv <- c(as.numeric(sar[[n - 2]]),as.numeric(sar[[n - 1]]),as.numeric(sar[[n]]))
    }
    return(rv)
  }

  starttime <- Sys.time()
  if (!is.null(stldir) & stldir != "") {
    stlfname <- sprintf("%s/%s",stldir,stlfname)
  }
  lines <- readLines(stlfname,warn = F)
  nlines0 <- length(lines)
  if (nlines0 < 6) {
    print(sprintf("File too short:%d",length(nlines0)))
    return(NULL)
  }
  print(sprintf("Read %d lines from %s",nlines0,stlfname))
  vertPtList <- list()

  # extract the first line which is the name of the part
  partname <- lines[[1]]
  partname <- gsub("solid ","",partname)
  partname <- gsub("_Default_sldprt","",partname)

  lines <- lines[2:length(lines)]

  # trim out stuff we do not need
  lines <- lines[!str_detect(lines,"outer loop")]
  lines <- lines[!str_detect(lines,"endloop")]
  lines <- lines[!str_detect(lines,"endfacet")]
  lines <- lines[!str_detect(lines,"endsolid")]
  print(sprintf("Stripped out %d irrelevant lines",nlines0 - length(lines)))

  # now loop over what is left and accumulate the facet vector
  facetvek <- list()
  for (line in lines) {
    if (str_detect(line,"facet")) {
      if (length(facetvek) == 4) {
        vertPtList <- appendListAsSubEl(vertPtList,facetvek)
      }
      facetvek <- list()
      facetvek <- appendListAsSubEl(facetvek,extractVert(line))
    } else if (str_detect(line,"vertex")) {
      facetvek <- appendListAsSubEl(facetvek,extractVert(line))
    } else {
      print(sprintf("opps unknown linetype:%s",line))
    }
  }
  vertPtList <- appendListAsSubEl(vertPtList,facetvek) # add the remaining one tp vertPtList
  vertTopList <- list()
  vertTopList$vertPtList <- vertPtList
  vertTopList$partname <- partname
  vertPartList[[partname]] <- vertTopList
  elap <- as.numeric((Sys.time() - starttime)[1],units = "secs")
  print(sprintf("Extracted %d vertices in %.1f secs for part '%s'",length(vertPtList),elap,partname))
  return(vertPartList)
}

# hash bookkeeping
vhashtab <- hash()
nvsaved <- 0
nvtot <- 0

addVertHashed <- function(mList,v,n = NULL) {
  ni <- length(mList$idxList)
  np <- length(mList$pntList)
  vkey <- paste0(sprintf("%e",v),collapse = ",")
  if (has.key(vkey,vhashtab)) {
    vval <- vhashtab[[vkey]]
    #print(sprintf("np:%d ni:%d vkey:%s nval:%d",np,ni,vkey,nval))
    mList$idxList <- c(mList$idxList,vval$vnum)
    nvsaved <<- nvsaved + 1
  } else {
    vnum <- (np + 3) / 3
    vval <- list()
    vval$vnum <- vnum
    vhashtab[vkey] <- vval
    mList$pntList <- c(mList$pntList,v[1],v[2],v[3])
    mList$idxList <- c(mList$idxList,vnum)
    nvtot <<- nvtot + 1
  }
  return(mList)
}

addVert <- function(mList,v,n = NULL) {
  # no hashing
  ni <- length(mList$idxList)
  np <- length(mList$pntList)
  mList$pntList <- c(mList$pntList,v[1],v[2],v[3])
  mList$idxList <- c(mList$idxList,ni + 1)
  nvtot <<- nvtot + 1
  return(mList)
}

addTri <- function(mList,v1,v2,v3,hashemup = T) {
  if (hashemup) {
    mList <- addVertHashed(mList,v1)
    mList <- addVertHashed(mList,v2)
    mList <- addVertHashed(mList,v3)
  } else {
    mList <- addVert(mList,v1)
    mList <- addVert(mList,v2)
    mList <- addVert(mList,v3)
  }
  return(mList)
}

normalize <- function(n) {
  vlen <- sqrt(n[[1]] ^ 2 + n[[2]] ^ 2 + n[[3]] ^ 2)
  if (vlen > 0) {
    n <- n / vlen
  }
  return(n)
}


findBBox <- function(ptmat,printword = "test",print = T) {
  minx <- miny <- minz <- +9e99
  maxx <- maxy <- maxz <- -9e99
  for (i in 1:dim(ptmat)[2]) {
    minx <- min(minx,ptmat[[1,i]])
    maxx <- max(maxx,ptmat[[1,i]])
    miny <- min(miny,ptmat[[2,i]])
    maxy <- max(maxy,ptmat[[2,i]])
    minz <- min(minz,ptmat[[3,i]])
    maxz <- max(maxz,ptmat[[3,i]])
  }
  stats <- list()
  minv <- c(minx,miny,minz)
  maxv <- c(maxx,maxy,maxz)
  cenv <- (maxv + minv) / 2
  stats$minv <- minv
  stats$maxv <- maxv
  stats$cenv <- cenv
  print(sprintf("%s bbox x: %.1f to %.1f    y: %.1f to %.1f    z: %.1f to %.1f",
                  printword,minx,maxx,miny,maxy,minz,maxz))
  print(sprintf("%s cenv %.1f %.1f %.1f",printword,cenv[1],cenv[2],cenv[3]))
  return(stats)
}

translatePointsFromMatrix <- function(ptmat,trn) {
  print(sprintf("Translating trn %.1f %.1f %.1f",trn[1],trn[2],trn[3]))
  for (i in 1:dim(ptmat)[2]) {
    ptmat[1,i] <- ptmat[1,i] + trn[1]
    ptmat[2,i] <- ptmat[2,i] + trn[2]
    ptmat[3,i] <- ptmat[3,i] + trn[3]
  }
  return(ptmat)
}


combinePointsIntoMesh <- function(compname = "some_comp",
                                  vertPtList,ix = 1,iy = 2,iz = 3,hashemup = T,
                                  trn = c(0,0,0),center = T,
                                  prerot = matrix(c(1,0,0,0,1,0,0,0,1),3,3),
                                  postrot = matrix(c(1,0,0,0,1,0,0,0,1),3,3)) {
  #  Takes a list of 3D vectors that representing a triangle
  # v1,v2,v3 being the points where the vertices are located
  # the ix,iy,iz allow the x,y and z positions to be exchanged
  # if hashemup is T, then identiacl points will be merged in the resulting
  # vertex list
  # Returns a list of indexs (1 based), and a list of points
  # no check or correction for backface/frontface consistentcy
  # if center=true, then the points are centered on the middle of the
  # bounding box and a translation vector is returned
  #
  vhashtab <<- hash()
  mList <- list()
  mList$idxList <- list()
  mList$pntList <- list()
  mList$nrmList <- list()
  normval <- NULL
  ineed <- max(ix,iy,iz)
  for (v in vertPtList) {
    if (length(v) >= ineed) {
      mList <- addTri(mList,v[[ix]],v[[iy]],v[[iz]],hashemup = hashemup)
    }
  }
  vidx <- unlist(mList$idxList)
  vpnt <- unlist(mList$pntList)

  vpntm <- t(matrix(vpnt,3,length(vpnt) / 3))
  vpntm <- vpntm %*% prerot
  vpntm[,1] <- vpntm[,1] + trn[1]
  vpntm[,2] <- vpntm[,2] + trn[2]
  vpntm[,3] <- vpntm[,3] + trn[3]
  vpntm <- vpntm %*% postrot

  cen <- c(0,0,0)
  if (center) {
    stats <- findBBox(comp$out_vp,compname)
    cen <- stats$cenv
    vpntm[,1] <- vpntm[,1] - cen[1]
    vpntm[,2] <- vpntm[,2] - cen[2]
    vpntm[,3] <- vpntm[,3] - cen[3]
  }

  rv <- list()
  rv$compname <- compname
  rv$vidx <- vidx
  rv$vpnt <- as.numeric(t(vpntm))
  rv$cen <- cen
  return(rv)
}

calcMeshCog <- function(part) {
  vb <- part$vb
  nvb <- length(vb)
  bsq <- (1:(nvb / 4) - 1) * 4
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

  print(sprintf("   cog:%.5f %.5f %.5f %.5f thickness: %.5f %.5f %.5f %.5f",cx,cy,cz,cw,tx,ty,tz,tw))
}

plotPartAsMesh <- function(compname,partname,vertTopList,comp,trn = c(0,0,0),rot = NULL,
                           amb = "silver",dif = NULL,spc = NULL,ems = NULL,alf = 1,shiny = 50,hashemup = T,quiet = F) {

  # we want it to be centerd on the center of the pcb (which id probably a bit under its cog, but close enough..
  pcbcen <- -0.5 * c(54.09200,71.18820,75.60720)
  reorient <- matrix(c(-1,0,0,0,1,0,0,0,-1),3,3)
  ident <- matrix(c(1,0,0,0,1,0,0,0,1),3,3)

  rv <- combinePointsIntoMesh(compname,vertTopList$vertPtList,2,3,4,prerot=rot,trn=(trn+pcbcen),postrot=ident,center=F)

  nv <- length(vertTopList$vertPtList)
  vidx <- rv$vidx
  vpnt <- rv$vpnt
  ni <- length(vidx)
  np <- length(vpnt)
  nvsv <- 3*nv - np

  if (!quiet) {
    print(sprintf("   tmesh3d - nv:%d np:%d ni:%d maxi:%d mini:%d - vsaved:%d",nv,np,ni,max(vidx),min(vidx),nvsv))
  }

  part <- tmesh3d(vpnt,vidx,homogeneous = F)
  #part <- translate3d(rotate3d(part,matrix = rot),trn[[1]],trn[[2]],trn[[3]])

  # Center and orient the drone - determined these from the data
  # Drone was upside down and facing the wrong way
  # making the center to be the center of the pcb board
  #
  #pcbcen <- -0.5*c(54.09200, 71.18820, 75.60720)
  #part <- translate3d(part,pcbcen[[1]],pcbcen[[2]],pcbcen[[3]])
  part <- rotate3d(part,pi,0,1,0) # rotate 180 around the y-axis

  if (!quiet) {
    calcMeshCog(part)
  }

  shade3d(part,color = amb,specular=spc,emissive=ems,alpha = alf,shiny = shiny)

  comp$out_vp <- part$vb
  comp$out_vi <- part$it
  stats <- findBBox(comp$out_vp,compname)
  comp$out_vp <- translatePointsFromMatrix(comp$out_vp, -stats$cenv )
  comp$out_sca <- c(1,1,1)

  # I don't know why this transformation of rot is necessary, but it is...
  # m1 swaps Y&Z and then inverts all the axes
  #
  m1 <- matrix(c(-1,0,0,0,0,-1,0,-1,0),3,3)
  comp$out_rot <- m1 %*% rot
  #comp$out_rot <- matrix(c(1,0,0,0,1,0,0,0,1),3,3)
  #comp$out_rot <- rot

  comp$out_trn <- stats$cenv
  stats <- findBBox(comp$out_vp,compname)
  #comp$out_trn <- c(0,0,0)
  return(comp)
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

colVekToStringColor <- function(clr) {
  clr <- pmax(0,pmin(clr,1))
  iclr <- round(255 * clr)
  hclr <- sprintf("#%2.2x%2.2x%2.2x",iclr[[1]],iclr[[2]],iclr[[3]])
  return(hclr)
}

plotWholeThing <- function(partAttList,partVertList,compList) {
  for (cp in compList) {

    compname <- cp$compname
    partname <- gsub("\\-\\d$","",compname)
    print(sprintf("Comp:%s part:%s",compname,partname))
    rot <- cp$rot
    trn <- cp$trn

    prta <- partAttList[[partname]]

    amb <- colVekToStringColor(prta$ambient)
    dif <- colVekToStringColor(prta$diffuse)
    spc <- colVekToStringColor(prta$specular)
    ems <- colVekToStringColor(prta$emissive)
    alf <- prta$ambient[4]
    shn <- prta$shinyness
    #print(prta$ambient)

    vtl <- partVertList[[partname]]
    newcp <- plotPartAsMesh(compname,partname,vtl,cp,trn,rot,amb,dif,spc,ems,alf,shn,hashemup = T)
    newcp$partid <- prta$id
    compList[[compname]] <- newcp
  }
  addAxes(len = 50)
  axes3d()
  return(compList)
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
      cp$rot <- round(rot,4)
      ntrn <- xml_find_first(it,".//*[local-name()='Translation']")
      # no idea where this factor of 1000 comes from (mm -> meters?)
      # some STL brain damage no doubt
      trn <- 1000.0*as.numeric(str_split(xml_text(ntrn),"\\s")[[1]])
      cp$trn <- trn
      cp$id <- 100*(length(compList)+1)
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
      nod <- xml_find_first(it,".//*[local-name()='Shininess']")
      prt$shinyness <- as.numeric(xml_text(nod))
      print(sprintf("   amb - %.3f",prt$ambient))
      print(sprintf("   dif - %.3f",prt$diffuse))
      print(sprintf("   spc - %.3f",prt$specular))
      print(sprintf("   emi - %.3f",prt$emissive))
      print(sprintf("   shn - %.3f",prt$shinyness))
      prt$id <- 100*(length(partAttList)+1)
      partAttList[[partname]] <- prt
    }
  }
  return(partAttList)
}

fixupname <- function(oname) {
  name <- oname
  name <- gsub("motor mount-1","motor mount-1",name)
  name <- gsub("motor mount-3","motor mount-2",name)
  name <- gsub("motor mount-4","motor mount-3",name)
  name <- gsub("motor mount-5","motor mount-4",name)

  name <- gsub("motor-1","motor---2",name)
  name <- gsub("motor-3","motor---1",name)
  name <- gsub("motor-2","motor---4",name)
  name <- gsub("motor-4","motor---3",name)
  name <- gsub("---","-",name)

  return(name)
}

writeOutCzfFiles <- function(fnameroot = "crazyflie",partAttList,partVertList,compList) {

  # Components
  cdf <- NULL
  for (c in compList) {
    vs <- round(c$out_sca,5)
    vt <- round(c$out_trn,5)
    mr <- round(c$out_rot,5)
    if (c$id == 1500) {
      # pcb not behaving like the rest. No idea why.
      mr <- matrix(c(1,0,0,0,1,0,0,0,1),3,3)
    }
    if (str_detect(c$compname,"propeller")) { 
      layers <- "cf//staticprop"
    } else if (str_detect(c$compname,"motor mount")) {
      layers <- "cf//base//momount"
    } else if (str_detect(c$compname,"motor")) {
      layers <- "cf//base//motor"
    } else if (str_detect(c$compname,"pcb")) {
      layers <- "cf//base//pcb"
    } else if (str_detect(c$compname,"battery")) {
      layers <- "cf//base//battery"
    } else if (str_detect(c$compname,"header")) {
      layers <- "cf//base//header"
    } else  {
     layers <- "cf//base"
    }
    options=""
    fixcompname <- fixupname(c$compname)
    c1df <- data.frame(id = c$id,compname = fixcompname,
                       partid = c$partid,partname = c$partname,
                       layers=layers,
                       options=options,
                       sca.x = vs[1],sca.y = vs[2],sca.z = vs[3],
                       trn.x = vt[1],trn.y = vt[2],trn.z = vt[3],
                       rot.11 = mr[1,1],rot.12 = mr[1,2],rot.13 = mr[1,3],
                       rot.21 = mr[2,1],rot.22 = mr[2,2],rot.23 = mr[2,3],
                       rot.31 = mr[3,1],rot.32 = mr[3,2],rot.33 = mr[3,3]
                       )
    cdf <- rbind(cdf,c1df)
  }
  cdf <- cdf[order(as.character(cdf$compname)),]
  fname <- sprintf("%s-components.csv",fnameroot)
  write.csv(cdf,fname,row.names = F)

  # Parts
  pdf <- NULL
  for (p in partAttList) {
    p1df <- data.frame(partid = p$id,partname=p$partname,
      amb.r = p$ambient[[1]],amb.g = p$ambient[[2]],amb.b = p$ambient[[3]],amb.a = p$ambient[[4]],
      dif.r = p$diffuse[[1]],dif.g = p$diffuse[[2]],dif.b = p$diffuse[[3]],dif.a = p$diffuse[[4]],
      spc.r = p$specular[[1]],spc.g = p$specular[[2]],spc.b = p$specular[[3]],spc.a = p$specular[[4]],
      ems.r = p$emissive[[1]],ems.g = p$emissive[[2]],ems.b = p$emissive[[3]],ems.a = p$emissive[[4]],
      shinyness=p$shinyness
      )
    pdf <- rbind(pdf,p1df)
  }
  fname <- sprintf("%s-parts.csv",fnameroot)
  write.csv(pdf,fname,row.names = F)

  # Points
  ptdf <- NULL
  for (c in compList) {
    pt1df <- as.data.frame(t(round(c$out_vp,5)))
    names(pt1df) <- c("x","y","z","w")
    pt1df$partid <- c$partid

    # only interested in writing the first component for every part
    fcidx <- which(cdf$partname == c$partname)[[1]]
    fcid <- cdf$id[fcidx]
    if (fcid == c$id) { 
      ptdf <- rbind(ptdf,pt1df)
    }
  }
  fname <- sprintf("%s-points.csv",fnameroot)
  write.csv(ptdf,fname,row.names = F)

  # VertsIdx
  ptdf <- NULL
  for (c in compList) {
    pt1df <- as.data.frame(t(c$out_vi))
    names(pt1df) <- c("v1","v2","v3")
    #pt1df$id <- c$id
    pt1df$partid <- c$partid

    # only write out the first ones
    fcidx <- which(cdf$partname == c$partname)[[1]]
    fcid <- cdf$id[fcidx]
    if (fcid == c$id) {
      ptdf <- rbind(ptdf,pt1df)
    }
  }
  fname <- sprintf("%s-vertidx.csv",fnameroot)
  write.csv(ptdf,fname,row.names = F)
}

# Start of actual program
# =======================
starttime <- Sys.time()

stldir <- "../Crazyflie-CAD/STL"

# Get the STL files with the vertices and read them in
stlfiles <- list.files(stldir,"\\.STL$")
partVertList <- list()
for (fname in stlfiles) {
  partVertList <- readCzfVertsFromStl(stldir,fname,partVertList)
}

# Now read the composistion file which has the part instances, 
#  their transformations which position them, and their material properties
# 
compList <- readCompositionFromXml(stldir,"Crazyflie_assembly.xml")
compList <- compList[order(sapply(compList,'[[',"compname"))]  # order them

partAttList <- readMaterialsFromXml(stldir,"Crazyflie_assembly.xml")
partAttList <- partAttList[order(sapply(partAttList,'[[',"partname"))] # order them

compList <- plotWholeThing(partAttList,partVertList,compList)

writeOutCzfFiles("crazyflie",partAttList,partVertList,compList)

elap <- as.numeric((Sys.time() - starttime)[1], units = "secs")
print(sprintf("Run took %.1f secs for %d verts - verts optimized away:%d", elap,nvtot,nvsaved))