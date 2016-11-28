library(rgl)
library(stringr)
library(xml2)

stldir <- "../Crazyflie-CAD/STL"

stlfiles <- list.files(stldir,"\\.STL$")

append <- function(vlist, v) {
  if (!is.null(v)) {
    vlist[[length(vlist) + 1]] <- v
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
        partlist <- append(partlist, facet)
      }
      facet <- list()
      facet <- append(facet, extractVert(line))
    } else if (str_detect(line, "vertex")) {
      facet <- append(facet, extractVert(line))
    } else {
      print(sprintf("opps unknown linetype:%s",line))
    }
  }
  partlist <- append(partlist, facet)
  elap <- as.numeric((Sys.time() - starttime)[1], units = "secs")
  print(sprintf("Extracted %d vertices in %.1f secs for part '%s'", length(partlist),elap,partname))
  return(partlist)
}




plotPart <- function(vertList, trn = c(0,0,0), rot=NULL, color="silver",alpha=1,shiny=shiny) {
  lx <- list()
  ly <- list()
  lz <- list()
  xi <- 1
  yi <- 2
  zi <- 3
  for (v in vertList) {
    if (length(v) == 4) {
      lx <- append(lx, v[[2]][xi] )
      lx <- append(lx, v[[3]][xi] )
      lx <- append(lx, v[[4]][xi])

      ly <- append(ly, v[[2]][yi] )
      ly <- append(ly, v[[3]][yi] )
      ly <- append(ly, v[[4]][yi])

      lz <- append(lz, v[[2]][zi] )
      lz <- append(lz, v[[3]][zi] )
      lz <- append(lz, v[[4]][zi] )
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
#    vx <- ux*rot[1,1] + uy*rot[1,2] + uz*rot[1,3]
#    vy <- ux*rot[2,1] + uy*rot[2,2] + uz*rot[2,3]
#    vz <- ux*rot[3,1] + uy*rot[3,2] + uz*rot[3,3]
  }
  #print("translating")
  vx <- vx + trn[[xi]]
  vy <- vy + trn[[yi]]
  vz <- vz + trn[[zi]]

  print(sprintf("trn-q: %.3f %.3f %.3f ", trn[[xi]], trn[[yi]], trn[[zi]]))

  triangles3d( vx,vy,vz, color=color,alpha=alpha,shiny=shiny)
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
colors <- c("white", "gray", "gray", "green", "yellow", "darkgray", "black", "orange")
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

#readxml <- F
#if (readxml) {
  #xmlfile <- sprintf("%s/%s", stldir, "Crazyflie_assembly.xml")
  #doc <- read_xml(xmlfile)
  #itree <- xml_find_first(doc, "//*[local-name()='InstanceTree']")
  #its <- xml_find_all(doc, "//*[local-name()='Instance']")
  #for (it in its) {
    ##print(as.character(it))
    #print(xml_attr(it, "name"))
    #tform <- xml_find_first(it, "//*[local-name()='Transform']")
    #if (length(tform) > 0) {
      #rot <- xml_find_first(it, "//*[local-name()='Rotation']")
      #print(sprintf("   %s", as.character(rot)))
      #print(xml_text(rot))
      #trn <- xml_find_first(it, "//*[local-name()='Translation']")
      ##print(sprintf("   %s", as.character(trn)))
      #print(xml_text(trn))
    #}
  #}
#}



plotWholeThing <- function(partList,doc) {
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
      #print(sprintf("   %s", as.character(nrot)))
      print(xml_text(nrot))
      rot <- matrix(as.numeric(str_split(xml_text(nrot), "\\s")[[1]]),3,3)
      print(rot)

      ntrn <- xml_find_first(it, ".//*[local-name()='Translation']")
      #print(sprintf("   %s", as.character(trn)))
      print(xml_text(ntrn))
      trn <- 1000*as.numeric(str_split(xml_text(ntrn), "\\s")[[1]])

      print(trn)

      prt <- getElement(partList,needpart)
      clr <- attr(prt, "color")
      alf <- attr(prt, "alpha")
      shn <- attr(prt, "shine")
      plotPart(prt, trn, rot, clr, alf, shn)
      print(" ")

    }
  }
  addAxes(len = 50)
}

xmlfile <- sprintf("%s/%s", stldir, "Crazyflie_assembly.xml")
doc <- read_xml(xmlfile)

plotWholeThing(partList,doc)
#d <- read_xml("simple1.xml")
#bstree <- xml_find_all(d, ".//bs:stmt") # Transaction Type
