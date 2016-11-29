open3d()

addVert <- function(mList,v) {
  ni <- length(mList$idxList)
  np <- length(mList$pntList)
  mList$pntList[[np + 1]] <- v[1]
  mList$pntList[[np + 2]] <- v[2]
  mList$pntList[[np + 3]] <- v[3]
  mList$idxList[[ni + 1]] <- ni + 1
  return(mList)
}

addTri <- function(mList,v1,v2,v3) {
  mList <- addVert(mList,v1)
  mList <- addVert(mList,v2)
  mList <- addVert(mList,v3)
  return(mList)
}

mList <- list()
mList$idxList <- list()
mList$pntList <- list()

v1 <- c(1,0,0)
v2 <- c(0,1,0)
v3 <- c(0,0,1)
mList <- addTri(mList,v1,v2,v3)
vidx <- unlist(mList$idxList)
vpnt <- unlist(mList$pntList)
part <- tmesh3d(vpnt,vidx)
wire3d(part)
