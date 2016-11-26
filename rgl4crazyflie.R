library(rgl)

stldir <- "../Crazyflie-CAD/STL"

stlfiles <- list.files(stldir,"\\.STL$")

loadStl <- function(stlfname) {
  lines <- readLines(con <- file(stlfname))
}