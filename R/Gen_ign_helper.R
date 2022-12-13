Gen_ign_helper <- function(x, n, replace = TRUE, prob = TRUE) {
  val <- as.vector(x[[1]])
  probs <-  val

  cellNum <- 1:raster::ncell(x)
  cellNum <- cellNum[!is.na(val)]

  probs <- probs[!is.na(val)]

  sites <- sample(cellNum, size=n, replace=replace, prob=probs)
  xy <- raster::xyFromCell(x, sites)

  xy

}
