.onUnload <- function(libpath) {
  library.dynam.unload("SpatialKDE", libpath)
}
