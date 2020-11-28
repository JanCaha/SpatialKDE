#' Create raster
#'
#' Create raster of equally spaced cells. The distance between centre of cells
#' in both x and y dimension is equal to \code{cell_size}.
#'
#' @param geometry \code{\link[sf]{sf}} \code{data.frame} containing geometry which should be cover by
#' the raster.
#' @param cell_size \code{numeric} specifying the distance for equally spaced cells.
#' @param side_offset \code{numeric} specifying the side offset, distance added to the convex hull
#' of input geometry to generate raster for KDE. Good estimate is usually the same value as band width of KDE.
#'
#' @return \code{\link[raster]{Raster-class}}
#' @export
#'
#' @importFrom sf st_convex_hull st_buffer st_geometry st_union st_as_sf
#' @importFrom raster raster
#'
#' @examples
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package="sf")) %>% st_transform(32031)
#' raster <- create_raster(nc, cell_size = 100000)
#'
create_raster <- function(geometry, cell_size, side_offset = 0){

  .validate_sf(geometry)

  .validate_sideoffset(side_offset)

  .validate_cellsize(cell_size)

  buffered_geometry <- geometry %>%
    sf::st_geometry() %>%
    sf::st_union() %>%
    sf::st_convex_hull() %>%
    sf::st_buffer(side_offset) %>%
    sf::st_as_sf()

  raster <- raster::raster(buffered_geometry,
                           resolution = c(cell_size, cell_size))

  raster
}
