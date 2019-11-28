#' Create raster
#'
#' Create raster of equaly spaced rectangles or hexagons. The distance between centre points
#' in both x and y dimension is equal to \code{cell_size}.
#'
#' @param geometry \code{\link[sf]{sf}} \code{data.frame} containing geometry which should be cover by
#' the grid.
#' @param cell_size \code{numeric} specifing the distance for equal spaced points or cells.
#' @param side_offset \code{numeric} specifing the side offset, distance added to bounding box
#' to generate points for KDE. Good estimate is usually the same value as band width of KDE.
#' @param only_inside \code{logical} specifing if the points should be generated only inside of the
#' geometry. Only valid for \code{"POLYGON"} or \code{"MULTIPOLYGON"} types. Default value is
#' \code{FALSE}.
#'
#' @return \code{\link[sf]{sf}} \code{data.frame}.
#' @export
#'
#' @describeIn create_raster Create rectangular raster
create_raster_rectangular <- function(geometry, cell_size, side_offset = 0, only_inside = FALSE){

  .create_raster(geometry, cell_size, side_offset, only_inside, TRUE)
}

#' @export
#'
#' @describeIn create_raster Create hexagonal raster
create_raster_hexagonal <- function(geometry, cell_size, side_offset = 0, only_inside = FALSE){

  .create_raster(geometry, cell_size, side_offset, only_inside, FALSE)
}

#' @importFrom sf st_geometry st_union st_convex_hull st_buffer st_make_grid
#' @importFrom sf st_sf st_intersection
#' @importFrom rlang .data
.create_raster <- function(geometry,
                           cell_size,
                           side_offset = 0,
                           only_inside = FALSE,
                           square = TRUE) {

  .validate_sf(geometry)

  .validate_sideoffset(side_offset)

  .validate_cellsize(cell_size)

  if (!(typeof(only_inside) == "logical")) {
    stop(glue::glue(
      "Parameter `only_inside` must be \"logical\". Currently it is of type: {typeof(only_inside)}."
    ))
  }

  buff_convex_hull <- geometry %>%
    sf::st_geometry() %>%
    sf::st_union() %>%
    sf::st_convex_hull() %>%
    sf::st_buffer(side_offset)

  grid <- buff_convex_hull %>%
    sf::st_make_grid(cellsize = cell_size,
                     what = "polygons",
                     square = square) %>%
    sf::st_sf()

  if (only_inside){

    if (.is_polygon(geometry)){
      suppressWarnings(
        grid <- grid %>%
          sf::st_intersection(geometry)
      )
    } else {
      warning(glue::glue("`Geometry` does not contain only `POLYGON` or `MULTIPOLYGON` geometries.",
                         "Cannot clip the outcome to the `geometry` by `only_insider` parameter."))
    }
  }

  grid
}
