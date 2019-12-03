#' Create grid
#'
#' Create grid of equaly spaced rectangles or hexagons. The distance between centre points
#' in both x and y dimension is equal to \code{cell_size}. The function is effectively a wrapper around
#' \code{\link[sf]{st_make_grid}} with a little bit of preprocessing including generation of grid only inside
#' \code{\link[sf]{st_convex_hull}}.
#'
#' @param geometry \code{\link[sf]{sf}} \code{data.frame} containing geometry which should be cover by
#' the grid.
#' @param cell_size \code{numeric} specifing the distance for equal spaced points or cells.
#' @param side_offset \code{numeric} specifing the side offset, distance added to the convex hull
#' to generate grid for KDE. Good estimate is usually the same value as band width of KDE.
#' @param only_inside \code{logical} specifing if the grid cells should be generated only inside of the
#' geometry. Default value is \code{FALSE}.
#'
#' @return \code{\link[sf]{sf}} \code{data.frame}.
#' @export
#'
#' @describeIn create_grid Create rectangular grid
create_grid_rectangular <- function(geometry, cell_size, side_offset = 0, only_inside = FALSE){

  .create_grid(geometry, cell_size, side_offset, only_inside, square = TRUE)
}

#' @export
#'
#' @describeIn create_grid Create hexagonal grid
create_grid_hexagonal <- function(geometry, cell_size, side_offset = 0, only_inside = FALSE){

  .create_grid(geometry, cell_size, side_offset, only_inside, square = FALSE)
}

#' @importFrom sf st_geometry st_union st_convex_hull st_buffer st_make_grid
#' @importFrom sf st_sf st_intersects st_covered_by
#' @importFrom rlang .data
#' @importFrom dplyr mutate filter select
.create_grid <- function(geometry,
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

  if (only_inside) {

    grid <- grid %>%
      dplyr::mutate(covered = as.numeric(sf::st_covered_by(grid, buff_convex_hull))) %>%
      dplyr::filter(!is.na(.data$covered))

  } else {

    grid <- grid %>%
      dplyr::mutate(intersect = as.numeric(sf::st_intersects(grid, buff_convex_hull))) %>%
      dplyr::filter(!is.na(.data$intersect))
  }

  grid %>%
    dplyr::select()
}
