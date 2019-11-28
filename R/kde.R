#' Kernel Density Estimation
#'
#' KDE for spatial data. The algorithm is heavily inspired by
#' \href{https://github.com/qgis/QGIS/blob/b3d2619976a69d7fb67b884492da491dfaba287c/src/analysis/raster/qgskde.cpp}{Heatmap tool}
#' in QGIS. The tutorial is provided \href{https://grindgis.com/software/heat-map-using-qgis}{here}.
#'
#' @param points \code{\link[sf]{sf}} \code{data.frame} containing only POINTS.
#' @param band_width \code{numeric} specifing the band width for KDE.
#' @param cell_size \code{numeric} specifing the distance for equal spaced points or cells. Must be
#' higher than 0. Can be left out if \code{grid} is provided.
#' @param kernel \code{character} specifing type of kernel to use. Available implemented kernels are
#' \code{"uniform", "quartic", "triweight", "epanechnikov"}. Default is \code{"quartic"} and if
#' uknown kernel name is used it falls back to the default value.
#' @param scaled \code{logical} specifing if the output values should be scaled. Default value is
#' \code{FALSE}.
#' @param grid \code{\link[sf]{sf}} \code{data.frame} outcome of function
#' \code{\link{create_raster_rectangular}} or \code{\link{create_raster_hexagonal}}.
#'
#' @return  \code{\link[sf]{sf}} \code{data.frame}.
#' @export
#'
#' @importFrom sf st_is_longlat st_bbox st_geometry st_union st_convex_hull st_buffer st_make_grid
#' @importFrom sf st_sf st_coordinates st_geometry_type st_centroid
#' @importFrom dplyr mutate
#' @importFrom glue glue glue_collapse
#' @importFrom Rcpp evalCpp
#' @importFrom rlang .data
#'
#' @useDynLib SpatialKDE
#'
kde <- function(points,
                band_width,
                cell_size,
                kernel = "quartic",
                scaled = FALSE,
                grid){

  available_kernels = c("uniform", "quartic", "triweight", "epanechnikov")

  .validate_sf(points)

  .validate_projected(points)

  .validate_points(points)

  .validate_sf(grid)

  .validate_projected(grid)

  .validate_bandwidth(band_width)

  if (!missing(cell_size)) {
    .validate_cellsize(cell_size)
  }

  if (!(kernel %in% available_kernels)){
    warning(glue::glue("Unknown `kernel` used. The implemented kernels are: ",
                       glue::glue_collapse(available_kernels, sep = ", "), ". ",
                       "Using `quartic` kernel as default."))
  } else {
    kernel = available_kernels[2]
  }

  if (missing(grid)) {
    grid <- create_raster_rectangular(points, cell_size, band_width)
  }

  cells_number <- nrow(grid)

  if (cells_number > 50000) {
    message(glue::glue("The number of cells (points) in the outcomes is large (`{cells_number}`). ",
                       "The calculation may take a while. To speed it up you can use larger `cell_size`."))
  }

  if (all(unique(st_geometry_type(grid)) == "POINT")) {
    grid_points <- grid
  } else {
    message("Using centroids instead of provided geometry to calculate KDE.")
    suppressWarnings(
      grid_points <- grid %>%
        st_centroid(of_largest_polygon = TRUE)
    )
  }

  kde_values <- kde_estimate(sf::st_coordinates(grid_points),
                             sf::st_coordinates(points),
                             bw = band_width,
                             kernel = kernel,
                             scaled = scaled)

  grid <- grid %>%
    dplyr::mutate(kde_value = kde_values)

  return(grid)
}
