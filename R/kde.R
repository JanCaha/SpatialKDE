#' Kernel Density Estimation
#'
#' KDE for spatial data. The algorithm is heavily inspired by
#' \href{https://github.com/qgis/QGIS/blob/b3d2619976a69d7fb67b884492da491dfaba287c/src/analysis/raster/qgskde.cpp}{Heatmap tool}
#' in QGIS. The help for QGIS tools is provided \href{https://docs.qgis.org/testing/en/docs/user_manual/processing_algs/qgis/interpolation.html#heatmap-kernel-density-estimation}{at the QGIS website}.
#' The a tutorial is provided \href{https://grindgis.com/software/heat-map-using-qgis}{here}.
#'
#' @details
#' \code{grid} parameter specifies output of the function. KDE is calculated on the specified \code{grid}.
#' If grid is \code{\link[raster]{Raster-class}} then outcome is also \code{\link[raster]{Raster-class}}.
#' If grid is \code{\link[sf]{sf}} \code{data.frame} then outcome is also \code{\link[sf]{sf}} \code{data.frame}.
#'
#'
#' @param points \code{\link[sf]{sf}} \code{data.frame} containing only POINTS.
#' @param band_width \code{numeric} specifing the band width for KDE.
#' @param cell_size \code{numeric} specifing the distance for equal spaced points or cells. Must be
#' higher than 0. Can be left out if \code{grid} is provided.
#' @param decay \code{numeric} specifing the decay parameter for \code{"triangular"} kernel. For
#' other kernels besides \code{"triangular"} the parameter is not used.
#' @param kernel \code{character} specifing type of kernel to use. Available implemented kernels are
#' \code{"uniform", "quartic", "triweight", "epanechnikov", "triangular"}. Default is \code{"quartic"} and if
#' uknown kernel name is used it falls back to the default value.
#' @param scaled \code{logical} specifing if the output values should be scaled. Default value is
#' \code{FALSE}.
#' @param grid  either \code{\link[sf]{sf}} \code{data.frame} (outcome of function
#' \code{\link{create_grid_rectangular}} or \code{\link{create_grid_hexagonal}}) or
#' \code{\link[raster]{Raster-class}} (outcome of function \code{\link{create_raster}}).
#'
#' @return  either \code{\link[sf]{sf}} \code{data.frame} or \code{\link[raster]{Raster-class}}
#' depending on class of \code{grid} parameter.
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
                decay = 1,
                kernel = "quartic",
                scaled = FALSE,
                grid){

  available_kernels = c("uniform", "quartic", "triweight", "epanechnikov", "triangular")

  .validate_sf(points)

  .validate_projected(points)

  .validate_points(points)

  if (missing(grid) & missing(cell_size)) {
    stop("Both variables `grid` and `cellsize` are not specified. Don't know how to create grid for KDE estimation.")
  }

  if (!missing(cell_size)) {
    .validate_cellsize(cell_size)
  }

  if (missing(grid)) {

    grid <- create_grid_rectangular(points, cell_size, band_width)

  }

  .validate_bandwidth(band_width)


  if (!(kernel %in% available_kernels)) {
    warning(glue::glue("Unknown `kernel` used. The implemented kernels are: ",
                       glue::glue_collapse(available_kernels, sep = ", "), ". ",
                       "Using `quartic` kernel as default."))
    kernel = available_kernels[2]
  }

  kde_calculated <- .kde(grid = grid,
                         points = points,
                         band_width = band_width,
                         decay = decay,
                         kernel = kernel,
                         scaled = scaled)

  return(kde_calculated)
}


.kde <- function(grid,
                 points,
                 band_width,
                 decay,
                 kernel,
                 scaled) {
  UseMethod(".kde")
}

#' @importFrom sf st_centroid st_coordinates
#' @importFrom dplyr mutate
.kde.sf <- function(grid,
                    points,
                    band_width,
                    decay,
                    kernel,
                    scaled) {

  .validate_sf(grid)

  .validate_projected(grid)

  cells_number <- nrow(grid)

  .warn_long_calculation(cells_number)

  if (all(unique(st_geometry_type(grid)) == "POINT")) {
    grid_points <- grid
  } else {
    message("Using centroids instead of provided `grid` geometries to calculate KDE estimates.")
    suppressWarnings(
      grid_points <- grid %>%
        sf::st_centroid(of_largest_polygon = TRUE)
    )
  }

  kde_values <- kde_estimate(sf::st_coordinates(grid_points),
                             sf::st_coordinates(points),
                             bw = band_width,
                             kernel = kernel,
                             scaled = scaled,
                             decay = decay)

  grid <- grid %>%
    dplyr::mutate(kde_value = kde_values)

  grid
}

#' @importFrom raster xyFromCell values
#' @importFrom sf st_coordinates
setMethod(".kde",
          "RasterLayer",
          function(grid,
                   points,
                   band_width,
                   decay,
                   kernel,
                   scaled) {

            .validate_raster_projected(grid)

            cells_number <- length(grid)

            .warn_long_calculation(cells_number)

            kde_values <- kde_estimate(raster::xyFromCell(grid, 1:cells_number),
                                       sf::st_coordinates(points),
                                       bw = band_width,
                                       kernel = kernel,
                                       scaled = scaled,
                                       decay = decay)

            raster::values(grid) <- kde_values

            grid
          }
)
