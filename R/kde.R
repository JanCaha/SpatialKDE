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
#' @param band_width \code{numeric} specifying the band width for KDE.
#' @param decay \code{numeric} specifying the decay parameter for \code{"triangular"} kernel. For
#' other kernels besides \code{"triangular"} the parameter is not used.
#' @param kernel \code{character} specifying type of kernel to use. Available implemented kernels are
#' \code{"uniform", "quartic", "triweight", "epanechnikov", "triangular"}. Default is \code{"quartic"} and if
#' unknown kernel name is used it falls back to the default value.
#' @param scaled \code{logical} specifying if the output values should be scaled. Default value is
#' \code{FALSE}.
#' @param weights \code{numeric} vector of weights for individual \code{points}.
#' @param grid  either \code{\link[sf]{sf}} \code{data.frame} (outcome of function
#' \code{\link{create_grid_rectangular}} or \code{\link{create_grid_hexagonal}}) or
#' \code{\link[raster]{Raster-class}} (outcome of function \code{\link{create_raster}}).
#' Does not have to be specified if \code{cell_size} is set.
#' @param cell_size \code{numeric} specifying the distance for equal spaced points. Must be
#' higher than 0. Can be left out if \code{grid} is provided as \code{grid} is used instead.
#' The code used to generate grid is \code{\link{create_grid_rectangular}(points, cell_size, band_width)}.
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
#' @importFrom rlang .data is_double is_integer
#' @importFrom vctrs vec_cast
#'
#' @useDynLib SpatialKDE
#'
#' @examples
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package="sf")) %>% st_transform(32031)
#' grid <- create_grid_hexagonal(nc, cell_size = 100000)
#' points <- st_sample(nc, 500) %>% st_as_sf()
#' kde_estimate_grid <- kde(points, band_width = 150000, grid = grid)
#' raster <- create_raster(nc, cell_size = 100000)
#' kde_estimate_raster <- kde(points, band_width = 150000, grid = raster)
#'
kde <- function(points,
                band_width,
                decay = 1,
                kernel = c("quartic", "uniform", "triweight", "epanechnikov", "triangular"),
                scaled = FALSE,
                weights = c(),
                grid,
                cell_size){

  kernel <- match.arg(kernel)

  .validate_sf(points)

  .validate_projected(points)

  .validate_points(points)

  if (length(weights) == 0) {
    weights = rep(1, nrow(points))
  }

  if (is_integer(weights, n = nrow(points))) {
    weights <- vctrs::vec_cast(weights, double())
  }

  if (!is_double(weights, n = nrow(points), finite = TRUE)) {
    stop(glue::glue("All values of `weights` must be numerical and finite vector (no `NA`s, `Inf` or `-Inf`).",
                    "The length of the vector must be equal to number of rows in `points`.",
                    "Length weights is `{length(weights)}` and number of rows in points is `{nrow(points)}`."))
  }

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

  kde_calculated <- .kde(grid = grid,
                         points = points,
                         band_width = band_width,
                         decay = decay,
                         kernel = kernel,
                         scaled = scaled,
                         weights = weights)

  return(kde_calculated)
}


.kde <- function(grid,
                 points,
                 band_width,
                 decay,
                 kernel,
                 scaled,
                 weights) {
  UseMethod(".kde")
}

#' @importFrom sf st_centroid st_coordinates st_crs st_transform
#' @importFrom dplyr mutate
#' @importFrom glue glue
.kde.sf <- function(grid,
                    points,
                    band_width,
                    decay,
                    kernel,
                    scaled,
                    weights) {

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

  if (sf::st_crs(points) != sf::st_crs(grid_points)) {
    message(glue::glue("`points` are transformed into `grid` CRS to make coordinates match. ",
                       "You may want to do this manually using `st_transform()` function to ",
                       "have better control over the process."))
    points <- points %>%
      sf::st_transform(crs = st_crs(grid_points))
  }

  kde_values <- kde_estimate(sf::st_coordinates(grid_points),
                             sf::st_coordinates(points),
                             bw = band_width,
                             kernel = kernel,
                             scaled = scaled,
                             decay = decay,
                             weights = weights)

  grid <- grid %>%
    dplyr::mutate(kde_value = kde_values)

  grid
}

#' @importFrom raster xyFromCell values
#' @importFrom sf st_coordinates
#' @importFrom methods setMethod
setMethod(".kde",
          "RasterLayer",
          function(grid,
                   points,
                   band_width,
                   decay,
                   kernel,
                   scaled,
                   weights) {

            .validate_raster_projected(grid)

            cells_number <- length(grid)

            .warn_long_calculation(cells_number)

            kde_values <- kde_estimate(raster::xyFromCell(grid, 1:cells_number),
                                       sf::st_coordinates(points),
                                       bw = band_width,
                                       kernel = kernel,
                                       scaled = scaled,
                                       decay = decay,
                                       weights = weights)

            raster::values(grid) <- kde_values

            grid
          }
)
